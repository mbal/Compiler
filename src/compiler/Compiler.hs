{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative (Applicative)
import qualified Data.ByteString.Lazy as B
   (ByteString, hGetContents, unpack, hPutStr, length)
import Control.Monad.Error (ErrorT (..), lift, replicateM)
import Data.Word (Word32, Word16, Word8)
import System.IO
import Data.Binary.Put
import Debug.Trace
import Data.Char
import Parser
import Bytecode
import qualified Data.Map as Map hiding (map)

{-- ============= STATE ============= --}
data CState = CState {
                nextConstantId :: Word16
                , nextVariableId :: Word16
                , constants :: [PyType]
                , varNames :: Map.Map Name Word16
                , variables :: [String]
                , instructions :: [Instruction]
                } deriving (Show)

initState = CState {
  nextConstantId =  0
  , nextVariableId = 0
  , constants = []
  , varNames = Map.empty
  , variables = []
  , instructions = []
  }

{- An instruction in the Python Bytecode is:
  * 1 Opcode (1 byte)
  * 1 argument, of 2 bytes
Only instructions with opcode > 90 have an argument -}
data Instruction = Instruction OpCode (Maybe Word16)
                   deriving (Show)

newtype CompilerState a = CompilerState { runCompilerState :: State CState a }
    deriving (Functor, Applicative, Monad, MonadState CState)

-- emits opCode with the given argument
emitCodeArg :: OpCode -> Word16 -> CompilerState ()
emitCodeArg opCode arg = emitCode $ Instruction opCode (Just arg)

emitCodeNoArg :: OpCode -> CompilerState ()
emitCodeNoArg opCode = emitCode $ Instruction opCode Nothing

emitCode :: Instruction -> CompilerState ()
emitCode inst = do
  oldInstructions <- gets instructions
  modify $ \s -> s { instructions = inst : oldInstructions }

freshConstantId :: CompilerState Word16
freshConstantId = do
  cId <- gets nextConstantId
  modify $ \s -> s { nextConstantId = cId + 1 }
  return cId

createConstant :: PyType -> CompilerState Word16
createConstant obj =
  do s <- gets constants
     getConstantId obj (indexOf obj (reverse s))

getConstantId obj Nothing =
  do cId <- freshConstantId
     oldConstants <- gets constants
     modify $ \s -> s { constants = obj : oldConstants }
     return cId
getConstantId _ (Just x) = do return $ fromInteger x

freshVariableId :: CompilerState Word16
freshVariableId =
  do x <- gets nextVariableId
     modify $ \s -> s { nextVariableId = x + 1 }
     return x

createVariable varName =
  do vars <- gets varNames
     case Map.lookup varName vars of
       Nothing -> newVariable varName
       Just x -> return $ x

newVariable name =
  do vId <- freshVariableId
     vars <- gets varNames
     modify $ \s -> s { varNames = Map.insert name vId vars }
     return vId

getVariableId name Nothing =
  do vId <- freshVariableId
     oldVariables <- gets variables
     modify $ \s -> s { variables = name : oldVariables }
     return vId
getVariableId _ (Just x) = do return $ fromInteger x
          
{-- ============= STATE ============= --}

indexOf :: (Eq a) => a -> [a] -> Maybe Integer
indexOf y list = indexOf' y list 0
  where indexOf' _ [] _ = Nothing
        indexOf' y (x:xs) a = if x == y then (Just a) else indexOf' y xs (a+1)


data PyType = PyInt { intvalue :: Integer }
            | PyString { string :: String }
            | PyTuple { elements :: [PyType] }
            | PyCode
            deriving (Show, Ord, Eq)

compileTopLevel = mapM compile

compile (BinaryOp op e1 e2) =
  do compile e1
     compile e2
     emitCodeNoArg $ emitOp op
     where emitOp Add = BINARY_ADD
           emitOp Subtract = BINARY_SUBTRACT
           emitOp Multiply = BINARY_MULTIPLY
           emitOp Divide = BINARY_TRUE_DIVIDE
           emitOp _ = error "compile BinaryOp: not valid"

compile (Const a) = do
  cId <- createConstant (PyInt { intvalue = a })
  emitCodeArg LOAD_CONST cId

compile (Let k e) = do
  compile e
  vId <- createVariable k
  emitCodeArg STORE_NAME vId

compile (Var k) = do
  s <- gets varNames
  case Map.lookup k s of
    Nothing -> error $ "Variable " ++ k ++ " is not defined"
    Just x -> emitCodeArg LOAD_NAME $ fromIntegral x

{-- XXX: assume that every function is a print --}
compile (FunApp fname args) = do
  mapM compile args
  -- 333: Py3k incompatibily, print is a function, so it should be loaded and
  -- called like any other functions.
  emitCodeNoArg PRINT_ITEM
  emitCodeArg LOAD_CONST 0
