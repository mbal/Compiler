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

data CState = CState {
                constantId :: Word16
                , variableId :: Word16
                , constants :: [PyType]
                , variables :: [String]
                , varLabels :: Map.Map String Integer
                , instructions :: [Instruction]
                } deriving (Show)

initState = CState 0 0 [] [] Map.empty []

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
  cId <- gets constantId
  modify $ \s -> s { constantId = cId + 1 }
  return cId

freshVariableId = do
  vId <- gets variableId
  modify $ \s -> s { variableId = vId + 1 }
  return vId

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

createVariable varName =
  do s <- gets variables
     getVariableId varName (indexOf varName (reverse s))

getVariableId name Nothing =
  do vId <- freshVariableId
     oldVariables <- gets variables
     modify $ \s -> s { variables = name : oldVariables }
     return vId
getVariableId _ (Just x) = do return $ fromInteger x
          
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
  s <- gets variables
  let vId = indexOf k (reverse s)
  case vId of
    Nothing -> error $ "Variable " ++ k ++ " not defined"
    Just x -> emitCodeArg LOAD_NAME $ fromIntegral x

{-- XXX: assume that every function is a print --}
compile (FunApp fname args) = do
  mapM compile args
  emitCodeNoArg PRINT_ITEM -- 333: Py3k incompatibily, print is a function
  emitCodeArg LOAD_CONST 0
