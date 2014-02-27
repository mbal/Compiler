{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler2 where
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative hiding ((<|>), many)
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
                , constants :: [PyType]
                , varLabels :: Map.Map String Integer
                , instructions :: [Instruction]
                } deriving (Show)

initState = CState 0 [] Map.empty []

{- An instruction in the Python Bytecode is:
  * 1 Opcode (1 byte)
  * 1 argument, of 2 bytes
Only instructions with opcode > 90 have an argument -}
data Instruction = Instruction OpCode (Maybe Word16)
                   deriving (Show)

newtype CompilerState a = CompilerState { runCompilerState :: State CState a }
    deriving (Functor, Applicative, Monad, MonadState CState)

test = putStrLn $ unlines $ map show $ reverse $ instructions $ execState (runCompilerState $ compile $ parseAST "5+3") initState

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

createConstant :: PyType -> CompilerState Word16
createConstant obj = do
  cId <- freshConstantId
  oldConstants <- gets constants
  modify $ \s -> s { constants = obj : oldConstants }
  return cId

data PyType = PyInt { intvalue :: Integer }
            | PyString { string :: String }
            | PyTuple { elements :: [PyType] }
            | PyCode
            deriving (Show, Ord, Eq)

compile (Num a) = do
  cId <- createConstant (PyInt { intvalue = a })
  emitCodeArg LOAD_CONST cId

compile (Sum e1 e2) = do
  compile e1
  compile e2
  emitCodeNoArg BINARY_ADD
