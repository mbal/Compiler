{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

module Compiler where
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

type PutData = ErrorT String PutM ()

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

data PyType = PyInt Integer
            | PyTuple [PyType]
            deriving (Show)

compile (Num a) = do
  cId <- createConstant (PyInt a)
  emitCodeArg LOAD_CONST cId

compile (Sum e1 e2) = do
  compile e1
  compile e2
  emitCodeNoArg BINARY_ADD

encodeInstruction :: Instruction -> [Word8]
encodeInstruction (Instruction opcode arg) =
  case Map.lookup opcode opcodeToWord8 of
    Nothing -> error ("INVALID OPCODE: " ++ show opcode)
    Just w8 ->
      case arg of
        Nothing -> [w8]
        Just a ->
          let (w2, w3) = split a in [w8, w2, w3]

writeU8 = lift . putWord8
writeU16 = lift . putWord16be
writeU32 = lift . putWord32le

writeToFile handle pyc = do
  bytes <- runPutDataCheck $ putPycFile pyc
  return bytes
  B.hPutStr handle bytes

writeFile pyc path = do
  handle <- openFile path WriteMode
  writeToFile handle pyc
  hClose handle

runPutData :: PutData -> Either String B.ByteString
runPutData comp =
   case runPutM (runErrorT comp) of
      (Left err, _) -> Left err
      (Right (), bs) -> Right bs

runPutDataCheck :: PutData -> IO B.ByteString
runPutDataCheck comp =
   case runPutData comp of
      Left e -> fail e
      Right bs -> return bs

magic :: Word32
magic = 168686339

putPycFile pyc = do
  writeU32 $ magic
  writeU32 $ (0 :: Word32) -- modified time
  writePreamble
  writeCode pyc

writePreamble = do
  writeU8 $ fromIntegral $ ord 'c'
  writeU32 0
  writeU32 0
  writeU32 2
  writeU32 64

writeCode pyc =
  traceShow (map encodeInstruction (reverse (instructions pyc)))
  (do
  writeU8 $ fromIntegral $ ord 's' -- source Code
  writeU32 $ byteCount $ instructions pyc
  (mapM_ writeU8 (concat (map encodeInstruction (reverse (instructions pyc)))))
  writeConstants (constants pyc)
  writeLocals []
  writeLocals []
  writeLocals []
  writeLocals ["p.py", "<module>"])

writeTuple content = do
  writeU8 $ encodeType PyTuple
  writeU32 $ fromIntegral $ length content
  mapM_ writeContent content

writeLocals l = do
  writeU8 $ fromIntegral $ ord '('
  writeU32 $ fromIntegral $ length l
  mapM_ writeContent l

writeContent x = do
  writeU8 $ fromIntegral $ ord 's'
  writeU32 $ fromIntegral $ length x
  mapM_ writeU8 (map (fromIntegral . ord) x)

writeConstants cList =
  traceShow cList
  (do
  writeU8 $ fromIntegral $ ord '('
  writeU32 $ fromIntegral $ length cList
  mapM_ writeConst cList)

writeConst (PyInt a) = do
  writeU8 $ fromIntegral $ ord 'i'
  writeU32 $ fromIntegral a

byteCount :: [Instruction] -> Word32
byteCount x = 7
