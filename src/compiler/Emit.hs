module Emit where
import Data.Word (Word8, Word16, Word32)
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (ord)
import Debug.Trace
import qualified Data.ByteString.Lazy as B
   (ByteString, hGetContents, unpack, hPutStr, length)
import Control.Monad.Error (ErrorT (..), lift, replicateM)
import Data.Word (Word32, Word16, Word8)
import System.IO
import Data.Binary.Put
import qualified Data.Map as Map hiding (map)

import Compiler
import Parser
import StackSize
import Bytecode

type PutData = ErrorT String PutM ()

data Type = NULL
          | NONE
          | FALSE
          | TRUE
          | STOPITER
          | ELLIPSIS
          | INT
          | INT64
          | FLOAT
          | BINARY_FLOAT
          | COMPLEX
          | BINARY_COMPLEX
          | LONG
          | STRING
          | TUPLE
          | LIST
          | DICT
          | CODE
          | UNICODE
          | UNKNOWN
          | SET
          | FROZENSET
          deriving (Eq, Ord, Show)

objectTypeToChar :: Map.Map Type Char
objectTypeToChar = Map.fromList [ (y, x) | (x, y) <- objectTypeList ]

objectTypeList :: [(Char, Type)]
objectTypeList = [
   ('0', NULL),
   ('N', NONE),
   ('F', FALSE),
   ('T', TRUE),
   ('S', STOPITER),
   ('.', ELLIPSIS),
   ('i', INT),
   ('I', INT64),
   ('f', FLOAT),
   ('g', BINARY_FLOAT),
   ('x', COMPLEX),
   ('y', BINARY_COMPLEX),
   ('l', LONG),
   ('s', STRING),
   ('(', TUPLE),
   ('[', LIST),
   ('{', DICT),
   ('c', CODE),
   ('u', UNICODE),
   ('?', UNKNOWN),
   ('<', SET),
   ('>', FROZENSET) ]

encodeObjectType :: Type -> Word8
encodeObjectType objectType =
   case Map.lookup objectType objectTypeToChar of
      Nothing -> error $ "bad object type: " ++ show objectType
      Just chr -> fromIntegral $ ord chr 

encodeInstruction :: Instruction -> [Word8]
encodeInstruction (Instruction opcode arg) =
  case Map.lookup opcode opcodeToWord8 of
    Nothing -> error ("INVALID OPCODE: " ++ show opcode)
    Just w8 ->
      case arg of
        Nothing -> [w8]
        Just a ->
          let (w2, w3) = split a in [w8, w2, w3]

-- Needed to avoid -XNoMonomorphismRestriction
writeU8 :: Word8 -> ErrorT String PutM ()
writeU8 = lift . putWord8
writeU16 :: Word16 -> ErrorT String PutM ()
writeU16 = lift . putWord16be
writeU32 :: Word32 -> ErrorT String PutM ()
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
  writePreamble pyc
  writeCode pyc

writePreamble pyc =
  do writeU8 $ fromIntegral $ ord 'c'
     writeU32 0
     writeU32 0
     writeU32 $ fromIntegral $ computeStackSize (instructions pyc) -- stacksize
     writeU32 64 --options


writeCode codeObject =
  do
    writeInstructions $ reverse $ (Instruction RETURN_VALUE Nothing) : (instructions codeObject)
    writeConstants $ reverse (constants codeObject)
    writeTuple $ reverse (variables codeObject)
    writeTuple []
    writeTuple []
    writeTuple []
    writeString PyString { string = "code" }
    writeString PyString { string = "<module>" }
    writeU32 1 -- first line of code ??
    writeString PyString { string = "aa" } -- lnotab ??

writeInstructions ilist = do
  writeU8 $ encodeType STRING
  writeU32 $ computeInstructionSize ilist
  mapM_ writeU8 (concat (map encodeInstruction ilist))

writeString str = do
  writeU8 $ encodeType STRING
  writeU32 $ fromIntegral $ length (string str)
  mapM_ writeU8 (map (fromIntegral . ord) (string str))

writeInt int = do
  writeU8 $ encodeType INT
  writeU32 int
    
encodeType x = case Map.lookup x objectTypeToChar of
  Nothing -> error $ "NO SUCH TYPE "
  Just v -> fromIntegral $ ord v

writeTuple content = do
  writeU8 $ encodeType TUPLE
  writeU32 $ fromIntegral $ length content
  mapM_ writeContent content

writeContent x = do
  writeU8 $ fromIntegral $ ord 's'
  writeU32 $ fromIntegral $ length x
  mapM_ writeU8 (map (fromIntegral . ord) x)

writeConstants cList =
  do
    writeU8 $ fromIntegral $ ord '('
    writeU32 $ fromIntegral $ length cList
    mapM_ writeConst cList

writeConst (PyInt a) = do
  writeU8 $ encodeType INT
  writeU32 $ fromIntegral a 
  
computeInstructionSize ilist =
  sum $ map instructionSize ilist where 
    instructionSize (Instruction _ Nothing) = 1
    instructionSize (Instruction _ (Just _)) = 3


