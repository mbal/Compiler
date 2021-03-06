{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module Emit where
import Data.Word (Word8, Word16, Word32)
import Control.Monad.Reader
import Data.Char (ord)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Lazy as B (ByteString, hPutStr)
import Control.Monad.Error (ErrorT (..))
import System.IO
import Data.Binary.Put
import qualified Data.Map as Map

import Types
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

encodeInstruction' :: AugInstruction -> [Word8]
encodeInstruction' (AugInstruction i _) =
  encodeInstruction i

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

writeToFile :: Handle -> CState -> IO ()
writeToFile handle pyc = do
  bytes <- runPutDataCheck $ putPycFile pyc
  _ <- return bytes
  B.hPutStr handle bytes

writeFile :: CState -> FilePath -> IO ()
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

putPycFile :: CState -> ErrorT String PutM ()
putPycFile pyc = do
  writeU32 $ (cMagic pyc)
  writeU32 $ (0 :: Word32) -- modified time
  writePreamble $ cBlock pyc
  writeCode $ cBlock pyc

writePreamble :: CodeBlock -> ErrorT String PutM ()
writePreamble pyc =
  do writeU8 $ fromIntegral $ ord 'c'
     writeU32 0
     writeU32 0
     writeU32 $ fromIntegral $ computeStackSize (block_instructions pyc)
     writeU32 64 --options

computeStackSize :: [AugInstruction] -> Integer
computeStackSize _ = 100

getNames :: Map.Map k a -> [k]
getNames namePosition = Map.keys namePosition

keysOrdered :: (Ord a) => Map.Map k a -> [k]
keysOrdered mp = map fst (sortBy (comparing snd) (Map.toList mp))

writeCode :: CodeBlock -> ErrorT String PutM ()
writeCode codeObject =
  do
    writeInstructions $ map instr (block_instructions codeObject)
    writeConstants $ reverse (block_constants codeObject)
    writeTuple $ map PyString $ keysOrdered
      (Map.union
       (block_names codeObject)
       (Map.map fun_location (block_functions codeObject)))
    writeTuple $ map PyString (keysOrdered (block_varnames codeObject))
    writeTuple $ map PyString (keysOrdered (block_freevars codeObject))
    writeTuple $ map PyString (keysOrdered (block_cellvars codeObject))
    writeString $ PyString (block_name codeObject)
    writeString PyString { string = "<module>" }
    writeU32 1 -- first line of code ??
    writeString PyString { string = "aa" } -- lnotab ??

writeCodeObj :: PyType -> ErrorT String PutM ()
writeCodeObj obj =
  do writeU8 $ encodeType CODE
     writeU32 $ argcount obj
     writeU32 $ nlocals obj
     writeU32 $ stackSize obj
     writeU32 $ flags obj
     writeString $ code obj
     writeObject $ consts obj
     writeObject $ names obj
     writeObject $ varnames obj
     writeObject $ freevars obj
     writeObject $ cellvars obj
     writeString PyString { string = "file" }
     writeString $ name obj
     writeU32 1 -- first line of code ??
     writeString PyString { string = "bb" } -- lnotab ??

writeObject :: PyType -> ErrorT String PutM ()
writeObject obj =
  case obj of
    PyNone -> writeConst obj
    PyCode {..} -> writeCodeObj obj
    PyString {..} -> writeString obj
    PyTuple k -> writeTuple k
    PyInt i -> writeInt $ fromInteger i

writeInstructions :: [Instruction] -> ErrorT String PutM ()
writeInstructions ilist = do
  writeU8 $ encodeType STRING
  writeU32 $ computeInstructionSize ilist
  mapM_ writeU8 (concat (map encodeInstruction ilist))

writeString :: PyType -> ErrorT String PutM ()
writeString str = do
  writeU8 $ encodeType STRING
  writeU32 $ fromIntegral $ length (string str)
  mapM_ writeU8 (map (fromIntegral . ord) (string str))

writeInt :: Word32 -> ErrorT String PutM ()
writeInt int = do
  writeU8 $ encodeType INT
  writeU32 int
    
encodeType :: Type -> Word8
encodeType x = case Map.lookup x objectTypeToChar of
  Nothing -> error $ "NO SUCH TYPE "
  Just v -> fromIntegral $ ord v

writeTuple :: [PyType] -> ErrorT String PutM ()
writeTuple content = do
  writeU8 $ encodeType TUPLE
  writeU32 $ fromIntegral $ length content
  mapM_ writeObject content

writeContent :: String -> ErrorT String PutM ()
writeContent x = do
  writeU8 $ fromIntegral $ ord 's'
  writeU32 $ fromIntegral $ length x
  mapM_ writeU8 (map (fromIntegral . ord) x)

writeConstants :: [PyType] -> ErrorT String PutM ()
writeConstants cList =
  do
    writeU8 $ fromIntegral $ ord '('
    writeU32 $ fromIntegral $ length cList
    mapM_ writeConst cList

writeConst :: PyType -> ErrorT String PutM ()
writeConst PyNone = do
  writeU8 $ encodeType NONE

writeConst (PyInt a) = do
  writeU8 $ encodeType INT
  writeU32 $ fromIntegral a 

writeConst (PyString k) = do
  writeU8 $ encodeType STRING
  writeU32 $ fromIntegral $ length k
  mapM_ writeU8 (map (fromIntegral . ord) k)

writeConst (PyCode {..}) = do
  writeU8 $ encodeType CODE
  mapM_ writeU32 [argcount, nlocals, stackSize, flags]
  mapM_ writeObject [code, consts, names, varnames, freevars,
                     cellvars, PyString "fname", name ]
  writeU32 1
  writeString $ PyString { string = "a" }

-- XXX: TODO. We should clean up this, and generally reorganized the code
-- since many functions actually could be rewritten as writeTuple
writeConst (PyTuple _) = undefined

instructionSize :: Instruction -> Word32
instructionSize (Instruction _ Nothing) = 1
instructionSize (Instruction _ (Just _)) = 3
    
computeInstructionSize :: [Instruction] -> Word32
computeInstructionSize ilist = sum $ map instructionSize ilist
