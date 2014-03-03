{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, MultiParamTypeClasses #-}
module Compiler where
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative (Applicative)
import qualified Data.ByteString.Lazy as B
   (ByteString, hGetContents, unpack, hPutStr, length)
import Control.Monad.Error (ErrorT (..), lift, replicateM)
import Data.Word (Word32, Word16, Word8)
import System.IO
import Debug.Trace
import Data.Char
import qualified Data.Map as Map hiding (map)

import Parser (Term(..), BOperation(..), Identifier)
import Bytecode
import Emit
import Types

initBlock = CodeBlock {
  block_instructions = []
  , block_names = Map.empty
  , block_constants = Map.empty
  , block_varnames = Map.empty
  , block_freevars = Map.empty
  , block_cellvars = Map.empty
  , block_name = ""
  , nextConstantID = 0
  , nextVariableID = 0
  }

initState = CState {
  cMagic = 168686339
  , cBlock = initBlock
  , cFilename = "out"
  }

{-newtype CompilerState a =
  CompilerState (StateT CState IO a)
  deriving (Monad, Functor, Applicative, MonadIO)

instance MonadState CState CompilerState where
  get = CompilerState get
  put s = CompilerState $ put s

runCompilerState (CompilerState comp) = evalStateT comp -}

newtype CompilerState a = CompilerState { runCompilerState :: State CState a }
    deriving (Functor, Applicative, Monad, MonadState CState)

-- emits opCode with the given argument
emitCodeArg :: OpCode -> Word16 -> CompilerState ()
emitCodeArg opCode arg = emitCode $ Instruction opCode (Just arg)

emitCodeNoArg :: OpCode -> CompilerState ()
emitCodeNoArg opCode = emitCode $ Instruction opCode Nothing

emitCode :: Instruction -> CompilerState ()
emitCode inst = do
  oldInstructions <- getBlockState block_instructions
  modifyBlockState $ \s -> s { block_instructions = inst : oldInstructions }

freshConstantId :: CompilerState Word16
freshConstantId = do
  cId <- getBlockState nextConstantID
  modifyBlockState $ \s -> s { nextConstantID = cId + 1 }
  return cId

modifyBlockState :: (CodeBlock -> CodeBlock) -> CompilerState ()
modifyBlockState f = do
  state <- getBlockState id
  setBlockState $ f state

getBlockState f = gets (f . cBlock)
setBlockState newState = do
  oldState <- get
  put $ oldState { cBlock = newState }

createConstant :: PyType -> CompilerState Word16
createConstant c@(PyCode {..}) =
  newConstant c
createConstant obj =
  (do s <- getBlockState block_constants
      case Map.lookup obj s of
        Nothing -> newConstant obj
        Just x -> return x)

newConstant name =
  do cId <- freshConstantId
     constants <- getBlockState block_constants
     modifyBlockState $ \s -> s { block_constants = Map.insert name cId constants }
     return cId

freshVariableId :: CompilerState Word16
freshVariableId =
  do x <- getBlockState nextVariableID
     modifyBlockState $ \s -> s { nextVariableID = x + 1 }
     return x

createVariable varName =
  do vars <- getBlockState block_names
     case Map.lookup varName vars of
       Nothing -> newVariable varName
       Just x -> return $ x

newVariable name =
  do vId <- freshVariableId
     vars <- getBlockState block_names
     modifyBlockState $ \s -> s { block_names = Map.insert name vId vars }
     return vId

{-- ============= STATE ============= --}

indexOf :: (Eq a) => a -> [a] -> Maybe Integer
indexOf y list = indexOf' y list 0
  where indexOf' _ [] _ = Nothing
        indexOf' y (x:xs) a = if x == y then (Just a) else indexOf' y xs (a+1)


compileTopLevel xs =
  do mapM compile xs
     returnNone

returnNone =
  do noneId <- createConstant PyNone
     emitCodeArg LOAD_CONST noneId
     emitCodeNoArg RETURN_VALUE

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
  s <- getBlockState block_names
  res <- searchVariable k
  case res of
    Just (typ, x) -> emitReadVar typ x
    Nothing -> error "Unknown variable"

-- 333: Py3k incompatibily, print is a function, so it should be loaded and
-- called like any other functions.
compile (FunApp "print" args) = do
  mapM compile args
  emitCodeNoArg PRINT_ITEM

compile (FunApp fname args) = do
  s <- getBlockState block_names
  case Map.lookup fname s of
    Nothing -> error $ "No such function: " ++ fname
    Just x -> do emitCodeArg LOAD_NAME x
                 mapM compile args
                 emitCodeArg CALL_FUNCTION (fromIntegral (length args)) -- XXX: 0 if no *args, **kwargs

compile (Defun fname fargs body) = do
  oldState <- getBlockState id
  modify $ \s -> s { cBlock = initBlock }
  modifyBlockState $ \s -> s { block_varnames = computeLocalsForFunction fargs }
  compile body
  emitCodeNoArg RETURN_VALUE
  compiledBody <- makeObject fargs
  modify $ \s -> s { cBlock = oldState }
  compileClosure (PyString { string=fname }) compiledBody fargs
  vId <- createVariable fname
  emitCodeArg STORE_NAME vId

--makeObject :: CompilerState PyType
makeObject fargs = do
  instr <- getBlockState block_instructions
  cnst <- getBlockState block_constants
  locals <- getBlockState block_varnames
  vnames <- getBlockState block_names
  let obj = PyCode {
                   argcount = fromIntegral (length fargs)
                   , nlocals = fromIntegral (length $ Map.toList locals)
                   , stackSize = 100
                   , flags = 0x43
                   , code = PyString $ (map (chr . fromIntegral) (concat (reverse (map encodeInstruction instr))))
                   , consts = PyTuple $ Map.keys cnst
                   , varnames = PyTuple $ map PyString (Map.keys locals)
                   , names = PyTuple $ map PyString (Map.keys vnames)
                   }
  return obj
  where
    makeConstants = Map.keys
    makeNames = Map.elems

emitWriteVar fname = do
  cId <- createConstant fname
  emitCodeArg STORE_NAME cId

compileClosure :: PyType -> PyType -> [Identifier] -> CompilerState ()
compileClosure name cbody args =
  do compileConstantEmit cbody
     emitCodeArg MAKE_FUNCTION 0

compileConstantEmit obj =
  do constantId <- createConstant obj
     emitCodeArg LOAD_CONST constantId

computeLocalsForFunction :: [Identifier] -> Map.Map Identifier Word16
computeLocalsForFunction formalParameters =
  Map.fromList (zip formalParameters [0.. ])

emitReadVar typ index =
  case typ of
    Name -> emitCodeArg LOAD_NAME index
    Global -> emitCodeArg LOAD_GLOBAL index
    Fast -> emitCodeArg LOAD_FAST index
    Deref -> emitCodeArg LOAD_DEREF index

searchVariable :: Identifier -> CompilerState (Maybe (VarType, Word16))
searchVariable varName = do
  whr <- getBlockState block_varnames
  case Map.lookup varName whr of
    Just x -> return $ Just (Fast, x)
    Nothing -> do
      globals <- getBlockState block_names
      case Map.lookup varName globals of
        Just y -> return $ Just (Global, y)
        Nothing -> return Nothing
