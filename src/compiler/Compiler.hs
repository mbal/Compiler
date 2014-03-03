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

import Parser (Term(..), BOperation(..), UOperation(..), Identifier)
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
  , block_labelMap = Map.empty
  , block_instructionOffset = 0
  , block_labelsForNextInstruction = []
  , block_nextLabelID = 0
  , block_nextConstantID = 0
  , block_nextVariableID = 0
  }

initState = CState {
  cMagic = 168686339
  , cBlock = initBlock
  , cFilename = "out"
  }

newtype CompilerState a = CompilerState { runCompilerState :: State CState a }
    deriving (Functor, Applicative, Monad, MonadState CState)

-- emits opCode with the given argument
emitCodeArg :: OpCode -> Word16 -> CompilerState ()
emitCodeArg opCode arg = emitCode $ Instruction opCode (Just arg)

emitCodeNoArg :: OpCode -> CompilerState ()
emitCodeNoArg opCode = emitCode $ Instruction opCode Nothing

emitCode :: Instruction -> CompilerState ()
emitCode inst = do
  labels <- getBlockState block_labelsForNextInstruction
  offset <- getBlockState block_instructionOffset
  labelMap <- getBlockState block_labelMap
  modifyBlockState $ \s -> s { block_labelsForNextInstruction = [] }
  forM_ labels (\x -> updateLabelMap x offset)
  oldInstructions <- getBlockState block_instructions
  let annotatedInstruction = AugInstruction inst offset
  modifyBlockState $
    \s -> s { block_instructions = annotatedInstruction : oldInstructions
            , block_instructionOffset = offset + (instructionSize inst) }

updateLabelMap label index = do
   oldLabelMap <- getBlockState block_labelMap
   let newLabelMap = Map.insert label index oldLabelMap
   modifyBlockState $ \s -> s { block_labelMap = newLabelMap }

freshConstantId :: CompilerState Word16
freshConstantId = do
  cId <- getBlockState block_nextConstantID
  modifyBlockState $ \s -> s { block_nextConstantID = cId + 1 }
  return cId

modifyBlockState :: (CodeBlock -> CodeBlock) -> CompilerState ()
modifyBlockState f = do
  state <- getBlockState id
  setBlockState $ f state

getBlockState f = gets (f . cBlock)
setBlockState newState = do
  oldState <- get
  put $ oldState { cBlock = newState }

newLabel :: CompilerState LabelID
newLabel = do
  lId <- getBlockState block_nextLabelID
  modifyBlockState $ \s -> s { block_nextLabelID = lId + 1 }
  return lId

assignLabel lblId = do
  oldLbl <- getBlockState block_labelsForNextInstruction
  modifyBlockState $ \s -> s { block_labelsForNextInstruction = lblId : oldLbl }

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
  do x <- getBlockState block_nextVariableID
     modifyBlockState $ \s -> s { block_nextVariableID = x + 1 }
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

-- main purpose of this function is to fix jump targets
assemble = do
  lblMap <- getBlockState block_labelMap
  code <- fmap reverse (getBlockState block_instructions)
  let annotatedCode = fixJumpTarget lblMap code
  modifyBlockState $ \s -> s { block_instructions = annotatedCode }

fixJumpTarget :: Map.Map LabelID Word16 -> [AugInstruction] -> [AugInstruction]
fixJumpTarget labelMap code =
  -- walk over the code, if an instruction is a jump, look it up in the
  -- label map, compute the offset (if relative) and fix the target
  map (\x -> fixJump labelMap x) code

fixJump mp instr@(AugInstruction (Instruction opcode arg) index) =
  if isJump opcode then
    case arg of
      Just x ->
        let index = Map.lookup x mp in
        case index of
          Just k -> newBytecode k instr
          Nothing -> error "shouldn't happen"
      Nothing -> error $ "shouldn't happen"
  else instr

newBytecode target (AugInstruction (Instruction opcode (Just arg)) index) =
  if isRelativeJump opcode then
    AugInstruction (Instruction opcode (Just t)) index
  else
    AugInstruction (Instruction opcode (Just target)) index
  where t = (target - (index + 3))

isJump :: OpCode -> Bool
isJump x = isRelativeJump x || isAbsoluteJump x

isRelativeJump :: OpCode -> Bool
isRelativeJump JUMP_FORWARD = True
isRelativeJump SETUP_LOOP = True
isRelativeJump FOR_ITER = True
isRelativeJump SETUP_FINALLY = True
isRelativeJump SETUP_EXCEPT = True
isRelativeJump SETUP_WITH = True
isRelativeJump _ = False

isAbsoluteJump :: OpCode -> Bool
isAbsoluteJump POP_JUMP_IF_FALSE = True
isAbsoluteJump POP_JUMP_IF_TRUE = True
isAbsoluteJump JUMP_ABSOLUTE = True
isAbsoluteJump CONTINUE_LOOP = True
isAbsoluteJump JUMP_IF_FALSE_OR_POP = True
isAbsoluteJump JUMP_IF_TRUE_OR_POP = True
isAbsoluteJump _ = False

compileTopLevel xs =
  do mapM compile xs
     returnNone
     assemble

returnNone =
  do noneId <- createConstant PyNone
     emitCodeArg LOAD_CONST noneId
     emitCodeNoArg RETURN_VALUE

compile (UnaryOp Minus e) =
  case e of
    Const k -> compile (Const (-k))
    _ -> do compile e
            emitCodeNoArg UNARY_NEGATIVE
     
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
    Nothing -> error $ "Unknown variable " ++ (show k)
compile (If cond thn els) = do
  compile cond
  flsLabel <- newLabel
  endLabel <- newLabel
  emitCodeArg POP_JUMP_IF_FALSE flsLabel
  compile thn
  emitCodeArg JUMP_FORWARD endLabel
  assignLabel flsLabel
  compile els
  assignLabel endLabel

-- 333: Py3k incompatibily, print is a function, so it should be loaded and
-- called like any other functions.
compile (FunApp (Var "print") args) = do
  mapM compile args
  emitCodeNoArg PRINT_ITEM

compile (FunApp (Var fname) args) = do
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
                   , code = PyString $ map (chr . fromIntegral)
                            (concat (map encodeInstruction' (reverse instr)))
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

-- XXX: that's not really true, it also depends on the context (i.e.
-- function, class, module, ...)
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
