{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, MultiParamTypeClasses #-}
module Compiler where
import Control.Monad.Reader
import Control.Monad.State (MonadState, State, put, get, gets, modify)
import Control.Applicative (Applicative)
import Data.Word (Word16)
import Debug.Trace
import Data.Char
import qualified Data.Map as Map hiding (map)

import Parser (Term(..), BOperation(..), UOperation(..), Identifier, Value(..))
import Bytecode
import Emit
import Types

initBlock :: CodeBlock
initBlock = CodeBlock {
  block_instructions = []
  , block_names = Map.empty
  , block_constantsMap = Map.empty
  , block_constants = []
  , block_functions = Map.empty
  , block_varnames = Map.empty
  , block_freevars = Map.empty
  , block_cellvars = Map.empty
  , block_name = ""
  , block_labelMap = Map.empty
  , block_instructionOffset = 0
  , block_labelsForNextInstruction = []
  , block_nextNameID = 0
  , block_nextLabelID = 0
  , block_nextConstantID = 0
  , block_nextVariableID = 0
  }

initState :: CState
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
  --labelMap <- getBlockState block_labelMap
  modifyBlockState $ \s -> s { block_labelsForNextInstruction = [] }
  forM_ labels (\x -> updateLabelMap x offset)
  oldInstructions <- getBlockState block_instructions
  let annotatedInstruction = AugInstruction inst offset
  modifyBlockState $
    \s -> s { block_instructions = annotatedInstruction : oldInstructions
            , block_instructionOffset = offset + (instructionSize inst) }

updateLabelMap :: LabelID -> Word16 -> CompilerState ()
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
createConstant c@(PyCode {..}) = do
  cId <- freshConstantId
  consts <- getBlockState block_constants
  modifyBlockState $ \s -> s { block_constants = c : consts }
  return cId

createConstant obj =
  do s <- getBlockState block_constantsMap
     case Map.lookup obj s of
       Nothing -> newConstant obj
       Just x -> return x

newConstant name =
  do cId <- freshConstantId
     cMap <- getBlockState block_constantsMap
     consts <- getBlockState block_constants
     modifyBlockState $ \s -> s { block_constantsMap = Map.insert name cId cMap,
                                  block_constants = name : consts }
     return cId

freshVariableId :: CompilerState Word16
freshVariableId =
  do x <- getBlockState block_nextVariableID
     modifyBlockState $ \s -> s { block_nextVariableID = x + 1 }
     return x

freshGlobalId =
  do x <- getBlockState block_nextNameID
     modifyBlockState $ \s -> s { block_nextNameID = x + 1 }
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

nestedBlock computation = do
  -- save the old state
  oldState <- getBlockState id
  -- get a new state
  modify $ \s -> s { cBlock = initBlock }
  -- run the computation
  result <- computation
  -- restore the old state
  modify $ \s -> s { cBlock = oldState }
  return result

{-- ============= STATE ============= --}


{-- =========== ASSEMBLE ============ --}
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

fixJump :: Map.Map LabelID Word16 -> AugInstruction -> AugInstruction
fixJump mp augInstr@(AugInstruction (Instruction opcode arg) index) =
  if isJump opcode then
    case arg of
      Just x ->
        let jmpTarget = Map.lookup x mp
            newArg = case jmpTarget of
              Just k -> computeTarget k index (instr augInstr)
              Nothing -> error "shouldn't happen"
            in augInstr { instr = (Instruction opcode (Just newArg)) }
      Nothing -> error $ "shouldn't happen"
  else augInstr

computeTarget :: Word16 -> Word16 -> Instruction -> Word16
computeTarget target instrIndex instr@(Instruction opcode _) =
  if isRelativeJump opcode then
    fromIntegral (target - (instrIndex + (instructionSize instr)))
  else
    fromIntegral target

{-- =========== ASSEMBLE ============ --}

compileTopLevel xs =
  do mapM compile xs
     returnNone
     assemble

returnNone =
  do noneId <- createConstant PyNone
     emitCodeArg LOAD_CONST noneId
     emitCodeNoArg RETURN_VALUE


compile (Array elements) =
  do mapM compile elements
     emitCodeArg BUILD_LIST (fromIntegral (length elements))
  
--- small optimization, but XXX: is it true?
compile (UnaryOp Plus e) =
  compile e

compile (UnaryOp Minus e) =
  case e of
    Const (Number k) -> compile (Const (Number (-k)))
    _ -> do compile e
            emitCodeNoArg UNARY_NEGATIVE

compile (UnaryOp Not e) = do
  compile e
  emitCodeNoArg UNARY_NOT

compile (UnaryOp Prime e) = do
  oldState <- getBlockState id
  modify $ \s -> s { cBlock = initBlock }
  modifyBlockState $ \s -> s { block_varnames = computeLocalsForFunction [] }
  compile e
  emitCodeNoArg RETURN_VALUE
  assemble
  compiledBody <- makeObject []
  modify $ \s -> s { cBlock = oldState }
  compileClosure (PyString { string="<lambda>" }) compiledBody []
     
compile (BinaryOp At e1 idx) =
  case e1 of
    (Array _) -> compileAt e1 idx
    _ -> error $ "@ operator used on non array element"
  {-do compile e1
     compile idx
     emitCodeNoArg BINARY_SUBSCR -}


compile (BinaryOp Compose f1 f2) = do
  compBody <- nestedBlock $
              (do modifyBlockState $
                    \s -> s { block_varnames = computeLocalsForFunction ["x"] }
                  compile (FunApp f1 [(FunApp f2 [Var "x"])])
                  emitCodeNoArg RETURN_VALUE
                  assemble
                  makeObject ["x"])
  compileClosure (PyString { string="lambda" }) compBody ["x"]

compile (BinaryOp And e1 e2) = do
  compile e1
  end <- newLabel
  emitCodeArg JUMP_IF_FALSE_OR_POP end
  compile e2
  assignLabel end

compile (BinaryOp Or e1 e2) = do
  compile e1
  end <- newLabel
  emitCodeArg JUMP_IF_TRUE_OR_POP end
  compile e2
  assignLabel end
     
compile node@(BinaryOp op e1 e2) =
  if isArith op then
    compileArith node
  else
    compileComparison node

compile (Const (Number a)) = do
  cId <- createConstant (PyInt { intvalue = a })
  emitCodeArg LOAD_CONST cId

compile (Const (StringValue s)) = do
  cId <- createConstant (PyString { string = s})
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
    Nothing -> do gId <- createGlobalVariable k
                  emitReadVar Global gId

compile (Hook f1 f2) = do
  --af1 <- arity f1
  --af2 <- arity f2
  -- thanks to the AST, both f1 and f2 are functions, but they could be
  -- of the wrong arity (2 for f1 and 1 for f2). On the other hand, this
  -- check is done by the python vm, for free. However, it would be nice
  -- to be warned before.
  --if isFunction f1 && isFunction f2 && (af1 == 2) && (af2 == 1) then
  compileAnonFunction (Hook f1 f2)
  --else
  --  error "domain error: in a hook, (f g) must be f/2 and g/1"
  
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
  fns <- getBlockState block_functions
  case Map.lookup fname fns of
    Just fnObj -> emitFunctionCall fname fnObj args
    Nothing -> {- here, we don't have the function ready, but
                  it could be a variable, binded with let x = foo; or
                  it could be defined after -}
      do (typ, x) <- getVariable fname
         emitReadVar typ x
         mapM compile args
         emitCodeArg CALL_FUNCTION (fromIntegral $ length args)

compile (FunApp term args) =
  do case term of
       (UnaryOp Prime _) -> do compile term
                               emitCodeArg CALL_FUNCTION 0
       _ -> do compile term
               mapM compile args
               emitCodeArg CALL_FUNCTION (fromIntegral (length args))

compile (Defun fname fargs body) = do
  compBody <- nestedBlock $
              (do modifyBlockState $
                    \s -> s { block_varnames = computeLocalsForFunction fargs
                            , block_name = fname }
                  compile body
                  emitCodeNoArg RETURN_VALUE
                  assemble
                  makeObject fargs)
  compileClosure (PyString { string=fname}) compBody fargs
  vId <- createFunction fname (length fargs) False
  emitCodeArg STORE_NAME vId

compile other = error $ "unknown: " ++ (show other)

createFunction :: String -> Int -> Bool -> CompilerState Word16
createFunction fname args isPrime = do
  oldFuns <- getBlockState block_functions
  fId <- freshVariableId
  let fobj = Function { fun_numArgs = args
                      , fun_isPrime = isPrime
                      , fun_location = fId
                      , fun_type = Global
                      }
  modifyBlockState $ \s -> s { block_functions = Map.insert fname fobj oldFuns }
  return fId

makeObject fargs = do
  instr <- getBlockState block_instructions
  cnst <- getBlockState block_constants
  locals <- getBlockState block_varnames
  vnames <- getBlockState block_names
  bname <- getBlockState block_name
  let obj = PyCode {
                   argcount = fromIntegral (length fargs)
                   , nlocals = fromIntegral (length $ Map.toList locals)
                   , stackSize = 100
                   , flags = 0x43
                   , code = PyString $ map (chr . fromIntegral)
                            (concat (map encodeInstruction' instr))
                   , consts = PyTuple $ reverse cnst
                   , varnames = PyTuple $ map PyString (keysOrdered locals)
                   , names = PyTuple $ map PyString (keysOrdered vnames)
                   , name = PyString bname
                   }
  return obj

emitWriteVar :: PyType -> CompilerState ()
emitWriteVar fname = do
  cId <- createConstant fname
  emitCodeArg STORE_NAME cId

compileClosure :: PyType -> PyType -> [Identifier] -> CompilerState ()
compileClosure name cbody args =
  do compileConstantEmit cbody
     emitCodeArg MAKE_FUNCTION 0

compileConstantEmit :: PyType -> CompilerState ()
compileConstantEmit obj =
  do constantId <- createConstant obj
     emitCodeArg LOAD_CONST constantId

computeLocalsForFunction :: [Identifier] -> Map.Map Identifier Word16
computeLocalsForFunction formalParameters =
  Map.fromList (zip formalParameters [0.. ])

-- XXX: that's not really true, it also depends on the context (i.e.
-- function, class, module, ...)
emitReadVar :: VarType -> Word16 -> CompilerState ()
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

createGlobalVariable :: Identifier -> CompilerState VariableID
createGlobalVariable name = do
  gId <- freshGlobalId
  names <- getBlockState block_names
  modifyBlockState $ \s -> s { block_names = Map.insert name gId names }
  return gId

isArith Add = True
isArith Subtract = True
isArith Multiply = True
isArith Divide = True
isArith _ = False

compileArith (BinaryOp op e1 e2) =
  do compile e1
     compile e2
     emitCodeNoArg $ emitOp op
     where emitOp Add = BINARY_ADD
           emitOp Subtract = BINARY_SUBTRACT
           emitOp Multiply = BINARY_MULTIPLY
           emitOp Divide = BINARY_TRUE_DIVIDE
           emitOp _ = error "compileArith: operation not valid"

compileComparison (BinaryOp op e1 e2) =
  do compile e1
     compile e2
     emitCodeArg COMPARE_OP (opKind op) 
  where opKind Lesser = 0
        opKind LEQ = 1
        opKind Equal = 2
        opKind NotEqual = 3
        opKind Greater = 4
        opKind GEQ = 5
        opKind o = error $ (show o)

emitFunctionCall fname fnObj args = 
    if fun_isPrime fnObj then
        emitCodeArg CALL_FUNCTION (fromIntegral $ length args)
    else
        if (length args) /= (fun_numArgs fnObj) then
            error $ "ERROR: function " ++ fname ++ "was called with " ++ 
                (show $ length args) ++ " but needs " ++
                (show $ fun_numArgs fnObj)
        else do emitReadVar (fun_type fnObj) (fun_location fnObj)
                mapM compile args
                emitCodeArg CALL_FUNCTION (fromIntegral $ length args)

getVariable name = do
  res <- searchVariable name
  (typ, x) <- case res of
    Just (a, b) -> do return (a, b)
    Nothing -> do gId <- createGlobalVariable name
                  return (Global, gId)
  return (typ, x)

compileAgenda arrExpr idxExpr =
  -- we compile the agenda version of At with an if expression
  -- ([f, g]@k)(n) ==> let h = if k == 0 then f else g; h(n);
  -- ([f, g]@k)    ==> let h = if k == 0 then f else g;
  -- unluckily, neither this approach can solve the problem with the
  -- primed function (that can be called with any number of arguments)
  traceShow arrExpr $ 
  compile (If (BinaryOp Equal idxExpr (Const (Number 0)))
           (arrExpr !! 0)
           (arrExpr !! 1))

compileAt arrExpr@(Array elems) idxExpr = do
  compile arrExpr
  compile idxExpr
  emitCodeNoArg BINARY_SUBSCR
  {-if (all isFunction elems) then
    compileAgenda elems idxExpr
  else do compile arrExpr
          compile idxExpr
          emitCodeNoArg BINARY_SUBSCR -}

searchFunction k = do
  fns <- getBlockState block_functions
  return $ Map.lookup k fns

isFunction (BinaryOp Compose _ _) = True
isFunction (BinaryOp _ _ _) = False
isFunction (BinOp _) = False
isFunction (UnaryOp Prime _) = True
isFunction (UnaryOp _ _) = False
isFunction (Var k) = True -- XXX: not true
isFunction (Hook _ _) = True
isFunction (FunApp _ _) = False
isFunction (Defun _ _ _) = False

arity :: Term -> CompilerState Int
arity (Var f) = do
  fns <- getBlockState block_functions
  case Map.lookup f fns of
    Nothing -> error $ "not a function"
    Just fnObj -> return $ (fun_numArgs fnObj)

compileAnonFunction (Hook f1 f2) = do
  compBody <- nestedBlock $
              (do modifyBlockState $
                    \s -> s { block_varnames = computeLocalsForFunction ["x"] }
                  compile (FunApp f1 [(Var "x"), (FunApp f2 [Var "x"])])
                  emitCodeNoArg RETURN_VALUE
                  assemble
                  makeObject ["x"])
  compileClosure (PyString { string="lambda" }) compBody ["x"]
