{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler where
import Control.Monad.Reader
import Control.Monad.State (modify)
import Data.Word (Word16)
import Data.Char
import Data.Maybe (mapMaybe)
import Data.List ((\\), union)
import qualified Data.Map as Map

import Parser (Term(..), BOperation(..), UOperation(..), Identifier,
               Value(..), SFKind(..))
import Bytecode
import Assemble
import Emit
import Types

import Debug.Trace

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
  , block_flags = 0
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
  , cDefinitions = Map.empty
  }

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
  -- XXX: we should compute the line number of the instruction, to get
  -- precise exceptions.
  modifyBlockState $ \s -> s { block_labelsForNextInstruction = [] }
  forM_ labels (\x -> updateLabelMap x offset)
  oldInstructions <- getBlockState block_instructions
  let annotatedInstruction = AugInstruction inst offset
  modifyBlockState $
    \s -> s { block_instructions = annotatedInstruction : oldInstructions
            , block_instructionOffset = offset +
                                        fromIntegral (instructionSize inst) }

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

newLabel :: CompilerState LabelID
newLabel = do
  lId <- getBlockState block_nextLabelID
  modifyBlockState $ \s -> s { block_nextLabelID = lId + 1 }
  return lId

assignLabel :: LabelID -> CompilerState ()
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

newConstant :: PyType -> CompilerState Word16
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

freshGlobalId :: CompilerState Word16
freshGlobalId =
  do x <- getBlockState block_nextNameID
     modifyBlockState $ \s -> s { block_nextNameID = x + 1 }
     return x

createVariable :: Identifier -> CompilerState Word16
createVariable varName =
  do vars <- getBlockState block_names
     case Map.lookup varName vars of
       Nothing -> newVariable varName
       Just x -> return $ x

newVariable :: Identifier -> CompilerState Word16
newVariable name =
  do vId <- freshVariableId
     vars <- getBlockState block_names
     modifyBlockState $ \s -> s { block_names = Map.insert name vId vars }
     return vId

nestedBlock :: CompilerState a -> CompilerState a
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

compileTopLevel :: [Term] -> CompilerState ()
compileTopLevel xs =
  do mapM_ compile xs
     returnNone
     assemble

returnNone :: CompilerState ()
returnNone =
  do noneId <- createConstant PyNone
     emitCodeArg LOAD_CONST noneId
     emitCodeNoArg RETURN_VALUE

compile :: Term -> CompilerState ()
compile (Array elements) =
  do mapM_ compile elements
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
     
compile node@(BinaryOp op _ _) =
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

compile (Const Nil) = do
  cId <- createConstant PyNone
  emitCodeArg LOAD_CONST cId

-- 333: possible Py3k incompatibility: True and False are reserved,
-- so they are probably saved in a different way.
compile (Const VTrue) = do
  compile (Const (StringValue "True"))

compile (Const VFalse) = do
  compile (Const (StringValue "False"))

compile (Let k e) = do
  case e of
    Lambda args _ -> do compile e
                        fId <- createFunction k (length args) False
                        emitCodeArg STORE_NAME fId
    _ -> do compile e
            vId <- createVariable k
            emitCodeArg STORE_NAME vId

compile (Var k) = do
  res <- searchVariable k
  case res of
    Just (typ, x) -> emitReadVar typ x
    Nothing -> do gId <- createGlobalVariable k
                  emitReadVar Global gId
  
compile (If cond thn els) = do
  -- XXX: can be optimized. (e.g. cond is an immediate)
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
  mapM_ compile args
  emitCodeNoArg PRINT_ITEM

compile (FunApp (Var fname) args) = do
  fns <- getBlockState block_functions
  case Map.lookup fname fns of
    Just fnObj -> emitFunctionCall fname fnObj args
    Nothing ->
      -- In this case, the function is not yet defined, therefore it's not
      -- in the current block. Moreover, it could be a variable, bound with
      -- let x = function. However, thanks to the passes, we have all
      -- the defined functions in a list. We do some basic checks (such as
      -- check that the number of arguments is correct).
      do (typ, x) <- getVariable fname
         emitReadVar typ x
         mapM_ compile args
         emitCodeArg CALL_FUNCTION (fromIntegral $ length args)
      {-do defs <- gets cDefinitions
         case Map.lookup fname defs of
           Just def ->
             if isPrime def then
               emitCodeArg CALL_FUNCTION (fromIntegral $ length args)
             else
               if (length args) /= (numArgs def) then
                 error $ "ERROR: function " ++ fname ++ " was called with " ++ 
                 (show $ length args) ++ " but needs " ++
                 (show $ numArgs def)
               else 
                 do (typ, x) <- getVariable fname
                    emitReadVar typ x
                    mapM_ compile args
                    emitCodeArg CALL_FUNCTION (fromIntegral $ length args)
           Nothing -> error $ "unknown function " ++ fname -}

compile (FunApp term args) =
  do case term of
       (UnaryOp Prime _) -> do compile term
                               emitCodeArg CALL_FUNCTION 0
       _ -> do compile term
               mapM_ compile args
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


compile expr@(Lambda args body) = do
  -- lambdas can have free variables.
  compBody <- nestedBlock $
              (do enterScope "<lambda>" expr
                  {-modifyBlockState $
                    \s -> s { block_varnames = computeLocalsForFunction args
                            , block_name = "<lambda>" } -}
                  compile body
                  emitCodeNoArg RETURN_VALUE
                  assemble
                  makeObject args)
  compileClosure (PyString { string = "<lambda>" }) compBody args

-- from now on, that's basically desugaring.
compile (SpecialForm Compose funcs) = do
  -- SpecialForm Compose [f1, f2, f3, ...] =
  --    = \x -> f1(f2(f3(...(x))));
  compile (Lambda ["x"] (foldr1 (\x y -> FunApp x [y]) (funcs ++ [Var "x"])))

compile (SpecialForm Hook exps) =
  compile $ handleTrain exps
  
handleTrain :: [Term] -> Term
handleTrain exps =
  if (length exps `rem` 2) == 0 then
    handleHook exps
  else
    handleFork exps

handleHook :: [Term] -> Term
handleHook [] = error "DOMAIN ERROR"
handleHook (f:[g]) = 
  (Lambda ["x"] (FunApp f [Var "x", FunApp g [Var "x"]]))
handleHook (f:gs) =
  (Lambda ["x"] (FunApp f [Var "x", handleTrain gs]))

handleFork :: [Term] -> Term
handleFork (f:g:[h]) =
  (Lambda ["x"] (FunApp g [FunApp f [Var "x"],
                           FunApp h [Var "x"]]))
handleFork (f:g:hs) =
  (Lambda ["x"] (FunApp g [FunApp f [Var "x"],
                           handleTrain hs]))

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

makeObject :: [Identifier] -> CompilerState PyType
makeObject fargs = do
  instr <- getBlockState block_instructions
  cnst <- getBlockState block_constants
  vnames <- getBlockState block_varnames
  fnamest <- getBlockState block_functions
  let fnames = Map.map fun_location fnamest
  let locals = Map.union vnames fnames
  bnames <- getBlockState block_names
  bname <- getBlockState block_name
  freevars <- getBlockState block_freevars
  cellvars <- getBlockState block_cellvars
  flags <- getBlockState block_flags
  let obj = PyCode {
                   argcount = fromIntegral (length fargs)
                   , nlocals = fromIntegral (length $ Map.toList locals)
                   , stackSize = 100
                   , flags = flags
                   , code = PyString $ map (chr . fromIntegral)
                            (concat (map encodeInstruction' instr))
                   , consts = PyTuple $ reverse cnst
                   , varnames = PyTuple $ map PyString (keysOrdered locals)
                   , freevars = PyTuple $ map PyString (keysOrdered freevars)
                   , cellvars = PyTuple $ map PyString (keysOrdered cellvars)
                   , names = PyTuple $ map PyString (keysOrdered bnames)
                   , name = PyString bname
                   }
  return obj

emitWriteVar :: PyType -> CompilerState ()
emitWriteVar fname = do
  cId <- createConstant fname
  emitCodeArg STORE_NAME cId

-- TODO: flags change when f is a closure.
-- Here, we do "the other way round", with respect to cpython:
-- we first compile, then compute the cellvars for the parent.
compileClosure :: PyType -> PyType -> [Identifier] -> CompilerState ()
compileClosure nm cbody args = do
  let numFreeVars = length (elements $ freevars cbody) 
  if (numFreeVars == 0) then
    do compileConstantEmit cbody
       emitCodeArg MAKE_FUNCTION 0
    else
    do let fv = elements $ freevars cbody
       {-forM_ fv $ \var -> do
         maybeVarInfo <- lookupClosureVar var
         case maybeVarInfo of
           Just (CellVar i) -> emitCodeArg LOAD_CLOSURE index
           Just (FreeVar i) -> emitCodeArg LOAD_CLOSURE index
           _ -> error $ "closure variable " ++ fv ++ " in " ++ name ++
                "is not a cell variable in enclosing scope"
       emitCodeArg BUILD_TUPLE $ fromIntegral numFreeVars
       compileConstantEmit cbody
       emitCodeArg MAKE_CLOSURE 0 -}
       modifyBlockState $
         \s -> s { block_cellvars = Map.fromList $ zip (map string fv) [0..] }
       mapM_ (\(x,y) -> (emitCodeArg LOAD_CLOSURE y)) (zip fv [0..])
       emitCodeArg BUILD_TUPLE (fromIntegral $ (length fv))
       compileConstantEmit cbody
       emitCodeArg MAKE_CLOSURE 0

compileConstantEmit :: PyType -> CompilerState ()
compileConstantEmit obj =
  do constantId <- createConstant obj
     emitCodeArg LOAD_CONST constantId

computeLocalsForFunction :: [Identifier] -> Map.Map Identifier Word16
computeLocalsForFunction formalParameters =
  Map.fromList (zip formalParameters [0.. ])

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
        Nothing -> do
          freevars <- getBlockState block_freevars
          case Map.lookup varName freevars of
            Just y -> return $ Just (Deref, y)
            Nothing -> return Nothing

createGlobalVariable :: Identifier -> CompilerState VariableID
createGlobalVariable name = do
  gId <- freshGlobalId
  names <- getBlockState block_names
  modifyBlockState $ \s -> s { block_names = Map.insert name gId names }
  return gId

isArith :: BOperation -> Bool
isArith Add = True
isArith Subtract = True
isArith Multiply = True
isArith Divide = True
isArith _ = False

compileArith :: Term -> CompilerState ()
compileArith (BinaryOp op e1 e2) =
  do compile e1
     compile e2
     emitCodeNoArg $ emitOp op
     where emitOp Add = BINARY_ADD
           emitOp Subtract = BINARY_SUBTRACT
           emitOp Multiply = BINARY_MULTIPLY
           emitOp Divide = BINARY_TRUE_DIVIDE
           emitOp _ = error "compileArith: operation not valid"
compileArith _ = error "DOMAIN ERROR: compileArith on non-arithmetic operation"

compileComparison :: Term -> CompilerState ()
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
        opKind _ = error "DOMAIN ERROR: compileComparison applied on non comparison"

compileComparison _ = error "DOMAIN ERROR: compileComparison applied on non comparison"

emitFunctionCall :: String -> Function -> [Term] -> CompilerState ()
emitFunctionCall fname fnObj args = 
    if fun_isPrime fnObj then
        emitCodeArg CALL_FUNCTION (fromIntegral $ length args)
    else
        if (length args) /= (fun_numArgs fnObj) then
            error $ "ERROR: function " ++ fname ++ "was called with " ++ 
                (show $ length args) ++ " but needs " ++
                (show $ fun_numArgs fnObj)
        else do emitReadVar (fun_type fnObj) (fun_location fnObj)
                mapM_ compile args
                emitCodeArg CALL_FUNCTION (fromIntegral $ length args)

getVariable :: Identifier -> CompilerState (VarType, Word16)
getVariable name = do
  res <- searchVariable name
  (typ, x) <- case res of
    Just (a, b) -> do return (a, b)
    Nothing -> do gId <- createGlobalVariable name
                  return (Global, gId)
  return (typ, x)

compileAgenda :: [Term] -> Term -> CompilerState ()
compileAgenda arrExpr idxExpr =
  -- we compile the agenda version of At with an if expression
  -- ([f, g]@k)(n) ==> let h = if k == 0 then f else g; h(n);
  -- ([f, g]@k)    ==> let h = if k == 0 then f else g;
  -- unluckily, neither this approach can solve the problem with the
  -- primed function (that can be called with any number of arguments)
  compile (If (BinaryOp Equal idxExpr (Const (Number 0)))
           (arrExpr !! 0)
           (arrExpr !! 1))

compileAt :: Term -> Term -> CompilerState ()
compileAt arrExpr@(Array _) idxExpr = do
  compile arrExpr
  compile idxExpr
  emitCodeNoArg BINARY_SUBSCR
  {-if (all isFunction elems) then
    compileAgenda elems idxExpr
  else do compile arrExpr
          compile idxExpr
          emitCodeNoArg BINARY_SUBSCR -}
compileAt _ _ = error $ "Domain error on @ operator"

searchFunction :: Identifier -> CompilerState (Maybe Function)
searchFunction k = do
  fns <- getBlockState block_functions
  return $ Map.lookup k fns

enterScope name expr@(Lambda args body) = do
  modifyBlockState $
    \s -> s {
      block_varnames = computeLocalsForFunction args
      , block_freevars = Map.fromList (zip (freeVariables expr) [0..])
      , block_name = name
      }

freeVariables (Defun _ args body) = freeVariables body \\ args
freeVariables (Lambda args body) = freeVariables body \\ args
freeVariables (Var k) = [k]
freeVariables (BinaryOp _ e1 e2) = union (freeVariables e1) (freeVariables e2)
freeVariables (UnaryOp _ e) = freeVariables e
freeVariables (FunApp _ args) =
  mapMaybe (\k -> case k of Var x -> Just x; _ -> Nothing) args
