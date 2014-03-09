{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, MultiParamTypeClasses, FlexibleContexts, NoMonomorphismRestriction #-}
module Definition (pass) where
import Data.Maybe (mapMaybe)
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative (Applicative, (<*>))
import Data.Traversable
import Data.List
import qualified Data.Map as Map
import Debug.Trace

import Types
import Parser


data Result = Yes | No | Unk
            deriving (Show, Eq)

data Definition = FunDcl { numArgs :: Int
                         , vtype :: VarType
                         , dname :: String
                         , isPrime :: Bool }
                  | VarDcl { dname :: String }
                  deriving (Show)

data CollectDef = CollectDef { definitions :: [Definition]
                             , equivalences :: Map.Map String [String]
                             }
                deriving (Show)

newtype Def a = Def { runDef :: State CollectDef a }
              deriving (Functor, Applicative, Monad, MonadState CollectDef) 

zeroState = CollectDef { definitions = []
                       , equivalences = Map.empty
                       }

pass = undefined

main = execState
       (runDef
        (collectDefinitions $
         parseAST "let bar= foo; function foo(a,b) = a + b; let baz = bar;"))
       zeroState

collectDefinitions ts = do
  Control.Monad.State.mapM_ p1 ts
  propagateDefinitions

memb2 _ [] = False
memb2 x (y:ys) = if (x == y) then True else (memb2 x ys)

propagateDefinitions = do
  graph <- gets equivalences
  defs <- gets definitions
  let v = inferTypes defs (Map.toList graph)
  traceShow v $ do
  modify $ \s -> s { definitions = [] }

findFirstCommon l1 l2 =
  let int = intersect l1 l2 in
      if length int > 0 then Just (int !! 0) else Nothing

keys [] = []
keys ((v, _):cs) = v : (keys cs)

inferTypes :: [Definition] -> [(String, [String])] -> [Definition]
inferTypes d [] = d
inferTypes definitions lst@((v, eqvs):cs) =
  let maybeDef = (findFirstCommon (keys lst) (map dname definitions)) in
  case maybeDef of
    Just defName -> 
      -- get actual definition
      let defm = lookup defName (zip (map dname definitions) definitions) in
      case defm of
        Just def ->
          -- propagation phase
          let newDefs = map (\x -> def { dname = x }) eqvs in
          inferTypes (newDefs ++ definitions) cs
    Nothing -> error $ "can't infer!"

p1 :: Term -> Def ()
p1 (Defun fname args body) =
  do oldDef <- gets definitions
     let fobj = (FunDcl { numArgs = length args
                           , vtype = Global
                           , dname = fname
                           , isPrime = False })
     modify $ \s -> s { definitions = fobj : oldDef }

p1 ins@(Let lname expr) =
  case isFunction expr of
    Yes -> -- here, we are sure that the expression denotes a function.
           -- this happens in two situations: special forms and prime.
      do oldDef <- gets definitions
         modify $ \s -> s { definitions = (createFunObj ins) : oldDef }
    No -> return ()
    Unk ->
      {- this case is the most interesting: we don't know whether the
         current binding represents a function or not. So, we set up
         a graph, listing all the known equivalences between functions.
         When the first pass is over, we will analyze the graph to infer
         something useful. -}
      do eqvs <- gets equivalences
         case Map.lookup lname eqvs of
           Nothing -> modify $ \s -> s {
             equivalences = Map.insert (getEquiv expr) [lname] eqvs }
           Just ks -> -- we already have some name equivalent to this
             -- binding, so we have to update the table.
             modify $ \s -> s { equivalences = Map.adjust
                                               (\x -> lname : x)
                                               (getEquiv expr) eqvs } 

getEquiv :: Term -> Identifier
getEquiv (Var v) = v
getEquiv _ = undefined

createFunObj :: Term -> Definition
createFunObj (Let name expr) =
  FunDcl { numArgs = computeNumArgs expr,
           vtype = Fast,
           dname = name,
           isPrime = computeIsPrime expr }
                          
isFunction :: Term -> Result
isFunction (UnaryOp Prime _) = Yes
isFunction (Var x) = Unk
isFunction _ = No

computeNumArgs :: Term -> Int
computeNumArgs (UnaryOp Prime _) = 0
computeNumArgs _ = undefined

computeIsPrime :: Term -> Bool
computeIsPrime (UnaryOp Prime _) = True
computeIsPrime _ = False
