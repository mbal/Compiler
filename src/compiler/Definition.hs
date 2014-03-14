{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Definition (pass) where
import Control.Monad.State
import Control.Monad.Reader()
import Control.Applicative (Applicative)
import Data.List
import qualified Data.Map as Map

import Types
import Parser

data Result = Yes | No | Unk
            deriving (Show, Eq)
data CollectDef = CollectDef { definitions :: Map.Map String Definition
                             , equivalences :: Map.Map String [String]
                             }
                deriving (Show)

newtype Def a = Def { runDef :: State CollectDef a }
              deriving (Functor, Applicative, Monad, MonadState CollectDef) 

zeroState :: CollectDef
zeroState = CollectDef { definitions = Map.empty
                       , equivalences = Map.empty
                       }

pass :: [Term] -> Map.Map String Definition
pass ast = definitions (execState (runDef (collectDefinitions ast)) zeroState)

collectDefinitions :: [Term] -> Def ()
collectDefinitions ts = do
  mapM_ p1 ts
  propagateDefinitions

propagateDefinitions :: Def ()
propagateDefinitions = do
  graph <- gets equivalences
  defs <- gets definitions
  let v = inferTypes defs (Map.toList graph)
  modify $ \s -> s { definitions = v }

findFirstCommon :: Eq a => [a] -> [a] -> Maybe a
findFirstCommon l1 l2 =
  let int = intersect l1 l2 in
      if length int > 0 then Just (int !! 0) else Nothing

keys :: [(k, v)] -> [k]
keys [] = []
keys ((v, _):cs) = v : (keys cs)

inferTypes :: Map.Map String Definition -> [(String, [String])] ->
              Map.Map String Definition
inferTypes d [] = d
inferTypes definitions lst@((_, eqvs):cs) =
  let maybeDef = findFirstCommon (keys lst) (Map.keys definitions) in
  case maybeDef of
    Just defName -> 
      -- get actual definition
      let defm = Map.lookup defName definitions in
      case defm of
        Just def ->
          -- propagation phase
          let df = putAllInMap eqvs def definitions in
          inferTypes df cs
    Nothing -> error $ "can't infer!"

putAllInMap [] _ r = r
putAllInMap (k:ks) value oldMap =
  putAllInMap ks value (Map.insert k value oldMap)

p1 :: Term -> Def ()
p1 (Defun fname args _) =
  do oldDef <- gets definitions
     let fobj = (FunDcl { numArgs = length args
                        , vtype = Global
                        , isPrime = False })
     modify $ \s -> s { definitions = Map.insert fname fobj oldDef }

p1 ins@(Let lname expr) =
  case isFunction expr of
    Yes -> -- here, we are sure that the expression denotes a function.
           -- this happens in two situations: special forms and lambdas.
      do oldDef <- gets definitions
         modify $ \s -> s { definitions = Map.insert lname (createFunObj ins) oldDef }
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
           Just _ -> -- we already have some name equivalent to this
             -- binding, so we have to update the table.
             modify $ \s -> s { equivalences = Map.adjust
                                               (\x -> lname : x)
                                               (getEquiv expr) eqvs } 

p1 _ = do return ()

getEquiv :: Term -> Identifier
getEquiv (Var v) = v
getEquiv _ = error "getEquiv"

createFunObj :: Term -> Definition
createFunObj (Let _ expr) =
  FunDcl { numArgs = computeNumArgs expr,
           vtype = Global,
           isPrime = computeIsPrime expr }
                          
isFunction :: Term -> Result
isFunction (UnaryOp Prime _) = Yes
isFunction (Var _) = Unk
isFunction (Lambda _ _) = Yes
isFunction (SpecialForm _ _) = Yes
isFunction _ = No

computeNumArgs :: Term -> Int
computeNumArgs (UnaryOp Prime _) = 0
computeNumArgs (SpecialForm _ _) = 1 -- not very nice.

computeIsPrime :: Term -> Bool
computeIsPrime (UnaryOp Prime _) = True
computeIsPrime _ = False
