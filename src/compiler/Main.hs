{-# OPTIONS_GHC -Wall #-}
module Main where
import Control.Monad.State
import Emit
import Compiler
import Parser
import Types
import qualified Definition as Def

import Debug.Trace

desugar :: a -> a
desugar = id

main :: IO ()
main = do
  ast <- getASTFromFile "ex2.x"
  traceShow ast $ do
  let definition = Def.pass ast
      desugr = desugar ast
      compiled = compileTopLevel desugr
      state = initState { cDefinitions = definition }
  Emit.writeFile (execState (runCompilerState compiled) state)
    "c:\\users\\utente\\desktop\\out.pyc"
