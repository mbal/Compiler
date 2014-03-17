{-# OPTIONS_GHC -Wall #-}

-- This module contains the main function.
module Main where
import Control.Monad.State (execState)

import Emit (writeFile)
import Compiler (compileTopLevel, initState)
import Parser (getASTFromFile)
import Types (runCompilerState)

desugar :: a -> a
desugar = id

main :: IO ()
main = do
  ast <- getASTFromFile "ex3.x"
  let --definition = Def.pass ast
    desugr = desugar ast
    compiled = compileTopLevel desugr
    state = initState
  Emit.writeFile (execState (runCompilerState compiled) state)
      "c:\\users\\utente\\desktop\\out.pyc"
