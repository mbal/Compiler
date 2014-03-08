{-# OPTIONS_GHC -Wall #-}
module Main where
import Control.Monad.State
import Emit
import Compiler
import Parser
import Definition

desugar :: a -> a
desugar = id

main :: IO ()
main = do
  ast <- getASTFromFile "ex.x"
  let annast = id ast
      desugr = desugar annast
      compiled = compileTopLevel desugr
  Emit.writeFile (execState (runCompilerState compiled) initState)
    "c:\\users\\utente\\desktop\\out.pyc"
