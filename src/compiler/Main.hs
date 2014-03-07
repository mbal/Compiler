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
main = Emit.writeFile
       (let ast = parseAST "function f(a) = 3*a; print(f(3));"
            annotatedAST = id ast
            desugared = desugar annotatedAST
            compiled = compileTopLevel desugared in
        execState (runCompilerState compiled) initState)
       "c:\\users\\utente\\desktop\\out.pyc"
