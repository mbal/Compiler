{-# OPTIONS_GHC -Wall #-}
module Main where
import Control.Monad.State
import Emit
import Compiler
import Parser

main :: IO ()
main = Emit.writeFile
       (execState 
        (runCompilerState $ compileTopLevel $ parseAST
         "function fact(n) = if n == 0 then 1 else n * fact(n-1); print(fact(10));")
         initState)
        "c:\\users\\utente\\desktop\\out.pyc"
