module Main where
import Control.Monad.Reader
import Control.Monad.State
import Emit
import Compiler
import Parser

main = Emit.writeFile
       (execState 
           (runCompilerState $ compileTopLevel $ parseAST
            "function fact(n) = if n then n * fact(n-1) else 1; print(fact(14));")
           initState)
       "c:\\users\\utente\\desktop\\out.pyc"
