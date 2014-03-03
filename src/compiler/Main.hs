module Main where
import Control.Monad.Reader
import Control.Monad.State
import Emit
import Compiler
import Parser

main = Emit.writeFile
       (execState 
           (runCompilerState $ compileTopLevel $ parseAST
            "function foo(b) = 2*b; let a = 5;let c = foo(3); print(c); print(foo(2));")
           initState)
       "c:\\users\\utente\\desktop\\out.pyc"
