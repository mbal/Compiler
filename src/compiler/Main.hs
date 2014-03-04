module Main where
import Control.Monad.Reader
import Control.Monad.State
import Emit
import Compiler
import Parser

main = Emit.writeFile
       (execState 
           (runCompilerState $ compileTopLevel $ parseAST
            "function foo() = 5*5-4*3*6; print(foo()); function bar() = 5*5-4*3*6; print(bar());")
           initState)
       "c:\\users\\utente\\desktop\\out.pyc"
