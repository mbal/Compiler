module Main where
import Control.Monad.Reader
import Control.Monad.State
import Emit
import Compiler
import Parser

main = Emit.writeFile
       (execState 
           (runCompilerState $ compileTopLevel $ parseAST
            "let a = 1; let b = 7; if a then print(b) else print(8);")
           initState)
       "c:\\users\\utente\\desktop\\out.pyc"
