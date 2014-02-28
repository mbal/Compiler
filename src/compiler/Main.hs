module Main where
import Control.Monad.Reader
import Control.Monad.State
import Emit
import Compiler
import Parser

main = Emit.writeFile
       (execState 
           (runCompilerState $ compileTopLevel $ parseAST
            "print(4+4); print(4);")
           initState)
       "c:\\users\\utente\\desktop\\out.pyc"
