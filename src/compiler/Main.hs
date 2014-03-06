module Main where
import Control.Monad.Reader
import Control.Monad.State
import Emit
import Compiler
import Parser

main = Emit.writeFile
       (execState 
           (runCompilerState $ compileTopLevel $ parseAST
            "")
           initState)
       "c:\\users\\utente\\desktop\\out.pyc"
