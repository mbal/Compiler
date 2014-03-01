module Main where
import Control.Monad.Reader
import Control.Monad.State
import Emit
import Compiler
import Parser

main = Emit.writeFile
       (execState 
           (runCompilerState $ compileTopLevel $ parseAST
            "let a = 4+4; let b = a + 1; print(b);")
           initState)
       "c:\\users\\utente\\desktop\\out.pyc"
