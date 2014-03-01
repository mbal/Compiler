module Main where
import Control.Monad.Reader
import Control.Monad.State
import Emit
import Compiler
import Parser

main = Emit.writeFile
       (execState 
           (runCompilerState $ compileTopLevel $ parseAST
            "let a = 4+4*2/43*2+21; print(a);")
           initState)
       "c:\\users\\utente\\desktop\\out.pyc"
