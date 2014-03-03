module Types where
import Data.Word
import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser (Term(..), BOperation(..), Identifier)
import Bytecode

{- An instruction in the Python Bytecode is:
  * 1 Opcode (1 byte)
  * 1 argument, of 2 bytes
Only instructions with opcode > 90 have an argument -}
data Instruction = Instruction OpCode (Maybe Word16)
                   deriving (Show)

data PyType = PyInt { intvalue :: Integer }
            | PyString { string :: String }
            | PyTuple { elements :: [PyType] }
            | PyNone
            | PyCode { 
                       argcount :: Word32
                       , nlocals :: Word32
                       , stackSize :: Word32
                       , flags :: Word32
                         -- this shouldn't be a PyType, but a PyString,
                         -- however, I don't know if there is a way, other
                         -- than adding another level of indirection (which
                         -- solves every problem)
                       , code :: PyType 
                       , consts :: PyType
                       , varnames :: PyType
                       , names :: PyType
                       }
            deriving (Show, Ord, Eq)

type ConstantID = Word16
type VariableID = Word16
type VarSet = Set.Set Identifier

emptyVarSet = Set.empty

data CState = CState {
  cMagic :: Word32
  , cBlock :: !CodeBlock
  , cFilename :: String
  } deriving (Show)

data VarType = Global | Fast | Deref | Name

data CodeBlock = CodeBlock {
  block_instructions :: [Instruction]
  , block_constants :: Map.Map PyType Word16
  , block_names :: Map.Map Identifier Word16 -- globals
  , block_varnames :: Map.Map Identifier Word16 -- python's locals
  , block_freevars :: Map.Map Identifier Word16 -- unused for now
  , block_cellvars :: Map.Map Identifier Word16 -- "" ""
  , block_name :: String -- name of the block (i.e. name of the function)
  , nextConstantID :: ConstantID
  , nextVariableID :: VariableID
  } deriving (Show)
