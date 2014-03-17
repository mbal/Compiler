{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where
import Data.Word
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (MonadState, State, put, get, gets)
import Control.Applicative

import Parser (Identifier)
import Bytecode

{- An instruction in the Python Bytecode is:
  * 1 Opcode (1 byte)
  * 1 argument, of 2 bytes
Only instructions with opcode > 90 have an argument -}
data Instruction = Instruction OpCode (Maybe Word16)
                   deriving (Show)

data AugInstruction = AugInstruction { instr :: Instruction
                                     , index :: Word16 }
                      deriving (Show)

data PyType = PyInt { intvalue :: Integer }
            | PyString { string :: String }
            | PyTuple { elements :: [PyType] }
            | PyNone
            | PyCode { argcount :: Word32
                     , nlocals :: Word32
                     , stackSize :: Word32
                     , flags :: Word32
                      -- this shouldn't be a PyType, but a PyString,
                      -- but I don't know if there is a way.
                     , code :: PyType 
                     , consts :: PyType
                     , varnames :: PyType
                     , cellvars :: PyType
                     , freevars :: PyType
                     , names :: PyType
                     , name :: PyType
                     }
            deriving (Show, Ord, Eq)

type ConstantID = Word16
type VariableID = Word16
type LabelID = Word16
type VarSet = Set.Set Identifier

emptyVarSet :: Set.Set Identifier
emptyVarSet = Set.empty

data Result = Yes | No | Unk
            deriving (Show, Eq)

data Definition = FunDcl { numArgs :: Int
                         , vtype :: VarType
--                         , dname :: String
                         , isPrime :: Bool
                         , higherOrder :: Result}
                  | VarDcl {  }
                  deriving (Show)

newtype CompilerState a = CompilerState { runCompilerState :: State CState a }
    deriving (Functor, Applicative, Monad, MonadState CState)

modifyBlockState :: (CodeBlock -> CodeBlock) -> CompilerState ()
modifyBlockState f = do
  state <- getBlockState id
  setBlockState $ f state

getBlockState :: (CodeBlock -> a) -> CompilerState a
getBlockState f = gets (f . cBlock)

setBlockState :: CodeBlock -> CompilerState ()
setBlockState newState = do
  oldState <- get
  put $ oldState { cBlock = newState }

data CState = CState {
  cMagic :: Word32
  , cBlock :: !CodeBlock
  , cFilename :: String
  , cDefinitions :: Map.Map String Definition
  } deriving (Show)

data VarType = Global | Fast | Deref | Name
             deriving (Show, Ord, Eq)

data Function = Function { fun_numArgs :: Int
                         , fun_isPrime :: Bool
                         , fun_location :: Word16
                         -- actually, a function is always either global or fast
                         , fun_type :: VarType
                         }
                deriving (Ord, Show)

instance Eq Function where
  (==) f1 f2 = (fun_location f1) == (fun_location f2)

data CodeBlock = CodeBlock {
  block_instructions :: [AugInstruction]
  , block_constantsMap :: Map.Map PyType Word16
  , block_constants :: [PyType]
  , block_names :: Map.Map Identifier Word16 -- globals
  , block_functions :: Map.Map Identifier Function 
  -- basically, other globals but reserved for functions. Not needed to python
  -- but may be useful for other purposes.
  , block_varnames :: Map.Map Identifier Word16 -- python's locals
  , block_freevars :: Map.Map Identifier Word16
  , block_cellvars :: Map.Map Identifier Word16
  , block_name :: String -- name of the block (i.e. name of the function)
  , block_labelMap ::  Map.Map LabelID Word16
  , block_instructionOffset :: Word16
  , block_flags :: Word32
  , block_labelsForNextInstruction :: [LabelID]
  , block_nextNameID :: VariableID
  , block_nextLabelID :: LabelID
  , block_nextConstantID :: ConstantID
  , block_nextVariableID :: VariableID
  } deriving (Show)
