module StackSize (computeStackSize) where
import Bytecode
import Compiler
import Test.QuickCheck

{- since we don't have jumps yet, the necessary stack size can be computed
easily, using Kadane's algorithm

* if there are no loops in the code, we can take the maximum sum subsequence
For example, if our instructions have the following "stack profile"
(where +x means push x cells and -x means pop x cells):
[1, 2, -1, 3, 2, -1, -1, -1] ==> max stack depth = 1 + 2 - 1 + 3 + 2 = 7
This case is easy. The Kadane's algorithm takes care of everything
(which, as a nice plus, it's fast).
* the introduction of loops in the bytecode makes everything more difficult.
If we don't make the assumption that cycles have no net effect on the
stack, it's even impossible (because, at the level of the bytecode, we
don't know how many iterations we will do)
-}

stackSize = maxSumSubseq
  
maxSumSubseq l = maxSumSubseq' l 0 0
  where maxSumSubseq' [] s _ = s
        maxSumSubseq' (x:xs) sofar endhere = maxSumSubseq' xs s e
          where e = max 0 (endhere + x)
                s = max sofar e

maxSumSubseq' :: [Integer] -> (Integer, Integer)
maxSumSubseq' l = foldl helper (0, 0) l
  where helper (a, b) x = (e, s)
          where e = max (a + x) 0
                s = max b e

maxSumSubseq2 = snd . maxSumSubseq'

-- maxSumSubseq and maxSumSubseq2 are equivalent, even in the performances

sumEmptyIsZero = maxSumSubseq [] == 0
sumGreaterThanMaximumValue :: [Integer] -> Property
sumGreaterThanMaximumValue x = not (null x) ==> (maxSumSubseq x) >= (maximum x)

resultsEqual x = not (null x) ==> (maxSumSubseq x) == (maxSumSubseq2 x)

-- some more specific properties would be nice, but what can we say?

computeStackSize x = stackSize (map (fromIntegral . opcodeStackEffect) x)

opcodeStackEffect :: Instruction -> Int
opcodeStackEffect (Instruction opcode arg) =
  case opcode of
    POP_TOP -> -1
    ROT_TWO -> 0
    ROT_THREE -> 0
    DUP_TOP -> 1
    DUP_TOP_TWO -> 10
    NOP -> 0
    UNARY_POSITIVE -> 0
    UNARY_NEGATIVE -> 0
    UNARY_NOT -> 0
    UNARY_INVERT -> 0

    BINARY_POWER -> -1
    BINARY_MULTIPLY -> -1
    BINARY_MODULO -> -1
    BINARY_ADD -> -1
    BINARY_SUBTRACT -> -1
    BINARY_SUBSCR -> -1
    BINARY_FLOOR_DIVIDE -> -1
    BINARY_TRUE_DIVIDE -> -1
    INPLACE_FLOOR_DIVIDE -> -1
    INPLACE_TRUE_DIVIDE -> -1
    STORE_MAP -> -2
    INPLACE_ADD -> -1
    INPLACE_SUBTRACT -> -1
    INPLACE_MULTIPLY -> -1
    INPLACE_MODULO -> -1
    STORE_SUBSCR -> -3
    DELETE_SUBSCR -> -2
    BINARY_LSHIFT -> -1
    BINARY_RSHIFT -> -1
    BINARY_AND -> -1
    BINARY_XOR -> -1
    BINARY_OR -> -1
    INPLACE_POWER -> -1
    GET_ITER -> 0
    STORE_LOCALS -> 10
    PRINT_EXPR -> -1
    PRINT_ITEM -> -1
    PRINT_NEWLINE -> 0
    
    --LOAD_BUILD_CLASS -> 10
    --YIELD_FROM -> 10
  
    INPLACE_LSHIFT -> -1
    INPLACE_RSHIFT -> -1
    INPLACE_AND -> -1
    INPLACE_XOR -> -1
    INPLACE_OR -> -1
  
    BREAK_LOOP -> 0
    WITH_CLEANUP -> -1 -- -1 sometimes more
    RETURN_VALUE -> -1
    IMPORT_STAR -> -1
    YIELD_VALUE -> 0
    POP_BLOCK -> 0
    END_FINALLY -> -3
    POP_EXCEPT -> 10
    -- HAVE_ARGUMENT -> 90
    STORE_NAME -> -1
    DELETE_NAME -> 0
    UNPACK_SEQUENCE -> hasArg $ \op -> op - 1
    FOR_ITER -> -1
    UNPACK_EX -> 10
    STORE_ATTR -> -2
    DELETE_ATTR -> -1
    STORE_GLOBAL -> -1
    DELETE_GLOBAL -> 0
    LOAD_CONST -> 1
    LOAD_NAME -> 1
    BUILD_TUPLE -> hasArg $ \op -> 1 - op
    BUILD_LIST -> hasArg $ \op -> 1 - op
    BUILD_SET -> hasArg $ \op -> 1 - op
    BUILD_MAP -> 1
    LOAD_ATTR -> 0
    COMPARE_OP -> -1
    IMPORT_NAME -> -1
    IMPORT_FROM -> 1
    JUMP_FORWARD -> 0
    JUMP_IF_FALSE_OR_POP -> 0 -- -1 if jump not taken
    JUMP_IF_TRUE_OR_POP -> 0 -- -1 if jump not taken
    JUMP_ABSOLUTE -> 0
    POP_JUMP_IF_FALSE -> -1
    POP_JUMP_IF_TRUE -> -1
    LOAD_GLOBAL -> 1
    CONTINUE_LOOP -> 0
    SETUP_LOOP -> 0
    SETUP_EXCEPT -> 0
    SETUP_FINALLY -> 0
    LOAD_FAST -> 1
    STORE_FAST -> -1
    DELETE_FAST -> 0
    RAISE_VARARGS -> hasArg $ \op -> -op
    CALL_FUNCTION -> hasArg $ \op -> -nargs(op)
    MAKE_FUNCTION -> hasArg $ \op -> -op
    BUILD_SLICE -> hasArg $ \op -> if op == 3 then -2 else -1
    MAKE_CLOSURE -> hasArg $ \op -> -op - 1
    LOAD_CLOSURE -> 1
    LOAD_DEREF -> 1
    STORE_DEREF -> -1
    DELETE_DEREF -> 10
    CALL_FUNCTION_VAR -> hasArg $ \op -> - nargs(op) - 1
    CALL_FUNCTION_KW -> hasArg $ \op -> -nargs(op) - 1
    CALL_FUNCTION_VAR_KW -> hasArg $ \op -> -nargs(op) - 2
    SETUP_WITH -> 4
    EXTENDED_ARG -> 10
    LIST_APPEND -> -1
    SET_ADD -> -1
    MAP_ADD -> -2
  where hasArg f = case arg of
          Nothing -> error $ "Instruction " ++ (show opcode) ++ ": missing operand"
          Just k -> f $ fromIntegral k
        
nargs o = o `mod` 256 + 2 * o `div` 256
        
