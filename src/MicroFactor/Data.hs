{-# LANGUAGE DeriveFunctor #-}

module MicroFactor.Data
    ( MicroFactorInstruction (..)
    , MicroFactorOperator (..)
    , InstructionRef (..)
    , Nested
    ) where

data MicroFactorInstruction r
    = Comment String
    | LiteralValue Word
    | LiteralAddress Word -- Port address
    | LiteralString String
    | Wrapper (MicroFactorInstruction r)
    | Call r
    | Operator MicroFactorOperator
    deriving (Eq, Show, Functor)

-- could have been part of MicroFactorInstruction
-- separate definition makes monad implementation simpler
data MicroFactorOperator
    = Execute
    | Debugger
    | Exit
    | LiteralFalse
    | LiteralTrue
    | LogicNot
    | LogicNor
    | LogicLt
    | LogicGt
    | LogicXor -- Neq
    | LogicNand
    | LogicAnd
    | LogicXnor -- Eq
    | LogicLte -- Impl
    | LogicGte
    | LogicOr

    | StackNip
    | StackDrop
    | StackPick
    | StackDuplicate -- pick 1
    | StackOver -- pick 2
    | StackTuck
    | StackRoll
    | StackSwap -- roll 1
    | StackRotate -- roll 2

    | ArithAdd
    | ArithSub
    | ArithMul
    | ArithDiv
    | ArithMod
    | ArithDivmod
    | ArithAbs
    | ArithMax
    | ArithMin
{-
    MICROFACTOR_INSTRUCTION_PUSH_OWN_PORTADDRESS,
    MICROFACTOR_INSTRUCTION_SEND_SERIAL,
    MICROFACTOR_INSTRUCTION_READ_FIELD,
    MICROFACTOR_INSTRUCTION_WRITE_FIELD,
    MICROFACTOR_INSTRUCTION_INPUT,
    MICROFACTOR_INSTRUCTION_OUTPUT,
    MICROFACTOR_INSTRUCTION_RANDOM,
-}
    | Send
    | Yield
    | YieldDelay
    | YieldInput
    | ThreadStart
    | ThreadPass
    deriving (Eq, Show)

instance Applicative MicroFactorInstruction where
    pure = return
    fi <*> a = fi >>= (`fmap` a)

instance Monad MicroFactorInstruction where
    return = Call
    i >>= f = case i of
        Call c -> f c
        Wrapper x -> Wrapper $ x >>= f
        Comment c -> Comment c
        LiteralValue v -> LiteralValue v
        LiteralAddress a -> LiteralAddress a
        LiteralString s -> LiteralString s
        Operator o -> Operator o

instance Foldable MicroFactorInstruction where
    foldr f z (Call c)    = f c z
    foldr f z (Wrapper c) = foldr f z c
    foldr _ z _           = z

instance Traversable MicroFactorInstruction where
    traverse f (Call c)    = Call <$> f c
    traverse f (Wrapper c) = Wrapper <$> traverse f c
    traverse _ x           = pure (case x of
        Comment c -> Comment c
        LiteralValue v -> LiteralValue v
        LiteralAddress a -> LiteralAddress a
        LiteralString s -> LiteralString s
        Operator o -> Operator o)

--------------------------------------------------------------------------------

class InstructionRef a where
    makeRef :: [MicroFactorInstruction a] -> a
    resolveRef :: a -> [MicroFactorInstruction a]

newtype Nested = Nested { unnest :: [MicroFactorInstruction Nested] } deriving Show
instance InstructionRef Nested where
    makeRef = Nested
    resolveRef = unnest