{-# LANGUAGE DeriveFunctor #-}

module MicroFactor.Data
    ( MicroFactorInstruction (..)
    , MicroFactorOperator (..)
    , InstructionRef (..)
    , Nested
    ) where

-- data type for instructions to be interpreted
-- generic over the type of values to be called
data MicroFactorInstruction r
    = Comment String
    | LiteralValue Word
    | LiteralAddress Word -- Port address
    | LiteralString String
    | Wrapper (MicroFactorInstruction r)
    | Call r
    | Operator MicroFactorOperator
    deriving (Eq, Functor)

-- could have been part of MicroFactorInstruction
-- separate definition makes monad & traversable implementations simpler
data MicroFactorOperator
    = Execute
    | Debugger
    | ControlIte
    | ControlIf
    | ControlDo
    | ControlWhen
    | ControlUnless
    | ControlForever
    | ControlLoop
    | ControlWhile
    | ControlUntil
    | ControlDip
    | ControlKeep

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

    | Send
    -- | Yield
    -- | YieldDelay
    -- | YieldInput
    -- | ThreadStart
    -- | ThreadPass
    -- | OwnPortAddress
    -- | ReadField
    -- | WriteField
    -- | Input
    -- | Output
    -- | Random
    deriving (Eq, Ord, Enum, Bounded)

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

-- a typeclass for references to instructions
-- so that the interpreter might be generic
class InstructionRef a where
    -- used to create a reference for anonymous blocks
    makeRef :: [MicroFactorInstruction a] -> a
    -- used to get the referenced list of instruction
    resolveRef :: a -> [MicroFactorInstruction a]
    -- used to refer to the instructions by name, instead of treating them as anonymous
    refName :: a -> String
    refName _ = ""

{-
-- flexible and undecidable instance :-)
instance InstructionRef a => Show a where
    show ref = case refName ref of
        "" -> show $ resolveRef ref
        name -> name
-}

-- the most simple implementation of `InstructionRef`
-- while avoiding infinite types
newtype Nested = Nested { unnest :: [MicroFactorInstruction Nested] } deriving Show
instance InstructionRef Nested where
    makeRef = Nested
    resolveRef = unnest

--------------------------------------------------------------------------------

instance Show MicroFactorOperator where
    show Execute        = "execute"
    show Debugger       = "debugger"
    show ControlIte     = "?"
    show ControlIf      = "if"
    show ControlDo      = "do"
    show ControlWhen    = "when"
    show ControlUnless  = "unless"
    show ControlForever = "forever"
    show ControlLoop    = "loop"
    show ControlWhile   = "while"
    show ControlUntil   = "until"
    show ControlDip     = "dip"
    show ControlKeep    = "keep"
    show LiteralFalse   = "false"
    show LiteralTrue    = "true"
    show LogicNot       = "not"
    show LogicNor       = "nor"
    show LogicLt        = "<"
    show LogicGt        = ">"
    show LogicXor       = "<>"
    show LogicNand      = "nand"
    show LogicAnd       = "and"
    show LogicXnor      = "="
    show LogicLte       = "->"
    show LogicGte       = ">="
    show LogicOr        = "or"
    show StackNip       = "nip"
    show StackDrop      = "drop"
    show StackPick      = "pick"
    show StackDuplicate = "dup"
    show StackOver      = "over"
    show StackTuck      = "tuck"
    show StackRoll      = "roll"
    show StackSwap      = "swap"
    show StackRotate    = "rot"
    show ArithAdd       = "+"
    show ArithSub       = "-"
    show ArithMul       = "*"
    show ArithDiv       = "/"
    show ArithMod       = "mod"
    show ArithDivmod    = "/mod"
    show ArithAbs       = "abs"
    show ArithMax       = "max"
    show ArithMin       = "min"
    show Send           = "."
    -- show Yield          = ""
    -- show YieldDelay     = ""
    -- show YieldInput     = ""
    -- show ThreadStart    = ""
    -- show ThreadPass     = ""
    -- show _ = ""

instance InstructionRef r => Show (MicroFactorInstruction r) where
    show (Comment c) = "(* " ++ c ++ " *)"
    show (LiteralValue w) = show w
    show (LiteralAddress w) = "#" ++ show w
    show (LiteralString s) = "\"" ++ s ++ "\"" -- TODO: escaping
    show (Wrapper (Call ref)) | isAnonymous ref = "(" ++ showList (resolveRef ref) ")"
    show (Wrapper i) = "'" ++ show i
    show (Call ref) | isAnonymous ref = error "call to anonymous function"
                    | otherwise = refName ref
    show (Operator o) = show o

    showList rs = (++) $ unwords $ map show rs

isAnonymous :: InstructionRef a => a -> Bool
isAnonymous = null . refName
