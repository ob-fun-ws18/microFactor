-- | Common data structures
module MicroFactor.Data
    ( TaggedValue (..)
    , MicroFactorInstruction (..)
    , MicroFactorOperator (..)
    , InstructionRef (..)
    , Nested
    ) where

import Control.Monad (ap)

-- | The values that can be used in microfactor computations
data TaggedValue r
    = Boolean Bool
    | Integer Word
    | PortAddress Word
    | Instruction (MicroFactorInstruction r)
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- using `Instruction r` in `TaggedValue` and then referring to `TaggedValue (MicroFactorInstruction r)` everywhere
-- would have allowed to use the derived `fmap` instead of `mapInstruction`
-- but it would have lost the meaning of `TaggedValue`
mapInstruction :: (MicroFactorInstruction a -> MicroFactorInstruction b) -> TaggedValue a -> TaggedValue b
mapInstruction _ (Boolean b) = Boolean b
mapInstruction _ (Integer w) = Integer w
mapInstruction _ (PortAddress w) = PortAddress w
mapInstruction f (Instruction i) = Instruction $ f i

-- | A data type for instructions to be interpreted.
-- It is generic over the type of values to be called
data MicroFactorInstruction r
    = Comment String -- ^ code annotation, does nothing
    | LiteralString String -- ^ debug output
    | Literal (TaggedValue r) -- ^ put value on stack
    | Call r -- ^ call other things
    | Operator MicroFactorOperator -- ^ operate on the stack(s)
    deriving (Eq, Functor, Foldable, Traversable)

-- could have been part of MicroFactorInstruction
-- separate definition makes monad implementation simpler
-- | The basic opcodes of the language
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
    -- Yield
    -- YieldDelay
    -- YieldInput
    -- ThreadStart
    -- ThreadPass
    -- OwnPortAddress
    -- ReadField
    -- WriteField
    -- Input
    -- Output
    -- Random
    deriving (Eq, Ord, Enum, Bounded)

instance Applicative MicroFactorInstruction where
    pure = return
    (<*>) = ap

-- not truly useful, but simplifies implementation of `resolveNames`
instance Monad MicroFactorInstruction where
    return = Call
    i >>= f = case i of
        Call c -> f c
        LiteralString s -> LiteralString s
        Literal x -> Literal $ mapInstruction (>>= f) x
        Comment c -> Comment c
        Operator o -> Operator o

--------------------------------------------------------------------------------

-- | A typeclass for references to instructions
-- so that the interpreter might be generic
class InstructionRef a where
    -- | create a reference for anonymous blocks
    makeRef :: [MicroFactorInstruction a] -> a
    -- | get the referenced list of instruction
    resolveRef :: a -> [MicroFactorInstruction a]
    -- | refer to the instructions by name, instead of treating them as anonymous
    refName :: a -> String
    refName _ = ""

{-
-- flexible and undecidable instance :-)
instance InstructionRef a => Show a where
    show ref = case refName ref of
        "" -> show $ resolveRef ref
        name -> name
-}

-- | the most simple implementation of `InstructionRef`
-- while avoiding infinite types
newtype Nested = Nested { unnest :: [MicroFactorInstruction Nested] } deriving Show
instance InstructionRef Nested where
    makeRef = Nested
    resolveRef = unnest

--------------------------------------------------------------------------------
-- | pretty-printing code
-- (also used as language definition, for resolving names to operators in the parser)
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

-- | pretty-printing code
instance InstructionRef r => Show (MicroFactorInstruction r) where
    show (Comment c) = "(* " ++ c ++ " *)"
    show (LiteralString s) = "\"" ++ s ++ "\"" -- TODO: escaping
    show (Literal (Boolean True)) = "true"
    show (Literal (Boolean False)) = "false"
    show (Literal (Integer w)) = show w
    show (Literal (PortAddress w)) = "#" ++ show w
    show (Literal (Instruction (Call ref))) | isAnonymous ref = "(" ++ showList (resolveRef ref) ")"
    show (Literal (Instruction i)) = "'" ++ show i
    show (Call ref) | isAnonymous ref = error "call to anonymous function"
                    | otherwise = refName ref
    show (Operator o) = show o

    showList rs = (++) $ unwords $ map show rs

isAnonymous :: InstructionRef a => a -> Bool
isAnonymous = null . refName
