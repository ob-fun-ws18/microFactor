{-# LANGUAGE NamedFieldPuns, RankNTypes, DeriveFunctor #-}

module MicroFactor.Interpreter
    ( ThreadInterpreter
    , interpret
    , InterpreterError (..)
    , ThreadState (..)
    , TaggedValue (..)
    , InterpreterResult (..)
    , Thread (..)
    , newThread
    , showValue
    ) where

import MicroFactor.Data

data InterpreterError
    = StackOverflow
    | StackUnderflow
    | InvalidExecutionToken
    | CouldNotSuspend
    | InfiniteLoop
    | TypeError String
    deriving (Eq, Show)

data ThreadState
    = Running -- or done, if nothing on return stack
    | Delayed
    | Yielded
    | Waiting
    deriving (Eq, Show)

data TaggedValue r
    = Boolean Bool
    | Integer Word
    | PortAddress Word
    | Instructions r
    deriving (Eq, Show, Functor)

data Thread r = Thread {
    stopPending :: Bool,
    stopCritical :: Bool,
    state :: ThreadState,
    dataStack :: [TaggedValue r],
    returnStack :: [r]
} deriving Show

newThread :: Thread r
newThread = Thread False False Running [] []

data InterpreterResult r a = InterpreterResult {
    interpreterOutput :: [String],
    interpreterThread :: Thread r,
    interpreterValue :: Either InterpreterError a
} deriving Show

instance Functor (InterpreterResult r) where
    fmap f r = r { interpreterValue = f <$> interpreterValue r }

newtype ThreadInterpreter r a = ThreadInterpreter {
    runInterpreter :: Thread r -> InterpreterResult r a
} deriving Functor

-- instance Functor (ThreadInterpreter r) where
--    fmap f = ThreadInterpreter . fmap (fmap f) . runInterpreter

instance Applicative (ThreadInterpreter r) where
    pure = return
    fi <*> a = fi >>= (`fmap` a)

instance Monad (ThreadInterpreter r) where
    return x = ThreadInterpreter $ \t -> InterpreterResult [] t (Right x)
    -- (>>=) :: forall r a b. ThreadInterpreter r a -> (a -> ThreadInterpreter r b) -> ThreadInterpreter r b
    m >>= f = ThreadInterpreter $ step . runInterpreter m
        where
            -- step :: InterpreterResult r a -> InterpreterResult r b
            step (InterpreterResult o1 t1 (Right x1)) =
                let InterpreterResult o2 t2 x2 = runInterpreter (f x1) t1
                in InterpreterResult (o1 <> o2) t2 x2
            -- step interpreterLeftResult = interpreterLeftResult
            step (InterpreterResult o t (Left e)) = InterpreterResult o t (Left e)
    fail e = ThreadInterpreter $ \t -> InterpreterResult [] t (Left $ TypeError e)

--------------------------------------------------------------------------------

-- pushData :: TaggedValue r -> Thread r -> Thread r
-- pushData x (t@Thread {dataStack}) = t { dataStack = x:dataStack }
pushData :: TaggedValue r -> ThreadInterpreter r ()
pushData x = ThreadInterpreter $ \t@Thread {dataStack} ->
    InterpreterResult [] t { dataStack = x:dataStack } (Right ())

popData :: ThreadInterpreter r (TaggedValue r)
popData = ThreadInterpreter $ \t -> case t of
    Thread { dataStack = [] } -> InterpreterResult [] t (Left StackUnderflow)
    Thread { dataStack = d:ds } -> InterpreterResult [] t { dataStack = ds } (Right d)

popDataBool :: ThreadInterpreter r Bool
popDataBool = do
    val <- popData
    case val of
        Boolean b -> return b
        Integer w -> return (w /= 0)
        x -> fail $ "expected Boolean (or Integer)" -- "but got" ++ show x

popDataInt :: ThreadInterpreter r Word
popDataInt = do
    val <- popData
    case val of
        Integer w -> return w
        _ -> fail $ "expected Integer"

interpretCompare :: (forall c. Ord c => c -> c -> Bool) -> ThreadInterpreter r ()
interpretCompare comp = do
    b <- popData
    a <- popData
    case (a, b) of
        (Boolean a, Boolean b) -> pushData $ Boolean $ comp a b
        (Integer a, Integer b) -> pushData $ Boolean $ comp a b
        (PortAddress a, PortAddress b) -> pushData $ Boolean $ comp a b
        (Instructions _, _) -> fail "cannot compare Instructions"
        (_, Instructions _) -> fail "cannot compare Instructions"
        _ -> fail "can only compare values of same type" -- tried show a with show b

interpretBinaryBool :: (Bool -> Bool -> Bool) -> ThreadInterpreter r ()
interpretBinaryBool op = do
    b <- popDataBool
    a <- popDataBool
    pushData $ Boolean $ op a b

interpretStack :: (forall a. [a] -> Maybe [a]) -> ThreadInterpreter r ()
interpretStack manip = ThreadInterpreter $ \t@Thread {dataStack} ->
    case manip dataStack of
        Just dataStack -> InterpreterResult [] t {dataStack} (Right ())
        Nothing -> InterpreterResult [] t (Left StackUnderflow)


interpretBinaryInt :: (Word -> Word -> Word) -> ThreadInterpreter r ()
interpretBinaryInt op = do
    b <- popDataInt
    a <- popDataInt
    pushData $ Integer $ op a b

--------------------------------------------------------------------------------

interpret :: InstructionRef a => [MicroFactorInstruction a] -> Thread a -> InterpreterResult a ()
interpret = runInterpreter . interpreter

interpreter :: InstructionRef a => [MicroFactorInstruction a] -> ThreadInterpreter a ()
interpreter [] = ThreadInterpreter $ \t -> case t of
    Thread { returnStack = [] } -> InterpreterResult [] t (Right ())
    Thread { returnStack = r:rs } -> runInterpreter (interpreter $ resolveRef r) t { returnStack = rs }
interpreter (Operator Exit:_) = interpreter []
-- interpreter (Call r:is) = interpreter (resolveRef r) >> interpreter is
interpreter (Call r:is) = ThreadInterpreter $ \t@Thread { returnStack } ->
    runInterpreter (interpreter $ resolveRef r) t { returnStack = makeRef is:returnStack }
interpreter (Operator Execute:is) = popData >>= \(Instructions r) -> interpreter (Call r:is)
interpreter (i:is) = (\() -> interpreter is) =<< case i of
    Comment _ -> return ()
    LiteralValue w -> pushData (Integer w)
    LiteralAddress w -> pushData (PortAddress w)
    LiteralString s -> ThreadInterpreter $ \t -> InterpreterResult [s] t (Right ())
    Wrapper i -> pushData $ Instructions (case i of
        Call is -> is
        _ -> makeRef [i])
    Operator Debugger -> return () -- TODO: output
    Operator LiteralFalse -> pushData (Boolean False)
    Operator LiteralTrue -> pushData (Boolean True)
    Operator LogicNot -> popDataBool >>= pushData . Boolean . not
    Operator LogicNor -> interpretBinaryBool $ \a b -> not (a || b)
    Operator LogicLt -> interpretCompare (<)
    Operator LogicGt -> interpretCompare (>)
    Operator LogicXor -> interpretCompare (/=) -- Neq
    Operator LogicNand -> interpretBinaryBool $ \a b -> not (a && b)
    Operator LogicAnd -> interpretBinaryBool (&&)
    Operator LogicXnor -> interpretCompare (==) -- Eq
    Operator LogicLte -> interpretCompare (<=) -- Impl
    Operator LogicGte -> interpretCompare (>=)
    Operator LogicOr -> interpretBinaryBool (||)
    Operator StackNip -> interpretStack $ \s -> case s of d:_:ds -> Just (d:ds); _ -> Nothing
    Operator StackDrop -> popData >> return ()
    -- Operator StackPick
    Operator StackDuplicate -> interpretStack $ \s -> case s of d:ds -> Just (d:d:ds); _ -> Nothing -- pick 0
    Operator StackOver -> interpretStack $ \s -> case s of d1:d2:ds -> Just (d2:d1:d2:ds); _ -> Nothing -- pick 1
    Operator StackTuck -> interpretStack $ \s -> case s of d1:d2:ds -> Just (d1:d2:d1:ds); _ -> Nothing
    -- Operator StackRoll
    Operator StackSwap -> interpretStack $ \s -> case s of d1:d2:ds -> Just (d2:d1:ds); _ -> Nothing -- roll 1
    Operator StackRotate -> interpretStack $ \s -> case s of d1:d2:d3:ds -> Just (d3:d1:d2:ds); _ -> Nothing -- roll 2

    Operator ArithAdd -> interpretBinaryInt (+)
    Operator ArithSub -> interpretBinaryInt (-)
    Operator ArithMul -> interpretBinaryInt (*)
    Operator ArithDiv -> interpretBinaryInt div
    Operator ArithMod -> interpretBinaryInt mod
    -- Operator ArithDivmod -> interpretBinaryInt
    Operator ArithAbs -> pushData . Integer =<< abs <$> popDataInt
    Operator ArithMax -> interpretBinaryInt max
    Operator ArithMin -> interpretBinaryInt min

    Operator Send -> do
        val <- popData
        ThreadInterpreter $ \t -> InterpreterResult [showValue val] t (Right ())

showValue :: TaggedValue a -> String
showValue (Boolean b) = show b
showValue (Integer w) = show w
showValue (PortAddress w) = '#':show w
showValue (Instructions _) = "<Code>"