-- | Runtime and interpreter for MicroFactor bytecode
module MicroFactor.Interpreter
    ( interpret
    , InterpreterError (..)
    , ThreadState (..)
    , InterpreterResult (..)
    , Thread (..)
    , newThread
    , showValue
    ) where

import MicroFactor.Data

-- | Things that can go wrong during interpretion
data InterpreterError
    -- | an attempt was made to pop from the empty (data) stack
    = StackUnderflow
    -- StackOverflow - cannot happen with Haskell's arbitrarily long lists :-)
    -- | an operation required an `Instruction` but was given some other value
    | InvalidExecutionToken
    -- | some other operation did get the wrong operands
    | TypeError String
        -- TODO: CouldNotSuspend
    -- TODO: InfiniteLoop
    deriving (Eq, Show)

-- | Possible state for cooperative threads (not yet implemented)
data ThreadState
    = Running -- or done, if nothing on return stack
    | Delayed
    | Yielded
    | Waiting
    deriving (Eq, Show)

-- | A structure representing a cooperative thread (that might be suspended)
data Thread r = Thread {
    -- TODO: stopPending :: Bool,
    -- TODO: stopCritical :: Bool,
    state :: ThreadState,
    dataStack :: [TaggedValue r],
    returnStack :: [r]
} deriving Show

-- | A fresh new thread, ready to run instructions
newThread :: Thread r
newThread = Thread {- False False -} Running [] []

-- | The result of executing instructions
data InterpreterResult r a = InterpreterResult {
    interpreterOutput :: [String], -- ^ debug and `Send` output from the execution
    interpreterThread :: Thread r, -- ^ the new state of the thread
    interpreterValue :: Either InterpreterError a -- ^ the result value of the current step
} deriving Show

instance Functor (InterpreterResult r) where
    fmap f r = r { interpreterValue = f <$> interpreterValue r }

-- | A monadic interpreter, representing a piece of code that can be executed on a thread
newtype ThreadInterpreter r a = ThreadInterpreter {
    runInterpreter :: Thread r -> InterpreterResult r a
} deriving Functor

-- instance Functor (ThreadInterpreter r) where
--    fmap f = ThreadInterpreter . fmap (fmap f) . runInterpreter

instance Applicative (ThreadInterpreter r) where
    pure = return
    fi <*> a = fi >>= (`fmap` a)

instance Monad (ThreadInterpreter r) where
    return x = ThreadInterpreter \t -> InterpreterResult [] t (Right x)
    (>>=) :: forall r a b. ThreadInterpreter r a -> (a -> ThreadInterpreter r b) -> ThreadInterpreter r b
    m >>= f = ThreadInterpreter $ step . runInterpreter m
        where
            step :: InterpreterResult r a -> InterpreterResult r b
            step (InterpreterResult o1 t1 (Right x1)) =
                let InterpreterResult o2 t2 x2 = runInterpreter (f x1) t1
                in InterpreterResult (o1 <> o2) t2 x2
            -- step interpreterLeftResult = interpreterLeftResult
            step (InterpreterResult o t (Left e)) = InterpreterResult o t (Left e)
    fail e = ThreadInterpreter \t -> interpreterFailResult t $ TypeError e

--------------------------------------------------------------------------------

-- fails and aborts the thread by truncating the return stack (i.e. no "catching")
interpreterFailResult :: Thread r -> InterpreterError -> InterpreterResult r a
interpreterFailResult t e = InterpreterResult [] t { returnStack = [] } $ Left e

-- put a value on the data stack
-- pushData :: TaggedValue r -> Thread r -> Thread r
-- pushData x (t@Thread {dataStack}) = t { dataStack = x:dataStack }
pushData :: TaggedValue r -> ThreadInterpreter r ()
pushData x = ThreadInterpreter \t@Thread {dataStack} ->
    InterpreterResult [] t { dataStack = x:dataStack } (Right ())

-- remove a value from the data stack
popData :: ThreadInterpreter r (TaggedValue r)
popData = ThreadInterpreter \t -> case t of
    Thread { dataStack = [] } -> interpreterFailResult t StackUnderflow
    Thread { dataStack = d:ds } -> InterpreterResult [] t { dataStack = ds } (Right d)

-- read the top value from the data stack
-- getData :: ThreadInterpreter r (TaggedValue r)
-- getData = popData >>= (pushData >>= ($>))

popDataBool :: ThreadInterpreter r Bool
popDataBool = do
    val <- popData
    case val of
        Boolean b -> return b
        Integer w -> return (w /= 0)
        x         -> fail $ "expected Boolean (or Integer)" -- "but got" ++ show x

popDataInstruction :: ThreadInterpreter r (MicroFactorInstruction r)
popDataInstruction = do
    val <- popData
    case val of
        Instruction r -> return r
        _             -> ThreadInterpreter \t -> interpreterFailResult t InvalidExecutionToken

popDataInt :: ThreadInterpreter r Word
popDataInt = do
    val <- popData
    case val of
        Integer w -> return w
        _         -> fail $ "expected Integer"


interpretCompare :: (forall c. Ord c => c -> c -> Bool) -> ThreadInterpreter r ()
interpretCompare comp = do
    b <- popData
    a <- popData
    case (a, b) of
        (Boolean a, Boolean b) -> pushData $ Boolean $ comp a b
        (Integer a, Integer b) -> pushData $ Boolean $ comp a b
        (PortAddress a, PortAddress b) -> pushData $ Boolean $ comp a b
        (Instruction _, _) -> fail "cannot compare Instructions"
        (_, Instruction _) -> fail "cannot compare Instructions"
        _ -> fail "can only compare values of same type" -- tried show a with show b

interpretBinaryBool :: (Bool -> Bool -> Bool) -> ThreadInterpreter r ()
interpretBinaryBool op = do
    b <- popDataBool
    a <- popDataBool
    pushData $ Boolean $ op a b

interpretStack :: (forall a. [a] -> Maybe [a]) -> ThreadInterpreter r ()
interpretStack manip = ThreadInterpreter \t@Thread {dataStack} ->
    case manip dataStack of
        Just dataStack -> InterpreterResult [] t {dataStack} (Right ())
        Nothing        -> interpreterFailResult t StackUnderflow


interpretBinaryInt :: (Word -> Word -> Word) -> ThreadInterpreter r ()
interpretBinaryInt op = do
    b <- popDataInt
    a <- popDataInt
    pushData $ Integer $ op a b

wrapper :: MicroFactorInstruction r -> MicroFactorInstruction r
wrapper = Literal . Instruction

wrapRef :: InstructionRef r => [MicroFactorInstruction r] -> MicroFactorInstruction r
wrapRef = wrapper . Call . makeRef

--------------------------------------------------------------------------------
-- | Interpret a sequence of instructions on a thread.
-- Creates output and affects the thread, and can fail with an error.
interpret :: InstructionRef a => [MicroFactorInstruction a] -> Thread a -> InterpreterResult a ()
interpret = runInterpreter . interpreter


interpreter :: InstructionRef a => [MicroFactorInstruction a] -> ThreadInterpreter a ()
interpreter [] = ThreadInterpreter \t -> case t of
    Thread { returnStack = [] } -> InterpreterResult [] t (Right ())
    Thread { returnStack = r:rs } -> runInterpreter (interpreter $ resolveRef r) t { returnStack = rs }
interpreter (Call r:[]) = interpreter $ resolveRef r -- tail call optimisation
-- interpreter (Call r:is) = interpreter (resolveRef r) >> interpreter is -- does not make use of the retur stack, cannot be suspended properly
interpreter (Call r:is) = ThreadInterpreter \t@Thread { returnStack } ->
    runInterpreter (interpreter $ resolveRef r) t { returnStack = makeRef is:returnStack }
interpreter (Operator Execute:is) = popDataInstruction >>= \r -> interpreter $ r : is
interpreter (Operator ControlIf:is) = do
    else' <- popDataInstruction
    then' <- popDataInstruction
    cond <- popDataBool
    interpreter $ (if cond then then' else else') : is
interpreter (Operator ControlWhen:is) = do
    then' <- popDataInstruction
    cond <- popDataBool
    if cond then interpreter $ then' : is else interpreter is
interpreter (Operator ControlUnless:is) = do
    else' <- popDataInstruction
    cond <- popDataBool
    if cond then interpreter is else interpreter $ else' : is
interpreter (Operator ControlForever:is) = do
    body <- popDataInstruction
    -- interpreter $ repeat body
    interpreter (body : wrapper body : Operator ControlForever : is)
interpreter (Operator ControlLoop:is) = do
    body <- popDataInstruction
    {-- first attempt:
    let rep = wrapRef [wrapper body, Operator ControlLoop]
    interpreter (body : rep : Operator ControlWhen : is) -}
    let rep = wrapRef [body, rep, Operator ControlWhen]
    interpreter (body : rep : Operator ControlWhen : is)
interpreter (Operator ControlWhile:is) = do
    body <- popDataInstruction
    pred <- popDataInstruction
    {-- first attempt:
    let rep = wrapper [body, pred, rep, Operator ControlWhen]
    interpreter (pred : rep : Operator ControlWhen : is) -}
    let rep = wrapRef (body : go)
        go = [pred, rep, wrapRef is, Operator ControlIf]
    interpreter go
interpreter (Operator ControlUntil:is) = do
    body <- popDataInstruction
    pred <- popDataInstruction
    {-- first attempt:
    let rep = wrapRef [body, pred, rep, Operator ControlUnless]
    interpreter (pred : rep : Operator ControlUnless : is) -}
    let rep = wrapRef (body : go)
        go = [pred, wrapRef is, rep, Operator ControlIf]
    interpreter go
interpreter (Operator ControlDo:is) = do
    body <- popDataInstruction
    pred <- popDataInstruction
    interpreter (body : wrapper pred : wrapper body : is)
interpreter (Operator ControlDip:is) = do
    ref <- popDataInstruction
    val <- popData
    interpreter (ref : Literal val : is)
interpreter (Operator ControlKeep:is) = do
    ref <- popDataInstruction
    val <- popData -- TODO: getData
    pushData val
    interpreter (ref : Literal val : is)
interpreter (i:is) = (\() -> interpreter is) =<< case i of
    Comment _ -> return ()
    LiteralString s -> ThreadInterpreter \t -> InterpreterResult [s] t (Right ())
    Literal val -> pushData val
    Operator Debugger -> return () -- TODO: output
    Operator ControlIte -> do
        b <- popData
        a <- popData
        cond <- popDataBool
        pushData $ if cond then a else b
    Operator LogicNot -> popDataBool >>= pushData . Boolean . not
    Operator LogicNor -> interpretBinaryBool \a b -> not (a || b)
    Operator LogicLt -> interpretCompare (<)
    Operator LogicGt -> interpretCompare (>)
    Operator LogicXor -> interpretCompare (/=) -- Neq
    Operator LogicNand -> interpretBinaryBool \a b -> not (a && b)
    Operator LogicAnd -> interpretBinaryBool (&&)
    Operator LogicXnor -> interpretCompare (==) -- Eq
    Operator LogicLte -> interpretCompare (<=) -- Impl
    Operator LogicGte -> interpretCompare (>=)
    Operator LogicOr -> interpretBinaryBool (||)
    Operator StackNip -> interpretStack \case d:_:ds -> Just (d:ds); _ -> Nothing
    Operator StackDrop -> popData >> return ()
    -- Operator StackPick
    Operator StackDuplicate -> interpretStack \case d:ds -> Just (d:d:ds); _ -> Nothing -- pick 0
    Operator StackOver -> interpretStack \case d1:d2:ds -> Just (d2:d1:d2:ds); _ -> Nothing -- pick 1
    Operator StackTuck -> interpretStack \case d1:d2:ds -> Just (d1:d2:d1:ds); _ -> Nothing
    -- Operator StackRoll
    Operator StackSwap -> interpretStack \case d1:d2:ds -> Just (d2:d1:ds); _ -> Nothing -- roll 1
    Operator StackRotate -> interpretStack \case d1:d2:d3:ds -> Just (d3:d1:d2:ds); _ -> Nothing -- roll 2

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
        ThreadInterpreter \t -> InterpreterResult [showValue val] t (Right ())

-- | shortened representation of `TaggedValue`s
showValue :: TaggedValue a -> String
showValue (Boolean b) = show b
showValue (Integer w) = show w
showValue (PortAddress w) = '#':show w
showValue (Instruction _) = "<Code>"