{-# LANGUAGE NamedFieldPuns, DeriveFunctor, RankNTypes #-}

module Lib
    ( MicroFactorInstruction (..)
    , InterpreterError (..)
    , Nested (..)
    , ParsedRef (..)
    , interpret
    , expressionParser
    , commandParser
    ) where

import Data.Monoid ((<>))
import Control.Monad
import Data.Functor (($>), (<&>))
import Data.Char (digitToInt)
import Text.Parsec.String (Parser)
import Text.Parsec

data MicroFactorInstruction r
    = Comment String
    | LiteralValue Word
    | LiteralAddress Word -- Port address
    | LiteralString String
    | Wrapper r -- Wrapper (MicroFactorInstruction r) ?
    | Call r

    | Execute
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
    | Yield
    | YieldDelay
    | YieldInput
    | ThreadStart
    | ThreadPass
    deriving (Eq, Show)

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
    deriving (Eq, Show)

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
            -- step interpreterResult = interpreterLeftResult
            step (InterpreterResult o t (Left e)) = InterpreterResult o t (Left e)
    fail e = ThreadInterpreter $ \t -> InterpreterResult [] t (Left $ TypeError e)

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

class InstructionRef a where
    makeRef :: [MicroFactorInstruction a] -> a
    resolveRef :: a -> [MicroFactorInstruction a]

newtype Nested = Nested { unnest :: [MicroFactorInstruction Nested] } deriving Show
instance InstructionRef Nested where
    makeRef = Nested
    resolveRef = unnest

interpret :: InstructionRef a => [MicroFactorInstruction a] -> ThreadInterpreter a ()
interpret [] = ThreadInterpreter $ \t -> case t of
    Thread { returnStack = [] } -> InterpreterResult [] t (Right ())
    Thread { returnStack = r:rs } -> runInterpreter (interpret $ resolveRef r) t { returnStack = rs }
interpret (Exit:_) = interpret []
-- interpret (Call r:is) = interpret (resolveRef r) >> interpret is
interpret (Call r:is) = ThreadInterpreter $ \t@Thread { returnStack } ->
    runInterpreter (interpret $ resolveRef r) t { returnStack = makeRef is:returnStack }
interpret (Execute:is) = popData >>= \(Instructions r) -> interpret (Call r:is)
interpret (i:is) = (\() -> interpret is) =<< case i of
    Comment _ -> return ()
    LiteralValue w -> pushData (Integer w)
    LiteralAddress w -> pushData (PortAddress w)
    LiteralString s -> ThreadInterpreter $ \t -> InterpreterResult [s] t (Right ())
    Wrapper i -> pushData (Instructions i)
    Debugger -> return () -- TODO: output
    LiteralFalse -> pushData (Boolean False)
    LiteralTrue -> pushData (Boolean True)
    LogicNot -> popDataBool >>= pushData . Boolean . not
    LogicNor -> interpretBinaryBool $ \a b -> not (a || b)
    LogicLt -> interpretCompare (<)
    LogicGt -> interpretCompare (>)
    LogicXor -> interpretCompare (/=) -- Neq
    LogicNand -> interpretBinaryBool $ \a b -> not (a && b)
    LogicAnd -> interpretBinaryBool (&&)
    LogicXnor -> interpretCompare (==) -- Eq
    LogicLte -> interpretCompare (<=) -- Impl
    LogicGte -> interpretCompare (>=)
    LogicOr -> interpretBinaryBool (||)
    StackNip -> interpretStack $ \s -> case s of d:_:ds -> Just (d:ds); _ -> Nothing
    StackDrop -> popData >> return ()
    -- StackPick
    StackDuplicate -> interpretStack $ \s -> case s of d:ds -> Just (d:d:ds); _ -> Nothing -- pick 0
    StackOver -> interpretStack $ \s -> case s of d1:d2:ds -> Just (d2:d1:d2:ds); _ -> Nothing -- pick 1
    StackTuck -> interpretStack $ \s -> case s of d1:d2:ds -> Just (d1:d2:d1:ds); _ -> Nothing
    -- StackRoll
    StackSwap -> interpretStack $ \s -> case s of d1:d2:ds -> Just (d2:d1:ds); _ -> Nothing -- roll 1
    StackRotate -> interpretStack $ \s -> case s of d1:d2:d3:ds -> Just (d3:d1:d2:ds); _ -> Nothing -- roll 2
    {-
        | ArithAdd
        | ArithSub
        | ArithMul
        | ArithDiv
        | ArithMod
        | ArithDivmod
        | ArithAbs
        | ArithMax
        | ArithMin
    -}

data ParsedRef
    = Anonymous [MicroFactorInstruction ParsedRef]
    | Named String
    deriving (Eq, Show)

expressionParser :: Parser [MicroFactorInstruction ParsedRef]
expressionParser = element `sepBy` spaces -- TODO: require at least 1 space?
  where
    element = choice
        [ parenthised
        , numberLiteral
        , stringLiteral
        , Wrapper . Named <$> (char '\'' *> identifier)
        , Call . Named <$> identifier
        ]
    identifier = flip label "identifier" $ many1 $ noneOf " ()[]{}':;"
    parenthised = flip labels ["comment", "block"] $ do
        end <- choice [char a $> b | (a, b) <- [('(',')'), ('[',']'), ('{','}')]]
        choice
            [ Comment <$> do
                delim <- oneOf "#-*"
                spaces
                manyTill anyChar $ try $ spaces >> char delim >> char end
            , Wrapper . Anonymous <$> (spaces *> expressionParser <* spaces <* char end)
            ]
    numberLiteral = flip label "number" $ (char '0' >> choice
        [ char 'x' >> many1 hexDigit <&> parseNumber 16
        , char 'b' >> many1 (oneOf "01") <&> parseNumber 2
        , many digit <&> parseNumber 10
        ]) <|> (many1 digit <&> parseNumber 10)
    parseNumber base = LiteralValue . foldl (\x -> ((base * x) +) . fromIntegral . digitToInt) 0
    stringLiteral = flip label "string" $ do
        delim <- many1 $ char '"'
        LiteralString <$> manyTill ((char '\\' >> choice
            [ char 'r' $> '\r'
            , char 'n' $> '\n'
            , char 't' $> '\t'
            , anyChar
            ]) <|> anyChar) (try $ string delim)

commandParser = choice
    [ do
        char ':'
        id <- many1 $ noneOf " ()[]{}':" -- identifier
        spaces
        expr <- expressionParser
        char ';'
        return (id, expr)
    -- , string "SAVE"
    ]