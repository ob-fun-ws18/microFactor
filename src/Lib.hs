{-# LANGUAGE NamedFieldPuns, DeriveFunctor, RankNTypes #-}

module Lib
    ( MicroFactorInstruction (..)
    , resolveNames
    , builtinSymbols
    , InstructionRef (..)
    , InterpreterError (..)
    , InterpreterResult (..)
    , ThreadInterpreter
    , runInterpreter
    , Thread (..)
    , newThread
    , Nested (..)
    , ParsedRef (..)
    , interpret
    , expressionParser
    , Command (..)
    , commandParser
    ) where

import Data.Monoid ((<>))
import Control.Monad
import Control.Arrow
import Data.Functor (($>), (<&>))
import Data.Char (digitToInt)
import Data.Map.Strict (Map, fromList)
import Text.Parsec.String (Parser)
import Text.Parsec

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

resolveNames :: (a -> Either b (MicroFactorInstruction c)) -> [MicroFactorInstruction a] -> Either b [MicroFactorInstruction c]
resolveNames f = fmap (fmap join) . traverse (traverse f)

builtinSymbols :: Map String (MicroFactorInstruction a)
builtinSymbols = fromList $ fmap (second Operator)
    [ ("nor",      LogicNor)
    --, ("-!>",      LogicInhib)
    , ("xor",      LogicXor)
    , ("nand",     LogicNand)
    , ("xnor",     LogicXnor)
    , ("->",       LogicLte)
    , (">=",       LogicGte)
    , ("<=",       LogicLte)
    , ("not",      LogicNot)
    --, ("rand",     RANDOM)
    , ("*",        ArithMul)
    , ("+",        ArithAdd)
    , ("-",        ArithSub)
    --, (".",        Send)
    , ("/",        ArithDiv)
    , ("/mod",     ArithDivmod)
    , ("<",        LogicLt)
    , ("=",        LogicXnor)
    , (">",        LogicGt)
    , ("abs",      ArithAbs)
    , ("and",      LogicAnd)
    , ("drop",     StackDrop)
    , ("dup",      StackDuplicate)
    , ("execute",  Execute)
    , ("max",      ArithMax)
    , ("min",      ArithMin)
    , ("mod",      ArithMin)
    , ("or",       LogicOr)
    , ("over",     StackOver)
    , ("rot",      StackRotate)
    , ("swap",     StackSwap)
    , ("<>",       LogicXor)
    , ("nip",      StackNip)
    , ("false",    LiteralFalse)
    , ("true",     LiteralTrue)
    , ("pick",     StackPick)
    , ("roll",     StackRoll)
    , ("tuck",     StackTuck)
    , ("debugger", Debugger)
    ]

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
interpret (Operator Exit:_) = interpret []
-- interpret (Call r:is) = interpret (resolveRef r) >> interpret is
interpret (Call r:is) = ThreadInterpreter $ \t@Thread { returnStack } ->
    runInterpreter (interpret $ resolveRef r) t { returnStack = makeRef is:returnStack }
interpret (Operator Execute:is) = popData >>= \(Instructions r) -> interpret (Call r:is)
interpret (i:is) = (\() -> interpret is) =<< case i of
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
    | Named SourcePos String
    deriving (Eq, Show)

instance InstructionRef ParsedRef where
    makeRef = Anonymous
    resolveRef = const [] -- TODO

expressionParser :: Parser [MicroFactorInstruction ParsedRef]
expressionParser = element `sepBy1` spaces -- TODO: require at least 1 space?
  where
    element = choice
        [ parenthised
        , numberLiteral
        , stringLiteral
        , Wrapper <$> (char '\'' *> identifier)
        , identifier
        ]
    identifier = Call <$> liftM2 Named getPosition identifierParser
    parenthised = flip labels ["comment", "block"] $ do
        end <- choice [char a $> b | (a, b) <- [('(',')'), ('[',']'), ('{','}')]]
        choice
            [ Comment <$> do
                delim <- oneOf "#-*"
                spaces
                manyTill anyChar $ try $ spaces >> char delim >> char end
            , Wrapper . Call . Anonymous <$> (spaces *> expressionParser <* spaces <* char end)
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

identifierParser :: Parser String
identifierParser = flip label "identifier" $ many1 $ noneOf " ()[]{}'\":;"

data Command
    = Quit
    | Define String [MicroFactorInstruction ParsedRef]
    | Evaluate [MicroFactorInstruction ParsedRef]
    | ShowDef String

commandParser :: Parser [Command]
commandParser = choice
    [ do
        char ':'
        id <- identifierParser
        spaces
        expr <- expressionParser
        char ';'
        return $ Define id expr
    , Quit <$ string "Quit"
    , ShowDef <$> (string "Show" *> many1 space *> identifierParser)
    , Evaluate <$> expressionParser <?> "expression"
    ] `sepBy` spaces <* eof