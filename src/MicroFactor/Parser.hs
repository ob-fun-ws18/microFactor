{-# LANGUAGE RecursiveDo #-}

-- | Parse string to bytecode and resolve identifiers to other functions
module MicroFactor.Parser
    ( ParsedRef (..)
    , ResolvedRef (..)
    , expressionParser
    , Command (..)
    , builtinSymbols
    , commandParser
    , dictionaryParser
    , MicroFactorScope
    , resolve
    , define
    , formatErrorMessages
    ) where

import Control.Monad
import Control.Applicative ((<|>))
import Data.Functor (($>), (<&>))
import Data.Char (digitToInt)
import Data.Map.Lazy (Map, fromList, lookup, insert)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Error
import Prelude hiding (lookup)

import MicroFactor.Data

--------------------------------------------------------------------------------

-- | The references to callables that the parser emits
data ParsedRef
    = Anonymous [MicroFactorInstruction ParsedRef] -- ^ a parenthised code block
    | Named SourcePos String -- ^ an identifier
    deriving (Eq, Show)

-- necessary for `Show`
instance InstructionRef ParsedRef where
    makeRef = Anonymous
    resolveRef = error "ParsedRefs cannot be interpreted"
    refName (Anonymous _) = ""
    refName (Named _ n) = n

-- | An optionally named reference to more instructions
data ResolvedRef = ResolvedRef String [MicroFactorInstruction ResolvedRef] deriving Eq

instance InstructionRef ResolvedRef where
    makeRef = ResolvedRef ""
    resolveRef (ResolvedRef _ is) = is
    refName (ResolvedRef n _) = n -- can be used to reconstruct the source text

instance Show ResolvedRef where
    -- avoid showing circular structures in recursive functions
    show (ResolvedRef "" is) = "(" ++ show is ++ ")"
    show (ResolvedRef name _) = name

--------------------------------------------------------------------------------

-- | A parser for an expression, i.e. an instruction sequence with at least one element
expressionParser :: Parser [MicroFactorInstruction ParsedRef]
expressionParser = element `sepBy1` spaces
  where
    element = choice
        [ parenthised
        , numberLiteral
        , stringLiteral
        , Literal . Instruction <$> (char '\'' *> identifier)
        , identifier
        ] <?> "expression item"
    identifier = Call <$> liftM2 Named getPosition identifierParser
    parenthised = flip labels ["comment", "block"] do
        end <- choice [char a $> b | (a, b) <- [('(',')'), ('[',']'), ('{','}')]]
        choice
            [ Comment <$> do
                delim <- oneOf "#-*"
                spaces
                manyTill anyChar $ try $ spaces >> char delim >> char end
            , Literal . Instruction . Call . Anonymous <$> (spaces *> element `sepEndBy1` spaces <* char end)
            ]
    numberLiteral = flip label "number" $ (char '0' >> choice
        [ char 'x' >> many1 hexDigit <&> parseNumber 16
        , char 'b' >> many1 (oneOf "01") <&> parseNumber 2
        , many digit <&> parseNumber 10
        ]) <|> (many1 digit <&> parseNumber 10)
    parseNumber base = Literal . Integer . foldl (\x -> ((base * x) +) . fromIntegral . digitToInt) 0
    stringLiteral = flip label "string" $ LiteralString <$> stringParser

-- | A parser for a identifier
identifierParser :: Parser String
identifierParser = flip label "identifier" $ many1 $ noneOf " ()[]{}'\":;" -- TODO: no whitespace

-- | A parser for string literal syntax
stringParser :: Parser String
stringParser = do
    delim <- many1 $ char '"'
    manyTill ((char '\\' >> choice
        [ char 'r' $> '\r'
        , char 'n' $> '\n'
        , char 't' $> '\t'
        , anyChar
        ]) <|> anyChar) (try $ string delim)

--------------------------------------------------------------------------------

-- | The various commands available on the REPL
data Command
    = Quit
    | List
    | Define String [MicroFactorInstruction ParsedRef]
    | Evaluate [MicroFactorInstruction ParsedRef]
    | ShowDef String
    | Load (Maybe String)
    | Save (Maybe String)
    deriving (Eq, Show)

-- | A parser for a sequence of commands
commandParser :: Parser [Command]
commandParser = choice
    [ uncurry Define <$> definitionParser
    , Quit <$ tryString "Quit"
    , List <$ tryString "List"
    , ShowDef <$> (tryString "Show" *> many1 space *> identifierParser)
    , Load <$> (tryString "Load" *> filename)
    , Load Nothing <$ tryString "Reload"
    , Save <$> (tryString "Save" *> filename)
    , Evaluate <$> expressionParser <?> "expression"
    ] `sepEndBy` spaces <* eof
  where
    tryString = try . string
    filename = optionMaybe $ many1 space *> (stringParser <|> identifierParser)

definitionParser :: Parser (String, [MicroFactorInstruction ParsedRef])
definitionParser = do
    char ':'
    id <- identifierParser
    spaces
    expr <- expressionParser
    char ';'
    return (id, expr)

dictionaryParser :: Parser [(String, [MicroFactorInstruction ParsedRef])]
dictionaryParser = sepEndBy definitionParser (newline <|> crlf) <* eof

--------------------------------------------------------------------------------
-- | Try to map the `Call` contents of a `MicroFactorInstruction` sequence to some other instruction, potentially with an error
resolveNames :: (a -> Either b (MicroFactorInstruction c)) -> [MicroFactorInstruction a] -> Either b [MicroFactorInstruction c]
resolveNames f = traverse (fmap join . traverse f)

-- | A lookup map of all the available `Operator`s from their names, including the two boolean literals
builtinSymbols :: InstructionRef r => Map String (MicroFactorInstruction r)
builtinSymbols = fromList $ fmap (show >>= (,)) $ fmap Operator [minBound..maxBound] ++ fmap (Literal . Boolean) [True, False]

-- | A `Map` of available functions
type MicroFactorScope = Map String [MicroFactorInstruction ResolvedRef]

-- | Try to resolve the `Named` `ParsedRef`s in a `MicroFactorInstruction` sequence.
-- Uses the `builtinSymbols` or a lookup in the supplied scope,
-- emitting an /unknown identifier/ message if it fails for any
resolve :: MicroFactorScope -> [MicroFactorInstruction ParsedRef] -> Either ParseError [MicroFactorInstruction ResolvedRef]
resolve userDefs = resolveNames go
  where
    go :: ParsedRef -> Either ParseError (MicroFactorInstruction ResolvedRef)
    go (Anonymous is) = fmap (Call . ResolvedRef "") (resolveNames go is)
    go (Named loc name) = maybe (Left $ newErrorMessage (Message $ "unknown identifier " ++ name) loc) Right $
        lookup name builtinSymbols <|> fmap (Call . ResolvedRef name) (lookup name userDefs)

-- | Add a function definition to the scope.
-- Uses `resolve`, but also supports recursive definitions to the new function itself.
-- TODO: Does not yet update references when redefining an existing name
define :: String -- ^ Name of the new function
    -> [MicroFactorInstruction ParsedRef] -- ^ Body of the new function
    -> MicroFactorScope
    -> Either ParseError MicroFactorScope
{- see Assembly: Circular Programming with Recursive do
    in The Monad.Reader Issue 6 <https://wiki.haskell.org/wikiupload/1/14/TMR-Issue6.pdf> -}
define name val defs = mdo
    let newDefs = insert name rval defs -- Note: this must be a Lazy map!
    rval <- resolve newDefs val -- travels back in time!
    return newDefs

-- | show the `errorMessages` of a `ParseError` only (not its `errorPos`).
-- Otherwise very similar to the `Show` instance. Notice the leading linebreak!
formatErrorMessages :: ParseError -> String
formatErrorMessages = showErrorMessages "or" "\noops?" "expecting" "unexpected" "end of input" . errorMessages