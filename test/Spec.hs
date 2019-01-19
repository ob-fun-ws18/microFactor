{-# LANGUAGE BinaryLiterals #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Map.Strict (Map, insert, empty)
import Text.Printf (printf)
import Text.Parsec (runParser, errorPos)
import Text.Parsec.Pos (SourcePos, newPos)

import MicroFactor

main :: IO ()
main = defaultMain $ testGroup "Microfactor"
    [ testGroup "Parser" $ fromAssertions "parse"
        [ parse "example" @?= Right [Call $ N "example"]
        , parse "'example" @?= Right [Wrapper $ Call $ N "example"]
        , parse "'example example" @?= Right [Wrapper $ Call $ N "example", Call $ N "example"]
        , parse "(* comment *)" @?= Right [Comment "comment"]
        , parse "(*comment *)" @?= Right [Comment "comment"]
        , parse "(* comment*)" @?= Right [Comment "comment"]
        , parse "(*comment*)" @?= Right [Comment "comment"]
        , parse "[- comment -]" @?= Right [Comment "comment"]
        , parse "{* comment *}" @?= Right [Comment "comment"]
        , parse "(# comment #)" @?= Right [Comment "comment"]
        , parse "{- comment -}" @?= Right [Comment "comment"]
        , parse "a (b c) d" @?= Right [Call $ N "a", Wrapper $ Call $ B [Call $ N "b", Call $ N "c"], Call $ N "d"]
        , parse "0" @?= Right [LiteralValue 0]
        , parse "0xa 0x1 0x05bd" @?= Right [LiteralValue 0xa, LiteralValue 0x1, LiteralValue 0x05bd]
        , parse "0xAB10" @?= Right [LiteralValue 0xab10]
        , parse "123 456" @?= Right [LiteralValue 123, LiteralValue 456]
        , parse "0b1010" @?= Right [LiteralValue 0b1010]
        , parse "\"string\"" @?= Right [LiteralString "string"]
        , parse "\"\\n\\r\\t\\\\ \\x\"" @?= Right [LiteralString "\n\r\t\\ x"]
        , parse "\"\" \" \"\"" @?= Right [LiteralString " \" "]
        ] ++
        [ testCase "resolve" $ parseContext [("hello", "1"), ("world", "2")] "world (\"oh\" 'hello) execute" @?= Right
            [ Call $ ResolvedRef "world" [LiteralValue 2]
            , Wrapper $ Call $ ResolvedRef ""
                [ LiteralString "oh"
                , Wrapper $ Call $ ResolvedRef "hello" [LiteralValue 1]]
            , Operator Execute]
        , roundTrip "1 2 \"string\" (* comment *)"
        , roundTrip "example (oh 'well) !"
        ]
    , testGroup "Commands"
        [ testCase "simple declaration" $ runParser commandParser () "" ":test 1 + 1;" @?= Right
            [Define "test" [LiteralValue 1, Call $ Named (column 9) "+", LiteralValue 1]]
        ]
    , testGroup "Interpreter"
        [ testInterpreter (dataStack . interpreterThread) [] "1 2 +" [Integer 3]
        , testInterpreter (dataStack . interpreterThread) [] "1 2 =" [Boolean False]
        , testInterpreter (returnStack . interpreterThread) [("add2", "2 +")] "1 add2" []
        , testInterpreter interpreterOutput [] "1 1 + 2 = ." ["True"]
        , testInterpreter interpreterValue [] "1 = 1" (Left StackUnderflow)
        , testInterpreter interpreterValue [("is", "=")] "1 is" (Left StackUnderflow)
        , testInterpreter (returnStack . interpreterThread) [("is", "=")] "1 is" []
        ]
    ]

fromAssertions :: String -> [Assertion] -> [TestTree]
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

-- like ParsedRef but without source position
data SimpleRef = N String | B [MicroFactorInstruction SimpleRef] deriving (Eq, Show)
instance InstructionRef SimpleRef where
    makeRef = B
    resolveRef (N _) = []
    resolveRef (B x) = x
    refName (N n) = n
    refName (B _) = ""
toSimple (Named _ r) = N r
toSimple (Anonymous is) = B $ fmap (fmap toSimple) is

parse = fmap (fmap (fmap toSimple)) . runParser expressionParser () ""

roundTrip :: String -> TestTree
roundTrip exp = testCase ("round-trip "++exp) $ show <$> parse exp @?= Right exp

column :: Int -> SourcePos
column = newPos "" 1

parseAndResolve :: Map String [MicroFactorInstruction ResolvedRef] -> String -> Either (SourcePos, String) [MicroFactorInstruction ResolvedRef]
parseAndResolve ctx str = do
    exp <- first (\e -> (errorPos e, "parse error")) $ runParser expressionParser () "" str
    resolve ctx exp

parseContext :: [(String, String)] -> String -> Either (SourcePos, String) [MicroFactorInstruction ResolvedRef]
parseContext list exp = do
    userDefs <- foldM (\m (id, v) -> insert id <$> parseAndResolve m v <*> pure m) mempty list
    parseAndResolve userDefs exp

testInterpreter :: (Eq a, Show a) => (InterpreterResult ResolvedRef () -> a) -> [(String, String)] -> String -> a -> TestTree
testInterpreter get ctx exp res = testCase exp $ get <$> (interpret <$> parseContext ctx exp <*> pure newThread) @?= Right res
