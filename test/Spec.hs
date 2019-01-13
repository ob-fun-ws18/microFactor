{-# LANGUAGE BinaryLiterals #-}

import Test.Tasty
import Test.Tasty.HUnit

import Text.Printf (printf)
import Text.Parsec (runParser)
import Lib

main :: IO ()
main = defaultMain $ testGroup "Microfactor"
    [ testGroup "Parser" $ fromAssertions "parse"
        [ parse "example" @?= Right [Call $ Named "example"]
        , parse "'example" @?= Right [Wrapper $ Named "example"]
        , parse "'example example" @?= Right [Wrapper $ Named "example", Call $ Named "example"]
        , parse "(* comment *)" @?= Right [Comment "comment"]
        , parse "(*comment *)" @?= Right [Comment "comment"]
        , parse "(* comment*)" @?= Right [Comment "comment"]
        , parse "(*comment*)" @?= Right [Comment "comment"]
        , parse "[- comment -]" @?= Right [Comment "comment"]
        , parse "{* comment *}" @?= Right [Comment "comment"]
        , parse "(# comment #)" @?= Right [Comment "comment"]
        , parse "{- comment -}" @?= Right [Comment "comment"]
        , parse "a (b c) d" @?= Right [Call $ Named "a", Wrapper $ Anonymous [Call $ Named "b", Call $ Named "c"], Call $ Named "d"]
        , parse "0" @?= Right [LiteralValue 0]
        , parse "0xa 0x1 0x05bd" @?= Right [LiteralValue 0xa, LiteralValue 0x1, LiteralValue 0x05bd]
        , parse "0xAB10" @?= Right [LiteralValue 0xab10]
        , parse "123 456" @?= Right [LiteralValue 123, LiteralValue 456]
        , parse "0b1010" @?= Right [LiteralValue 0b1010]
        , parse "\"string\"" @?= Right [LiteralString "string"]
        , parse "\"\\n\\r\\t\\\\ \\x\"" @?= Right [LiteralString "\n\r\t\\ x"]
        , parse "\"\" \" \"\"" @?= Right [LiteralString " \" "]
        ]
    , testGroup "Commands"
        [ testCase "simple declaration" $ runParser commandParser () "" ":test 1 + 1;" @?= Right ("test", [LiteralValue 1, Call $ Named "+", LiteralValue 1])
        ]
    ]

fromAssertions :: String -> [Assertion] -> [TestTree]
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

parse = runParser expressionParser () ""