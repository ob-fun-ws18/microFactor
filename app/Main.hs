module Main where

import Control.Applicative ((<|>))
import Control.Monad (forever, unless, forM_)
import Control.Concurrent
import Data.Map.Strict
import Text.Parsec (runParser)
import Text.Parsec.Error
import Text.Parsec.Pos
import System.IO
import System.Console.ANSI
import Prelude hiding (lookup)

import MicroFactor

main :: IO ()
main = do
    -- setCursorPosition 5 0
    setTitle "MicroFactor"
    repl AppState { definitions = empty, thread = newThread }

data AppState = AppState
    { definitions :: Map String [MicroFactorInstruction ResolvedRef]
    , thread :: Thread ResolvedRef
}

repl state = do
    putStr "µ> "
    hFlush stdout
    line <- getLine
    case runParser commandParser () "" line of
        Left e -> do
            putErrorMessage (errorPos e) $ showErrorMessages "or" "oops?" "expecting" "unexpected" "end of input" $ errorMessages e
            repl state
        Right cmds -> run state cmds

run state [] = repl state
run _ (Quit:_) = return ()
run state (Define id val:cmds) = case resolve (definitions state) val of -- TODO: create circular reference to updated `defs`
    Left (pos, name) -> do
        putErrorMessage pos $ "unknown identifier " ++ name
        repl state -- dismiss other cmds
    Right rval -> do
        let defs = insert id rval (definitions state)
        putStrLn $ "Defined " ++ id
        run state { definitions = defs } cmds
run state (Evaluate exp:cmds) = case resolve (definitions state) exp of
    Left (pos, name) -> do
        putErrorMessage pos $ "unknown identifier " ++ name
        repl state -- dismiss other cmds
    Right rval -> do
        print rval
        let res = runInterpreter (interpret rval) (thread state)
        forM_ (interpreterOutput res) putStrLn
        case interpreterValue res of
            Left e -> print e
            Right _ -> return ()
        putStrLn $ "< " ++ (show $ reverse $ dataStack $ interpreterThread res)
        run state { thread = interpreterThread res } cmds
run state (ShowDef id:cmds) = do
    putStrLn $ case lookup id $ definitions state of
        Nothing -> id ++ " not found"
        Just ref -> ":" ++ id ++ " " ++ show ref
    run state cmds

putErrorMessage :: SourcePos -> String -> IO ()
putErrorMessage pos msg = do
    setSGR [SetColor Background Dull Red]
    putStrLn $ replicate (sourceColumn pos) ' ' ++ "  ^"
    putStrLn msg
    setSGR [] -- reset
