module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List (intercalate)
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
    setTitle "MicroFactor"
    evalStateT repl AppState { definitions = empty, thread = newThread }

data AppState = AppState
    { definitions :: Map String [MicroFactorInstruction ResolvedRef]
    , thread :: Thread ResolvedRef
}

repl :: StateT AppState IO ()
repl = do
    line <- liftIO do
        setSGR [SetColor Foreground Vivid Blue]
        putStr "µ> "
        setSGR []
        hFlush stdout
        getLine
    case runParser commandParser () "" line of
        Left e -> do
            putErrorMessage e
            repl
        Right cmds -> run cmds


run :: [Command] -> StateT AppState IO ()
run [] = repl
run (Quit:_) = return ()
run (Define id val:cmds) = gets definitions >>= \defs -> case resolve defs val of -- TODO: create circular reference to updated `defs`
    Left e -> do
        putErrorMessage e
        repl -- dismiss other cmds
    Right rval -> do
        modify (\state -> state { definitions = insert id rval (definitions state) })
        liftIO $ putStrLn $ "Defined " ++ id
        run cmds
run (Evaluate exp:cmds) = gets definitions >>= \defs -> case resolve defs exp of
    Left e -> do
        putErrorMessage e
        repl -- dismiss other cmds
    Right rval -> do
        -- print rval
        state <- get
        let res = interpret rval $ thread state
        put state { thread = interpreterThread res }
        liftIO $ forM_ (interpreterOutput res) putStrLn
        -- liftIO $ print $ interpreterThread res
        case interpreterValue res of
            Left e -> liftIO $ putWarning $ show e
            Right _ -> return ()
        liftIO $ putStrLn $ "{ " ++ (intercalate " { " $ fmap showValue $ reverse $ dataStack $ interpreterThread res)
        run cmds
run (ShowDef id:cmds) = do
    userFns <- gets definitions
    liftIO $ putStrLn $ case lookup id userFns of
        Nothing -> id ++ " not found"
        Just ref -> ":" ++ id ++ " " ++ show ref ++ ";"
    run cmds
run (List:cmds) = do
    userFns <- gets definitions
    liftIO $ putStrLn $ unwords $ keys (builtinSymbols :: Map String (MicroFactorInstruction ResolvedRef))
    liftIO $ putStrLn $ unwords $ keys userFns
    run cmds

putErrorMessage :: ParseError -> StateT s IO ()
putErrorMessage err = liftIO do
    let pos = errorPos err
    let msg = formatErrorMessages err
    putStr $ replicate (2 + sourceColumn pos) ' ' -- 2 is the number of characters in the prompt
    putWarning $ "^" ++ msg

-- print a message with green on red color.
putWarning :: String -> IO ()
putWarning msg = do
    setSGR [SetColor Background Dull Red, SetColor Foreground Vivid Green]
    putStr msg
    setSGR [] -- reset
    putStr "\n" -- newline after the reset, otherwise the next line keeps the background
