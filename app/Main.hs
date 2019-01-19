module Main where

import Control.Applicative ((<|>))
import Control.Monad (forever, unless, forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Concurrent
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

repl = do
    line <- liftIO $ do
        setSGR [SetColor Foreground Vivid Blue]
        putStr "µ> "
        setSGR []
        hFlush stdout
        getLine
    case runParser commandParser () "" line of
        Left e -> do
            putErrorMessage (errorPos e) $ showErrorMessages "or" "oops?" "expecting" "unexpected" "end of input" $ errorMessages e
            repl
        Right cmds -> run cmds

run [] = repl
run (Quit:_) = return ()
run (Define id val:cmds) = gets definitions >>= \defs -> case resolve defs val of -- TODO: create circular reference to updated `defs`
    Left (pos, name) -> do
        putErrorMessage pos $ "unknown identifier " ++ name
        repl -- dismiss other cmds
    Right rval -> do
        modify (\state -> state { definitions = insert id rval (definitions state) })
        liftIO $ putStrLn $ "Defined " ++ id
        run cmds
run (Evaluate exp:cmds) = gets definitions >>= \defs -> case resolve defs exp of
    Left (pos, name) -> do
        putErrorMessage pos $ "unknown identifier " ++ name
        repl -- dismiss other cmds
    Right rval -> do
        -- print rval
        state <- get
        let res = interpret rval $ thread state
        put state { thread = interpreterThread res }
        liftIO $ forM_ (interpreterOutput res) putStrLn
        -- liftIO $ print $ interpreterThread res
        case interpreterValue res of
            Left e -> asWarning $ print e
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
    liftIO $ putStrLn $ unwords $ keys builtinSymbols
    liftIO $ putStrLn $ unwords $ keys userFns
    run cmds

putErrorMessage :: SourcePos -> String -> StateT s IO ()
putErrorMessage pos msg = asWarning $ do
    putStrLn $ replicate (sourceColumn pos) ' ' ++ "  ^"
    putStrLn msg

asWarning :: IO a -> StateT s IO ()
asWarning print = liftIO $ do
    setSGR [SetColor Background Dull Red, SetColor Foreground Vivid Green]
    print
    setSGR [] -- reset
