module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM_, (>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List (intercalate)
import Data.Map.Lazy (Map, empty, lookup, keys, foldMapWithKey)
import Data.Maybe (listToMaybe)
import Data.Version (showVersion)
import Text.Parsec (runParser)
import Text.Parsec.Error
import Text.Parsec.Pos
import System.IO
import System.Environment (getArgs)
import System.Console.ANSI
import Prelude hiding (lookup)

import MicroFactor

main :: IO ()
main = do
    setTitle "MicroFactor"
    arg <- listToMaybe <$> getArgs
    evalStateT (maybe repl (run . return . Load . Just) arg) AppState { definitions = empty, dictPath = arg, thread = newThread }

data AppState = AppState
    { definitions :: Map String [MicroFactorInstruction ResolvedRef]
    , dictPath :: Maybe FilePath
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
    handleError (runParser commandParser () "" line) run


run :: [Command] -> StateT AppState IO ()
run [] = repl
run (Quit:_) = return ()
run (Define id val:cmds) = gets definitions >>= \defs ->
    handleError (define id val defs) \newDefs -> do
        modify (\state -> state { definitions = newDefs })
        liftIO $ putStrLn $ "Defined " ++ id
        run cmds
run (Evaluate exp:cmds) = gets definitions >>= \defs ->
    handleError (resolve defs exp) \rval -> do
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
run (Load arg:cmds) = do
    path <- gets dictPath
    case arg <|> path of
        Nothing -> (liftIO $ putWarning "no filepath given") >> repl
        Just p -> do
            -- TODO: catch io exceptions
            defines <- liftIO $ withFile p ReadMode \h -> do
                hSetEncoding h utf8
                text <- hGetContents h
                return $! runParser dictionaryParser () p text
            scope <- gets definitions
            -- let x = foldr (>=>) return $ map (uncurry define) ds
            -- https://hackage.haskell.org/package/foldl-1.4.5/docs/Control-Foldl.html#t:EndoM
            -- let x = appEndoM $ foldMap (EndoM . uncurry define) ds
            let newDefs = defines >>= (\ds -> (foldr (>=>) return $ map (uncurry define) ds) scope)
            either (\err -> liftIO (putWarning (show err)) >> repl) (\newScope -> do
                modify (\state -> state { definitions = newScope })
                liftIO $ putStrLn "ok."
                run cmds) newDefs
run (Save arg:cmds) = do
    path <- gets dictPath
    case arg <|> path of
        Nothing -> (liftIO $ putWarning "no filepath given") >> repl
        Just p -> do
            scope <- gets definitions
            let text = foldMapWithKey (\name def -> ":"++name++" "++show def++";\n") scope
            -- TODO: catch io exceptions
            liftIO $ withFile p WriteMode \h -> do
                hSetEncoding h utf8
                hPutStr h text
            run cmds


handleError :: Either ParseError a -> (a -> StateT AppState IO ()) -> StateT AppState IO ()
handleError = flip $ either \err -> putErrorMessage err >> repl -- dismiss other cmds

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
