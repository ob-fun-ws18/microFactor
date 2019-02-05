module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM_, (>=>))
-- import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.List (intercalate)
import Data.Map.Lazy (Map, empty, lookup, keys)
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
    evalStateT (maybe repl (handleRepl . run . Load . Just) arg) AppState { definitions = empty, dictPath = arg, thread = newThread }

data AppState = AppState
    { definitions :: Map String [MicroFactorInstruction ResolvedRef]
    , dictPath :: Maybe FilePath
    , thread :: Thread ResolvedRef
}

data AppErr = QuitRepl | ParseErr ParseError | MsgErr String

repl :: StateT AppState IO ()
repl = do
    line <- liftIO do
        setSGR [SetColor Foreground Vivid Blue]
        putStr "µ> "
        setSGR []
        hFlush stdout
        getLine
    handleRepl do
        cmds <- liftAppParseErr $ runParser commandParser () "" line
        mapM_ run cmds

handleRepl :: ExceptT AppErr (StateT AppState IO) () -> StateT AppState IO ()
handleRepl e = runExceptT e >>= \case
    Left QuitRepl -> return ()
    Left (ParseErr err) -> putErrorMessage err >> repl
    Left (MsgErr err) -> liftIO (putWarning $ show err) >> repl
    Right () -> repl


run :: Command -> ExceptT AppErr (StateT AppState IO) ()
run Quit = throwError QuitRepl
run (Define id val) = do
    defs <- gets definitions
    newDefs <- liftAppParseErr $ define id val defs
    modify (\state -> state { definitions = newDefs })
    liftIO $ putStrLn $ "Defined " ++ id
run (Evaluate exp) = do
    defs <- gets definitions
    rval <- liftAppParseErr $ resolve defs exp
    -- print rval
    state <- get
    let res = interpret rval $ thread state
    put state { thread = interpreterThread res }
    liftIO $ forM_ (interpreterOutput res) putStrLn
    -- liftIO $ print $ interpreterThread res
    liftAppErr $ interpreterValue res
    liftIO $ putStrLn $ "{ " ++ (intercalate " { " $ fmap showValue $ reverse $ dataStack $ interpreterThread res)
run (ShowDef id) = do
    userFns <- gets definitions
    liftIO $ putStrLn $ case lookup id userFns of
        Nothing -> id ++ " not found"
        Just ref -> ":" ++ id ++ " " ++ show ref ++ ";"
run List = do
    userFns <- gets definitions
    liftIO $ putStrLn $ unwords $ keys (builtinSymbols :: Map String (MicroFactorInstruction ResolvedRef))
    liftIO $ putStrLn $ unwords $ keys userFns
run (Load arg) = do
    path <- getFilePath arg
    -- TODO: catch io exceptions
    parseRes <- liftIO $ withFile path ReadMode \h -> do
        hSetEncoding h utf8
        text <- hGetContents h
        return $! runParser dictionaryParser () path text
    defines <- liftAppErr parseRes
    scope <- gets definitions
    -- let x = foldr (>=>) return $ map (uncurry define) ds
    -- https://hackage.haskell.org/package/foldl-1.4.5/docs/Control-Foldl.html#t:EndoM
    -- let x = appEndoM $ foldMap (EndoM . uncurry define) ds
    newDefs <- liftAppErr $ (foldr (>=>) return $ map (uncurry define) defines) scope
    modify (\state -> state { definitions = newDefs })
    liftIO $ putStrLn "ok."
run (Save arg) = do
    path <- getFilePath arg
    text <- gets $ formatScope . definitions
    -- TODO: catch io exceptions
    liftIO $ withFile path WriteMode \h -> do
        hSetEncoding h utf8
        hPutStr h text

liftAppErr :: (Show e, MonadError AppErr m) => Either e a -> m a
liftAppErr = liftEither . (first $ MsgErr . show)

liftAppParseErr :: (MonadError AppErr m) => Either ParseError a -> m a
liftAppParseErr = liftEither . first ParseErr

getFilePath :: Maybe FilePath -> ExceptT AppErr (StateT AppState IO) FilePath
getFilePath arg = do
    path <- gets dictPath
    maybe (throwError $ MsgErr "no filepath given") return $ arg <|> path


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
