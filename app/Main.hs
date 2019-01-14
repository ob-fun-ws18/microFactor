module Main where

import Lib
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

main :: IO ()
main = do
    -- setCursorPosition 5 0
    setTitle "Microfactor"
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
            putStrLn $ replicate (sourceColumn $ errorPos e) ' ' ++ "  ^"
            putStrLn $ showErrorMessages "or" "oops?" "expecting" "unexpected" "end of input" $ errorMessages e
            repl state
        Right cmds -> run state cmds

run state [] = repl state
run _ (Quit:_) = return ()
run state (Define id val:cmds) = case resolve (definitions state) val of -- TODO: create circular reference to updated `defs`
    Left pos -> do
        putStrLn $ replicate (sourceColumn pos) ' ' ++ "  ^"
        putStrLn "unknown identifier"
        repl state -- dismiss other cmds
    Right rval -> do
        let defs = insert id rval (definitions state)
        putStrLn $ "Defined " ++ id
        run state { definitions = defs } cmds
run state (Evaluate exp:cmds) = case resolve (definitions state) exp of
    Left pos -> do
        putStrLn $ replicate (sourceColumn pos) ' ' ++ "  ^"
        putStrLn "unknown identifier in"
        print exp
        repl state -- dismiss other cmds
    Right rval -> do
        print rval
        let res = runInterpreter (interpret rval) (thread state)
        forM_ (interpreterOutput res) putStrLn
        case interpreterValue res of
            Left e -> print e
            Right _ -> return ()
        putStrLn $ "< " ++ (show $ dataStack $ interpreterThread res)
        run state { thread = interpreterThread res } cmds
run state (ShowDef id:cmds) = do
    putStrLn $ ":" ++ id ++ " "
    run state cmds

resolve userDefs = resolveNames go
  where
    go :: ParsedRef -> Either SourcePos (MicroFactorInstruction ResolvedRef)
    go (Anonymous is) = fmap (Call . ResolvedRef "") (resolveNames go is)
    go (Named loc name) = maybe (Left loc) Right $ lookup name builtinSymbols <|> fmap (Call . ResolvedRef name) (lookup name userDefs)

data ResolvedRef = ResolvedRef String [MicroFactorInstruction ResolvedRef]

instance InstructionRef ResolvedRef where
    makeRef = ResolvedRef ""
    resolveRef (ResolvedRef _ is) = is

instance Show ResolvedRef where
    show (ResolvedRef name _) = "ResolvedRef { " ++ name ++ " }"