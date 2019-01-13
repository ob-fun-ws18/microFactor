module Main where

import Lib
import Control.Monad (forever, unless)
import Control.Concurrent
import System.Console.ANSI

main :: IO ()
main = do
    -- setCursorPosition 5 0
    setTitle "Microfactor"
    forkIO $ run 0
    go ""
    putStrLn "Bye!"
  where
    {-
    go status = do
        clearLine
        setSGR [Reset, SetColor Foreground Vivid White, SetColor Background Dull Blue]
        putStrLn $ "Status: " ++ status;
        setSGR [] -- reset
        msg <- getLine
        cursorUpLine 2
        clearLine
        putStrLn msg
        go $ reverse $ take 50 $ reverse $ status ++ msg
    -}
    {-
    go status = do
        putStrLn ">"
        setSGR [Reset, SetColor Foreground Vivid White, SetColor Background Dull Blue]
        putStr $ "Status: " ++ status
        setSGR [] -- reset
        cursorUpLine 1
        cursorForward 2
        msg <- getLine
        -- cursorUpLine 1; clearLine; putStrLn msg
        clearLine
        unless (msg == "exit") $ go $ reverse $ take 50 $ reverse $ status ++ " " ++ msg
    -}
    go status = do
        putStr "> "
        msg <- getLine
        clearLine
        unless (msg == "exit") $ go $ reverse $ take 50 $ reverse $ status ++ " " ++ msg
    run count = do
        saveCursor
        cursorDownLine 1
        clearLine
        putStr $ "Count: " ++ show count
        restoreCursor
        threadDelay $ 200 * 1000
        run $ count + 1