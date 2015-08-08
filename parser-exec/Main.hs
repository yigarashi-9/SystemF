module Main where

import Control.Monad
import Control.Exception
import System.IO
import System.Environment
import System.Exit

import Parser
import Syntax

main :: IO ()
main = catch (do (expr:_) <- getArgs
                 (either error (putStrLn . astToJson)) . parseTerm $ expr) err
    where
      err e = do
        hPutStrLn stderr $ show (e :: SomeException)
        exitWith (ExitFailure 1)
