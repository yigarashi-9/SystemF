module Main where

import Control.Monad
import Parser

main :: IO ()
main = getLine >>= ((either print print) . parseTerm)
