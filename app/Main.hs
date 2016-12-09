module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    num <- getArgs
    case num of
        ["1"] -> day1
        ["2"] -> day2
        _ -> undefined
