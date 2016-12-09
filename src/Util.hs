module Util where

    getInput :: String -> IO String
    getInput = readFile . ("data/" ++)
