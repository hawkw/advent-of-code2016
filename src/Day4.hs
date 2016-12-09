module Day4( day4 ) where

    import Util
    import Data.Char
    import Data.List

    -- | Parses all of the letters in the name of a room
    nameLetters :: String -> String
    nameLetters = filter isAlpha . takeWhile (/= '[')

    expectedChecksum :: String -> String
    expectedChecksum = (5 `mostCommon`) . nameLetters

    realChecksum :: String -> String
    realChecksum = take 5 . drop 1 . dropWhile (/= '[')

    isValid :: String -> Bool
    isValid s = expectedChecksum s == realChecksum s

    sectorID :: String -> Integer
    sectorID = read . filter isNumber
        -- where isNum c = isAlphaNum c && not (isAlpha c)

    day4 :: IO ()
    -- day4 = undefined
    day4 = do
        input <- getInput "day4"
        print "Solution 1"
        print . sum $ (sectorID <$> filter isValid (lines input))
