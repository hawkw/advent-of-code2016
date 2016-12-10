module Day4( day4 ) where

    import Util
    import Data.Char
    import Data.List
    import Control.Monad

    -- | Parses all of the letters in the name of a room
    nameLetters :: String -> String
    nameLetters = filter isAlpha . takeWhile (/= '[')

    expectedChecksum :: String -> String
    expectedChecksum = (5 `mostCommon`) . nameLetters

    realChecksum :: String -> String
    realChecksum = take 5 . drop 1 . dropWhile (/= '[')

    isValid :: String -> Bool
    isValid = liftM2 (==) expectedChecksum realChecksum

    sectorID :: String -> Int
    sectorID = read . filter isNumber
        -- where isNum c = isAlphaNum c && not (isAlpha c)

    caesar :: Int -> String -> String
    caesar n = fmap shift
        where shift :: Char -> Char
              shift c
                | isAlpha c = remap $ (demap c + n) `mod` 26
                | c == '-'  = ' '
                | otherwise = c
              demap = subtract (ord 'a') . ord
              remap = chr . (ord 'a' +)

    decrypt :: String -> String
    decrypt s = caesar (sectorID s) namePart
        where namePart = takeWhile (/= '[') s

    day4 :: IO ()
    -- day4 = undefined
    day4 = do
        input <- getInput "day4"
        let rooms = lines input
        putStr "Solution 1: "
        print . sum $ (sectorID <$> filter isValid rooms)
        putStrLn "Solution 2:"
        -- print . filter ("north pole" `isPrefixOf`) $ decrypt <$> rooms
        print . filter ("north" `isPrefixOf`) $ decrypt <$> rooms
