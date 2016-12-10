{-|
    You are faced with a security door designed by Easter Bunny engineers that
     seem to have acquired most of their security knowledge by watching hacking
     movies.

    The eight-character password for the door is generated one character at a
    time by finding the MD5 hash of some Door ID (your puzzle input) and an
    increasing integer index (starting with 0).

    A hash indicates the next character in the password if its hexadecimal
    representation starts with five zeroes. If it does, the sixth character
    in the hash is the next character of the password.

    For example, if the Door ID is abc:

        The first index which produces a hash that starts with five zeroes is
        3231929, which we find by hashing abc3231929;

        the sixth character of the hash, and thus the first character of the
        password, is 1.

        5017308 produces the next interesting hash, which starts with 0
        00008f82..., so the second character of the password is 8.

        The third time a hash starts with five zeroes is for abc5278568,
        discovering the character f. In this example, after continuing this
        search a total of eight times, the password is 18f47a30.

    Given the actual Door ID, what is the password?
-}
module Day5 (day5) where

    import Crypto.Hash
    import Data.ByteString (ByteString)
    import Data.String (fromString)
    import Data.List
    import Data.Monoid
    import Data.Maybe


    -- | Returns True if the next character in a string is a password character
    isPassChar :: String -> Bool
    isPassChar = ("00000" `isPrefixOf`)

    -- | Returns the next password character if it exists
    passChar :: String -> Maybe Char
    passChar ('0':'0':'0':'0':'0': c : _) = Just c
    passChar _                            = Nothing

    mkHash :: String -> Int -> String
    mkHash = ((show . md5 . fromString) .) . candidate
        where candidate = (. show) . (<>)
              md5 :: ByteString -> Digest MD5
              md5 = hash

    password :: String -> String
    password s = take 8 . catMaybes $ ((passChar . thisHash) <$> [0..])
        where thisHash = mkHash s

    day5 :: IO ()
    day5 = do
        let input = "reyedfim"
        putStr "Solution 1: "
        print $ password input
