module Util where

    import qualified Data.Map as Map
    import Data.List

    getInput :: String -> IO String
    getInput = readFile . ("data/" ++)

    mostCommon :: Ord a => Int -> [a] -> [a]
    n `mostCommon` l = fst <$> (take n . sortBy order . Map.toList $ counts l)
        where counts = foldr (\i -> Map.insertWith (+) i 1) Map.empty
              order (a, count_a) (b, count_b)
                | count_a /= count_b = count_b `compare` count_a
                | count_a == count_b = a `compare` b
