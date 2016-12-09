{-|
    Day 3: Squares With Three Sides

    Now that you can think clearly, you move deeper into the labyrinth of
    hallways and office furniture that makes up this part of Easter Bunny HQ.
    This must be a graphic design department; the walls are covered in
    specifications for triangles.

    Or are they?

    The design document gives the side lengths of each triangle it describes,
    but... 5 10 25? Some of these aren't triangles. You can't help but mark
    the impossible ones.

    In a valid triangle, the sum of any two sides must be larger than the
    remaining side. For example, the "triangle" given above is impossible,
    because 5 + 10 is not larger than 25.

    In your puzzle input, how many of the listed triangles are possible?
-}
module Day3 (day3) where

    import Data.List
    import Util

    type Triangle = (Integer, Integer, Integer)

    isValid :: Triangle -> Bool
    isValid (a, b, c)
        | a + b > c && a + c > b && b + c > a = True
        | otherwise                           = False

    -- | see https://people.eecs.berkeley.edu/~wkahan/Triangle.pdf
    -- isValid' :: Triangle -> Bool
    -- isValid' (a, b, c)
    --     | c' - (a' - b') <= 0 = False
    --     | otherwise           = False
    --     where [c', b', a'] = sort [a, b, c]

    trisByRow :: String -> [Triangle]
    trisByRow s = (\[a, b, c] -> (a, b, c)) <$> points
        where rows :: [[String]]
              rows = words <$> lines s
              points :: [[Integer]]
              points = fmap (read <$>) rows

    day3 :: IO ()
    day3 = do
        input <- getInput "day3"
        print "Solution 1"
        print (length . filter isValid $ trisByRow input)
