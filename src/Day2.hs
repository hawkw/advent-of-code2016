{-# LANGUAGE NamedFieldPuns #-}

module Day2 where

    import Util (getInput)
    import Data.Maybe
    import qualified Data.Map as Map

    data Instruction
        = U
        | D
        | R
        | L
        deriving (Show, Read, Eq)

    -- instance Read Instruction where
    --     readsPrec _ ('U':rest) = [(U, rest)]
    --     readsPrec _ ('D':rest) = [(D, rest)]
    --     readsPrec _ ('R':rest) = [(R, rest)]
    --     readsPrec _ ('L':rest) = [(L, rest)]
    --     readsPrec _ _          = []

    type Point = (Int, Int)

    data Button = Button
        { pos :: Point
        , num :: Char
        }

    instance Show Button where
        show Button { num } = show num


    data Grid = Grid
        { button    :: Point -> Maybe Button
        , width     :: !Int
        , height    :: !Int
        }

    mkGrid :: String -> Grid
    mkGrid string = Grid { button = button
                         , width  = length (head rows)
                         , height = length rows }
        where gridMap  = Map.fromList gridList
              gridList = [ ((x, y), char)
                         | (y, line) <- zip [0..] rows
                         , (x, char) <- zip [0..] line
                         , char /= ' '
                         ]
              button p = Button p <$> Map.lookup p gridMap
              rows     = lines string

    midpoint :: Grid -> Button
    midpoint g = fromJust $ button g ( width g `quot` 2, height g `quot` 2)

    grid = mkGrid "123\n\
                  \456\n\
                  \789\n"

    grid2 = mkGrid
        "  1  \n\
        \ 234 \n\
        \56789\n\
        \ ABC \n\
        \  D  \n"

    move :: Grid -> Button -> Instruction -> Button
    move Grid { button } b = fromMaybe b . button . rawMove b
        where rawMove Button { pos = (x, y) } i = case i of
                U -> (x, y - 1)
                D -> (x, y + 1)
                R -> (x + 1, y)
                L -> (x - 1, y)

    run :: Grid -> Button -> [Instruction] -> Button
    run zig = foldl $ move zig-- for great justice

    solve :: Grid -> [[Instruction]] -> [Button]
    solve g = drop 1 . reverse . foldl run' [midpoint g]
        where run' bs@(b:_) is  = run g b is : bs

    -- | Parses an input string to a list of lists of Instructions
    parseInput :: String -> [[Instruction]]
    parseInput s = fmap (\ch -> read [ch]) <$> lines s

    day2 :: IO ()
    day2 = do
        moves <- parseInput <$> getInput "day2"
        -- print moves
        -- print grid
        print "Solution 1"
        -- print moves
        print $ num <$> solve grid moves
        print "Solution 2"
        print $ num <$> solve grid2 moves
