{-# LANGUAGE NamedFieldPuns #-}
{-
    --- Day 1: No Time for a Taxicab ---

    You're airdropped near Easter Bunny Headquarters in a city somewhere.
    "Near", unfortunately, is as close as you can get - the instructions on
    the Easter Bunny Recruiting Document the Elves intercepted start here,
    and nobody had time to work them out further.

    The Document indicates that you should start at the given coordinates
    (where you just landed) and face North. Then, follow the provided sequence:
    either turn left (L) or right (R) 90 degrees, then walk forward the given
    number of blocks, ending at a new intersection.

    There's no time to follow such ridiculous instructions on foot, though, so
    you take a moment and work out the destination. Given that you can only
    walk on the street grid of the city, how far is the shortest path to the
    destination?

-}

type Point a = (a, a)

data Heading = North | East | South | West
               deriving (Eq, Show, Enum, Ord, Bounded)

data Direction = L | R
                 deriving (Eq, Show, Read)

data Pose = Pose { position :: !(Point Integer)
                 , heading  :: !Heading }
                 deriving (Eq, Show)

data Move = Move { turn :: !Direction
                 , dist :: !Integer }
                 deriving (Eq, Show)

instance Read Move where
    readsPrec _ ('L':rest) = [(Move { turn = L, dist = read rest }, "")]
    readsPrec _ ('R':rest) = [(Move { turn = R, dist = read rest }, "")]
    readsPrec _ _          = []

-- | Returns the `Pose` at the end of a `Move`
move :: Pose -> Move -> Pose
move Pose { position = (x, y), heading } Move { turn, dist } =
    let heading' = rotate turn heading
        position' = case heading' of
          North -> (x, y + dist)
          South -> (x, y - dist)
          East  -> (x + dist, y)
          West  -> (x - dist, y)
    in Pose { position = position', heading = heading' }


-- | Returns the list of all `Pose`s traversed by a `Move`
steps :: Pose -> Move -> [Pose]
steps Pose { position = (x, y), heading } Move { turn, dist } =
    let heading' = rotate turn heading
        step i = case heading' of
            North -> (x, y + i)
            South -> (x, y - i)
            East  -> (x + i, y)
            West  -> (x - i, y)
    in (\x -> Pose { position = x, heading = heading'}) <$>
       [ step i | i <- [1..dist] ]

rotate :: Direction -> Heading -> Heading
rotate R West = North
rotate R h = succ h
rotate L North = West
rotate L h = pred h

-- | Compute the "taxicab distance" between a position and the origin
taxiDist :: Num a => Point a -> a
taxiDist (x, y) = abs x + abs y

main :: IO ()
main = do
    string <- getLine
    (print . taxiDist . position) .
        foldl move initial $
        (read . takeWhile notComma) <$> words string
    where initial = Pose { position = (0, 0), heading = North }
          notComma = (',' /= )
        --   moveFromStr ('R':xs) = Move { turn = R, dist = read xs}
        --   moveFromStr ('L':xs) = Move { turn = L, dist = read xs}
