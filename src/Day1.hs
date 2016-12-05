{-# LANGUAGE NamedFieldPuns #-}
module Day1 where
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
                 , heading  :: !Heading
                 }

data Move = Move { turn :: !Direction
                 , dist :: !Integer
                 }

move :: Move -> Pose -> Pose
move Move { turn, dist } Pose { position = (x, y), heading } =
        Pose { position = position', heading = heading' }
    where heading' = rotate turn heading
          position' = case heading' of
              North -> (x, y + dist)
              South -> (x, y - dist)
              East -> (x - dist, y)
              West -> (x + dist, y)

rotate :: Direction -> Heading -> Heading
rotate R West = North
rotate R h = succ h
rotate L North = West
rotate L h = pred h


-- | Compute the "taxicab distance" between two vectors `p` and `q`.
taxiDist :: Num a => Point a -> Point a -> a
taxiDist (px, py) (qx, qy) = abs (px - qx) + abs (py - qy)
