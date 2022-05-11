module Coord where -- ( someFunc )

data Zone = Zone Integer Integer    -- Width (0 +w) and Height (0 +h)
    deriving (Show, Eq)

data Coordinates = Coord Integer Integer
    deriving (Eq)

data Direction = U | D | R | L      -- U (Up) | D (Down) | R (Right) | L (Left)
    deriving (Show, Eq)

data Movement = Mov Direction Integer
    deriving (Show, Eq)

instance Show Coordinates where
    show (Coord x y) = "Coord x:" <> (show x) <> " y:" <> (show y)

-- Width & Height > 0
prop_inv_zone :: Zone -> Bool
prop_inv_zone (Zone w h) = w>0 && h>0

-- X & Y > 0 (~ for ok Zone)
prop_inv_coordinates :: Coordinates -> Bool
prop_inv_coordinates (Coord x y) = x>=0 && y>=0

-- Coordinates must be inside Zone
prop_inv_zone_coord :: Zone -> Coordinates -> Bool
prop_inv_zone_coord (Zone w h) (Coord x y) =
    prop_inv_zone (Zone w h) && prop_inv_coordinates (Coord x y) && x<=w && y<=h

createZone :: Integer -> Integer -> Zone
createZone w h
    | w>0 && h>0 = Zone w h
    | otherwise = Zone 640 480   -- Default size if error

move :: Coordinates -> Movement -> Coordinates
move (Coord x y) (Mov U u) = Coord x (y+u)
move (Coord x y) (Mov D d) = Coord x (y-d)
move (Coord x y) (Mov R r) = Coord (x+r) y
move (Coord x y) (Mov L l) = Coord (x-l) y

prop_move_leftRight :: Coordinates -> Integer -> Bool
prop_move_leftRight (Coord x y) i = move (move (Coord x y) (Mov R i)) (Mov L i) == (Coord x y)

prop_move_upDown :: Coordinates -> Integer -> Bool
prop_move_upDown (Coord x y) i = move (move (Coord x y) (Mov U i)) (Mov D i) == (Coord x y)

-- if out of zone after movement, do not move!
moveSafe :: Zone -> Coordinates -> Movement -> Coordinates
moveSafe (Zone _ h) (Coord x y) (Mov U u)
    | y+u <= h = move (Coord x y) (Mov U u)
    | otherwise = Coord x y
moveSafe _ (Coord x y) (Mov D d)
    | y-d >= 0 = move (Coord x y) (Mov D d)
    | otherwise = Coord x y
moveSafe (Zone w _) (Coord x y) (Mov R r)
    | x+r <= w = move (Coord x y) (Mov R r)
    | otherwise = Coord x y
moveSafe (Zone w _) (Coord x y) (Mov L l)
    | x-l >= 0 = move (Coord x y) (Mov L l)
    | otherwise = Coord x y

-- Int nb mvt must be positive
prop_inv_movement :: Movement -> Bool
prop_inv_movement (Mov _ n) = n>=0

-- check that always remains in zone
prop_moveSafe_in_zone :: Zone -> Coordinates -> Movement -> Bool
prop_moveSafe_in_zone z c m =
    prop_inv_zone z && prop_inv_coordinates c && prop_inv_movement m &&
    prop_inv_zone_coord z c && prop_inv_zone_coord z (moveSafe z c m)
