module Coord where -- ( someFunc )

data Zone = Zone Integer Integer    -- Width (-w +w) and Height (0 +h)
    deriving (Show, Eq)

data Coordinates = Coord Integer Integer
    deriving (Show, Eq)

data Direction = U | D | R | L      -- U (Up) | D (Down) | R (Right) | L (Left)
    deriving (Show, Eq)

data Movement = Mov Direction Integer
    deriving (Show, Eq)

-- Width & Height > 0
prop_inv_zone :: Zone -> Bool
prop_inv_zone (Zone w h) = w>0 && h>0

-- Coordinates must be inside Zone
prop_inv_zone_coord :: Zone -> Coordinates -> Bool
prop_inv_zone_coord (Zone w h) (Coord x y) = x>=(-w) && x<=w && y>=0 && y<=h

-- Int nb mvt > 0
prop_inv_movement :: Movement -> Bool
prop_inv_movement (Mov _ n) = n>=0

move :: Coordinates -> Movement -> Coordinates
move (Coord x y) (Mov U u) = Coord x (y+u)
move (Coord x y) (Mov D d) = Coord x (y-d)
move (Coord x y) (Mov R r) = Coord (x+r) y
move (Coord x y) (Mov L l) = Coord (x-l) y

prop_move_leftRight :: Coordinates -> Integer -> Bool
prop_move_leftRight (Coord x y) i = move (move (Coord x y) (Mov R i)) (Mov L i) == (Coord x y)

prop_move_upDown :: Coordinates -> Integer -> Bool
prop_move_upDown (Coord x y) i = move (move (Coord x y) (Mov U i)) (Mov D i) == (Coord x y)

-- moveSafe :: Coordinates -> Movement -> Zone -> Maybe Coordinates
-- Q: au lieu de ne pas bouger, aller au bout et s'arrêter ?
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
    | x-l >= (-w) = move (Coord x y) (Mov L l)
    | otherwise = Coord x y

-- check that movement does not go outside zone
prop_moveSafe :: Zone -> Coordinates -> Movement -> Bool
prop_moveSafe (Zone _ h) (Coord x y) (Mov U u) = y+u <= h
prop_moveSafe _ (Coord x y) (Mov D d) = y-d >= 0
prop_moveSafe (Zone w _) (Coord x y) (Mov R r) = x+r <= w
prop_moveSafe (Zone w _) (Coord x y) (Mov L l) = x-l >= (-w)

-- check that always remains in zone
prop_moveSafe_in_zone :: Zone -> Coordinates -> Movement -> Bool
prop_moveSafe_in_zone z c m = (prop_inv_zone_coord z c) && (prop_inv_zone_coord z (moveSafe z c m))
