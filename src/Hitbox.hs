module Hitbox where

import Data.Sequence (Seq (..), (!?))
import qualified Data.Sequence as S

import Coord

data Hitbox =
    Rect Coordinates Integer Integer    -- Rectangle Coord (point on the top left) + w / h
    | Composite (Seq Hitbox)

instance Show Hitbox where
    show (Rect c w h) = "Top left:" <> (show c) <> " rect dim = w:" <> (show w) <> " h:" <> (show h)
    show (Composite shb) = "HB1 (Normal): " <> (show (S.index shb 0)) <> " - (Kick): " <> (show (S.index shb 1))

prop_inv_hb :: Hitbox -> Bool
prop_inv_hb (Rect c w h) = prop_inv_coordinates c && w>0 && h>0
prop_inv_hb (Composite s) = case s of
    Empty -> False
    _ -> True

prop_inv_hitbox :: Hitbox -> Bool
prop_inv_hitbox (Rect _ _ _) = False
prop_inv_hitbox (Composite s) = (S.length s)==2 && foldr (\rect res -> res && (prop_inv_hb rect)) True s

-- Warning : sizes are hardcoded
createHitbox :: Integer -> Integer -> Integer -> Hitbox
createHitbox 1 x y = Composite (S.fromList [(Rect (Coord x y) 80 160), (Rect (Coord x y) 110 160)])
createHitbox 2 x y = Composite (S.fromList [(Rect (Coord x y) 80 160), (Rect (Coord (x-30) y) 110 160)])

moveHitbox :: Integer -> Coordinates -> Hitbox
moveHitbox fid (Coord x y) = createHitbox fid x y

rectIntersect :: Hitbox -> Hitbox -> Bool
rectIntersect (Rect (Coord x1 y1) w1 h1) (Rect (Coord x2 y2) w2 h2) = not ( x1+w1<x2 || x2+w2<x1 || y1+h1<y2 || y2+h2<y1 )
rectIntersect _ _ = False

touchHitbox :: Hitbox -> Hitbox -> Bool
touchHitbox (Composite shb1) (Composite shb2) =
    let (Rect r1topLeft w1 h1) = S.index shb1 1 in
    let (Rect r2topLeft w2 h2) = S.index shb2 0 in
        rectIntersect (Rect r1topLeft w1 h1) (Rect r2topLeft w2 h2)
