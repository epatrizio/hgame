module Hitbox where

import Data.Sequence (Seq (..), (!?))
import qualified Data.Sequence as S

import Coord

data Hitbox =
    Rect Coordinates Integer Integer    -- Rectangle Coord (point on the top left) + w / h
    | Composite (Seq Hitbox)

prop_inv_hb_notEmpty :: Hitbox -> Bool
prop_inv_hb_notEmpty (Rect _ w h) = w>0 && h>0
prop_inv_hb_notEmpty (Composite s) = case s of
    Empty -> False
    _ -> True

