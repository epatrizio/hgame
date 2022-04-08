import Test.Hspec

import CoordSpec as C
import HitboxSpec as H
-- import GameSpec as G

main :: IO ()
main = hspec $ do
  -- Coordinates
  C.coordUT
  C.coordQCT
  -- Hitbox
  H.hitboxUT
