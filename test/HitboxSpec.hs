module HitboxSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Sequence (Seq (..), (!?))
import qualified Data.Sequence as S

import Coord as C
import Hitbox as H

genIntPositive :: Integer -> Gen Integer
genIntPositive i = choose (i, 1000)

hb_f1 :: Hitbox
hb_f1 = createHitbox 1 0 0

hb_f21 :: Hitbox
hb_f21 = createHitbox 2 100 100

hb_f22 :: Hitbox
hb_f22 = createHitbox 2 10 10

genRect :: Integer -> Integer -> Integer -> Integer -> Gen Hitbox
genRect x1 y1 w1 h1 = do
  x <- choose (x1, x1+w1)
  y <- choose (y1, y1+h1)
  w <- choose (1, 100)
  h <- choose (1, 100)
  return (Rect (Coord x y) w h)

-- Unit tests
hitboxUT = do
  describe "Hitbox - Unit tests" $ do
    it "prop_inv_hb - KO null sizes" $
      prop_inv_hb (Rect (Coord 0 0) 0 0) `shouldBe` False
    it "prop_inv_hb - KO incorrect coordinates" $
      prop_inv_hb (Rect (Coord (-1) (-1)) 10 10) `shouldBe` False
    it "prop_inv_hb - KO empty sequence" $
      prop_inv_hb (Composite S.empty) `shouldBe` False
    it "prop_inv_hb - OK one rectangle" $
      prop_inv_hb (Rect (Coord 0 0) 5 5) `shouldBe` True
    it "prop_inv_hb - OK not empty sequence" $
      prop_inv_hb (Composite (S.fromList [(Rect (Coord 0 0) 5 5)])) `shouldBe` True
    it "touchHitbox - not f1 f21" $
      touchHitbox hb_f1 hb_f21 `shouldBe` False
    it "touchHitbox - not f1 f21" $
      touchHitbox hb_f21 hb_f1 `shouldBe` False
    it "touchHitbox - f1 f22" $
      touchHitbox hb_f1 hb_f22 `shouldBe` True
    it "touchHitbox - f1 f22" $
      touchHitbox hb_f22 hb_f1 `shouldBe` True

-- Tester le cerateHB

hitboxQCT = do
  describe "Hitbox - QuickCheck tests" $ do
    it "createHitbox 1" $
      forAll (genIntPositive 0) $ \x -> forAll (genIntPositive 0) $ \y -> prop_inv_hitbox (createHitbox 1 x y)
    it "createHitbox 2" $   -- 22 Hardcoded :(
      forAll (genIntPositive 22) $ \x -> forAll (genIntPositive 0) $ \y -> prop_inv_hitbox (createHitbox 2 x y)
    it "rectIntersect 1" $
      forAll (genRect 0 0 100 100) $ \(Rect (Coord x1 y1) w1 h1) ->
        forAll (genRect x1 y1 w1 h1) $ \r2 ->
          rectIntersect (Rect (Coord x1 y1) w1 h1) r2
    it "rectIntersect 2" $
      forAll (genRect 0 0 100 100) $ \(Rect (Coord x1 y1) w1 h1) ->
        forAll (genRect x1 y1 w1 h1) $ \r2 ->
          rectIntersect r2 (Rect (Coord x1 y1) w1 h1)
    it "rectIntersect not 1" $
      forAll (genRect 0 0 100 100) $ \(Rect (Coord x1 y1) w1 h1) ->
        forAll (genRect (x1+w1+1) 0 100 100) $ \r2 ->
          not (rectIntersect (Rect (Coord x1 y1) w1 h1) r2)
    it "rectIntersect not 2" $
      forAll (genRect 0 0 100 100) $ \(Rect (Coord x1 y1) w1 h1) ->
        forAll (genRect 0 (y1+h1+1) 100 100) $ \r2 ->
          not (rectIntersect (Rect (Coord x1 y1) w1 h1) r2)
    it "rectIntersect not 3" $
      forAll (genRect 0 0 100 100) $ \(Rect (Coord x1 y1) w1 h1) ->
        forAll (genRect (x1+w1+1) 0 100 100) $ \r2 ->
          not (rectIntersect r2 (Rect (Coord x1 y1) w1 h1))
    it "rectIntersect not 4" $
      forAll (genRect 0 0 100 100) $ \(Rect (Coord x1 y1) w1 h1) ->
        forAll (genRect 0 (y1+h1+1) 100 100) $ \r2 ->
          not (rectIntersect r2 (Rect (Coord x1 y1) w1 h1))
