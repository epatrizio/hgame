module HitboxSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Sequence (Seq (..), (!?))
import qualified Data.Sequence as S

import Coord as C
import Hitbox as H

-- Unit tests
hitboxUT = do
  describe "Hitbox - Unit tests" $ do
    it "prop_inv_hb_notEmpty - KO null sizes" $
      prop_inv_hb_notEmpty (Rect (Coord 0 0) 0 0) `shouldBe` False
    it "prop_inv_hb_notEmpty - KO empty sequence" $
      prop_inv_hb_notEmpty (Composite S.empty) `shouldBe` False
    it "prop_inv_hb_notEmpty - OK one rectangle" $
      prop_inv_hb_notEmpty (Rect (Coord 0 10) 5 5) `shouldBe` True
    it "prop_inv_hb_notEmpty - OK not empty sequence" $
      prop_inv_hb_notEmpty (Composite (S.fromList [(Rect (Coord 0 10) 5 5)])) `shouldBe` True
