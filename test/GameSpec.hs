module GameSpec where

import Test.Hspec
import Test.QuickCheck

import Coord as C
import Game as G
import Hitbox as H

genDir :: Gen Direction
genDir = elements [U,D,R,L]

-- Unit tests
gameUT = do
  describe "Game - Unit tests" $ do
    it "Specific createGameState" $ do
      let GameIn f1 f2 z s i = createGameState 1024 531 "name1" "name2" in
        prop_inv_gameState (GameIn f1 f2 z s i) &&
        prop_inv_hitbox (hitboxF f1) &&
        prop_inv_hitbox (hitboxF f2) &&
        prop_inv_zone_hitbox z (hitboxF f1) &&
        prop_inv_zone_hitbox z (hitboxF f2)
          `shouldBe` True
    it "Specific action KO" $
      let GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 (OK l2)) z s i = createGameState 1024 531 "name1" "name2" in
      let GameIn af1 (Fighter ai2 an2 ac2 ah2 ad2 aa2 (OK al2)) az as ai = action 1 Kick (GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 (OK l2)) z s i) in
        al2 `shouldBe` l2
    it "Specific action OK" $
      let GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 (OK l2)) z s i = createGameState 1024 531 "name1" "name2" in
      let GameIn af1 (Fighter ai2 an2 ac2 ah2 ad2 aa2 (OK al2)) az as ai = action 1 Kick (GameIn f1 (Fighter i2 n2 c2 (moveHitbox 2 (Coord 350 350)) d2 a2 (OK l2)) z s i) in
        al2 `shouldBe` (l2-1)

-- QuickCheck auto tests
gameQCT = do
  describe "Game - QuickCheck tests" $ do
    it "createGameState" $ property $
      \name1 -> collect (length name1) $ \name2 -> collect (length name2) $
        prop_inv_gameState (createGameState 1024 531 name1 name2)
    it "moveD" $ property $
      forAll (oneof [return 1, return 2]) $ \fid -> forAll genDir $ \dir -> \name1 name2 ->
        let GameIn f1 f2 z s i = moveD fid dir (createGameState 1024 531 name1 name2) in
          prop_inv_gameState (GameIn f1 f2 z s i) &&
          prop_inv_hitbox (hitboxF f1) &&
          prop_inv_hitbox (hitboxF f2) &&
          prop_inv_zone_hitbox z (hitboxF f1) &&
          prop_inv_zone_hitbox z (hitboxF f2)
