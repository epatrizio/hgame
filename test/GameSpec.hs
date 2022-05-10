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
    it "Specific action KO (hb don't touch - action 1)" $ -- At the beginning, fighters don't touch each other
      let g@(GameIn { fighter2 = f2@(Fighter { stateF = OK l2 }) }) = createGameState 1024 531 "name1" "name2" in
      let GameIn { fighter2 = Fighter { stateF = OK al2 } } = action 1 Kick g in
        al2 `shouldBe` l2
    it "Specific action KO (hb don't touch - action 2)" $
      let g@(GameIn { fighter1 = f1@(Fighter { stateF = OK l1 }) }) = createGameState 1024 531 "name1" "name2" in
      let GameIn { fighter1 = Fighter { stateF = OK al1 } } = action 2 Kick g in
        al1 `shouldBe` l1
    it "Specific action OK (hb touch)" $ -- Move before action (for touch)
      let GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 (OK l2) t2) z s i = createGameState 1024 531 "name1" "name2" in
      let GameIn { fighter2 = Fighter { stateF = OK al2 } } = action 1 Kick (GameIn f1 (Fighter i2 n2 c2 (moveHitbox 2 (Coord 350 350)) d2 None (OK l2) t2) z s i) in
        al2 `shouldBe` (l2-1)
    it "Specific action KO (hb touch, but Protect)" $
      let GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 (OK l2) t2) z s i = createGameState 1024 531 "name1" "name2" in
      let GameIn { fighter2 = Fighter { stateF = OK al2 } } = action 1 Kick (GameIn f1 (Fighter i2 n2 c2 (moveHitbox 2 (Coord 350 350)) d2 Protect (OK l2) t2) z s i) in
        al2 `shouldBe` l2
    it "Specific action KO (hb touch, but Jump)" $
      let GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 (OK l2) t2) z s i = createGameState 1024 531 "name1" "name2" in
      let GameIn { fighter2 = Fighter { stateF = OK al2 } } = action 1 Kick (GameIn f1 (Fighter i2 n2 c2 (moveHitbox 2 (Coord 350 350)) d2 Jump (OK l2) t2) z s i) in
        al2 `shouldBe` l2
    it "Specific action OK (> GameOver fid:1 win)" $
      let GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 (OK l2) t2) z s i = createGameState 1024 531 "name1" "name2" in
      let GameOver (FighterId fid) = action 1 Kick (GameIn f1 (Fighter i2 n2 c2 (moveHitbox 2 (Coord 350 350)) d2 None (OK 1) t2) z s i) in
        fid `shouldBe` 1
    it "Specific action on GameOver" $
      let GameOver (FighterId fid) = action 1 Kick (GameOver (FighterId 1)) in fid `shouldBe` 1

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
