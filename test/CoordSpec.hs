module CoordSpec where

import Test.Hspec
import Test.QuickCheck

import Coord as C

-- Unit tests
coordUT = do
  describe "Coord - Unit tests move" $ do
    it "Up movement" $
      move (Coord 0 0) (Mov U 10) `shouldBe` (Coord 0 10)
    it "Down movement" $
      move (Coord 0 0) (Mov D 10) `shouldBe` (Coord 0 (-10))
    it "Right movement" $
      move (Coord 0 0) (Mov R 10) `shouldBe` (Coord 10 0)
    it "Left movement" $
      move (Coord 0 0) (Mov L 10) `shouldBe` (Coord (-10) 0)
  describe "Coord - Unit tests moveSafe" $ do
    it "Up movement OK" $
      moveSafe (Zone 50 50) (Coord 0 0) (Mov U 10) `shouldBe` (Coord 0 10)
    it "Down movement OK" $
      moveSafe (Zone 50 50) (Coord 0 10) (Mov D 10) `shouldBe` (Coord 0 0)
    it "Right movement OK" $
      moveSafe (Zone 50 50) (Coord 0 0) (Mov R 10) `shouldBe` (Coord 10 0)
    it "Left movement OK" $
      moveSafe (Zone 50 50) (Coord 0 0) (Mov L 10) `shouldBe` (Coord (-10) 0)
    it "Up movement KO" $
      moveSafe (Zone 5 5) (Coord 0 0) (Mov U 10) `shouldBe` (Coord 0 0)
    it "Down movement KO" $
      moveSafe (Zone 5 5) (Coord 0 0) (Mov D 10) `shouldBe` (Coord 0 0)
    it "Right movement KO" $
      moveSafe (Zone 5 5) (Coord 0 0) (Mov R 10) `shouldBe` (Coord 0 0)
    it "Left movement OK" $
      moveSafe (Zone 5 5) (Coord 0 0) (Mov L 10) `shouldBe` (Coord 0 0)

genZoneOk :: Gen Zone
genZoneOk = do
  w <- choose (1, 100)
  h <- choose (1, 100)
  return (Zone w h)

genZoneKo :: Gen Zone
genZoneKo = do
  w <- choose (-100, 0)
  h <- choose (-100, 0)
  return (Zone w h)

genCoordOk :: Gen Coordinates
genCoordOk = do
  x <- choose (-50, 50)
  y <- choose (0, 100)
  return (Coord x y)

genMovUOk :: Integer -> Gen Movement
genMovUOk i = do
  m <- choose (0, i)
  return (Mov U m)

genMovDOk :: Integer -> Gen Movement
genMovDOk i = do
  m <- choose (0, i)
  return (Mov D m)

genMovROk :: Integer -> Gen Movement
genMovROk i = do
  m <- choose (0, i)
  return (Mov R m)

genMovLOk :: Integer -> Gen Movement
genMovLOk i = do
  m <- choose (0, i)
  return (Mov L m)

-- QuickCheck auto tests
coordQCT = do
  describe "Coord - QuickCheck prop" $ do
    it "prop_inv_zone OK" $ property $
      forAll genZoneOk $ prop_inv_zone
    it "prop_inv_zone KO" $ property $
      forAll genZoneKo $ \z -> not (prop_inv_zone z)
    it "prop_inv_zone_coord" $ property $
      forAll genCoordOk $ prop_inv_zone_coord (Zone 50 100)
    it "prop_inv_movement Up" $ property $
      forAll (genMovUOk 100) $ prop_inv_movement
    it "prop_inv_movement Down" $ property $
      forAll (genMovDOk 100) $ prop_inv_movement
    it "prop_inv_movement Right" $ property $
      forAll (genMovROk 100) $ prop_inv_movement
    it "prop_inv_movement Left" $ property $
      forAll (genMovLOk 100) $ prop_inv_movement
    it "prop_move_leftRight" $ property $
      \x y i -> prop_move_leftRight (Coord x y) i
    it "prop_move_upDown" $ property $
      \x y i -> prop_move_upDown (Coord x y) i
  describe "Coord - QuickCheck moveSafe" $ do
    it "prop_moveSafe Up" $ property $
      forAll (genMovUOk 100) $ \z -> prop_moveSafe (Zone 50 100) (Coord 0 0) z
    it "prop_moveSafe Down" $ property $
      forAll (genMovDOk 100) $ \z -> prop_moveSafe (Zone 50 100) (Coord 0 100) z
    it "prop_moveSafe Right" $ property $
      forAll (genMovROk 100) $ \z -> prop_moveSafe (Zone 50 100) (Coord (-50) 0) z
    it "prop_moveSafe Left" $ property $
      forAll (genMovLOk 100) $ \z -> prop_moveSafe (Zone 50 100) (Coord 50 0) z
    it "prop_moveSafe_in_zone Up" $ property $
      forAll (genMovUOk 1000) $ \z -> prop_moveSafe_in_zone (Zone 10 20) (Coord 0 0) z
    it "prop_moveSafe_in_zone Down" $ property $
      forAll (genMovDOk 1000) $ \z -> prop_moveSafe_in_zone (Zone 10 20) (Coord 0 0) z
    it "prop_moveSafe_in_zone Right" $ property $
      forAll (genMovROk 1000) $ \z -> prop_moveSafe_in_zone (Zone 10 20) (Coord 0 0) z
    it "prop_moveSafe_in_zone Left" $ property $
      forAll (genMovLOk 1000) $ \z -> prop_moveSafe_in_zone (Zone 10 20) (Coord 0 0) z

-- ToDo
-- KO tests

