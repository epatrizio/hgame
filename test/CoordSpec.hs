module CoordSpec where

import Test.Hspec
import Test.QuickCheck

import Coord as C

genIntPositive :: Bool -> Gen Integer
genIntPositive True = choose (1, 1000)
genIntPositive False = choose (0, 1000)

genIntNegative :: Bool -> Gen Integer
genIntNegative True = choose (-1000, -1)
genIntNegative False = choose (-1000, 0)

genZoneOk :: Gen Zone
genZoneOk = do
  w <- choose (1, 100)
  h <- choose (1, 100)
  return (Zone w h)

genCoordInZone :: Zone -> Gen Coordinates
genCoordInZone (Zone w h) = do
  x <- choose (0, w)
  y <- choose (0, h)
  return (Coord x y)

genCoordOutZone :: Zone -> Gen Coordinates
genCoordOutZone (Zone w h) = do
  x <- choose (w+1, w+1000)
  y <- choose (h+1, h+1000)
  return (Coord x y)

genMovOk :: Integer -> Gen Movement
genMovOk i = do
  m <- choose (0, i)
  d <- elements [U,D,R,L]
  return (Mov d m)

-- Unit tests
coordUT = do
  describe "Coord - Unit tests createZone" $ do
    it "createZone OK" $
      createZone 10 20 `shouldBe` Zone 10 20
    it "createZone default dim 1 OK" $
      createZone 0 0 `shouldBe` Zone 640 480
    it "createZone default dim 2 OK" $
      createZone (-10) (-20) `shouldBe` Zone 640 480
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
      moveSafe (Zone 50 50) (Coord 10 0) (Mov L 10) `shouldBe` (Coord 0 0)
    it "Up movement KO" $
      moveSafe (Zone 5 5) (Coord 0 0) (Mov U 10) `shouldBe` (Coord 0 0)
    it "Down movement KO" $
      moveSafe (Zone 5 5) (Coord 0 0) (Mov D 10) `shouldBe` (Coord 0 0)
    it "Right movement KO" $
      moveSafe (Zone 5 5) (Coord 0 0) (Mov R 10) `shouldBe` (Coord 0 0)
    it "Left movement OK" $
      moveSafe (Zone 5 5) (Coord 0 0) (Mov L 10) `shouldBe` (Coord 0 0)

-- QuickCheck auto tests
coordQCT = do
  describe "Coord - QuickCheck prop" $ do
    it "createZone OK" $ property $
      forAll (genIntPositive True) $ \w -> forAll (genIntPositive True) $ \h -> (createZone w h) == (Zone w h)
    it "createZone default" $ property $
      forAll (genIntNegative False) $ \w -> forAll (genIntNegative False) $ \h -> (createZone w h) == (Zone 640 480)
    it "prop_inv_zone_coord OK" $ property $
      forAll (genCoordInZone (Zone 100 50)) $ prop_inv_zone_coord (Zone 100 50)
    it "prop_inv_zone_coord KO" $ property $
      forAll (genCoordOutZone (Zone 100 50)) $ \c -> not (prop_inv_zone_coord (Zone 100 50) c)
    it "prop_move_leftRight" $ property $
      \x y i -> prop_move_leftRight (Coord x y) i
    it "prop_move_upDown" $ property $
      \x y i -> prop_move_upDown (Coord x y) i
  describe "Coord - QuickCheck moveSafe" $ do
    it "prop_moveSafe_in_zone in" $ property $
      forAll genZoneOk $ \z ->
        forAll (genCoordInZone z) $ \c ->
          forAll (genMovOk 1000) $ \m -> prop_moveSafe_in_zone z c m
    it "prop_moveSafe_in_zone out" $ property $
      forAll genZoneOk $ \z ->
        forAll (genCoordOutZone z) $ \c ->
          forAll (genMovOk 1000) $ \m -> not (prop_moveSafe_in_zone z c m)
