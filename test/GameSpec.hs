module GameSpec where

import Test.Hspec
import Test.QuickCheck

import Game as G

-- QuickCheck auto tests
gameQCT = do
  describe "Game - QuickCheck tests" $ do
    it "createGameState" $ property $
      \name1 -> collect (length name1) $ \name2 -> collect (length name2) $
        prop_inv_gameState (createGameState name1 name2)
