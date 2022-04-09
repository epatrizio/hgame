{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Coord (Coordinates (..))
import qualified Coord as C

import Game (GameState, Fighter (..))
import qualified Game as G

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadFighter :: String -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadFighter sid rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId sid) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId sid) (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId sid) sprite smap
  return (tmap', smap')

main :: IO ()
main = do
  initializeAll
  window <- createWindow "PAF Project - Street Fighter 2" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- load assets
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  (tmap1', smap1') <- loadFighter "fighter1" renderer "assets/perso.bmp" tmap smap
  (tmap2', smap2') <- loadFighter "fighter2" renderer "assets/perso.bmp" tmap1' smap1'
  -- init game
  let gameState = G.createGameState
  let kbd = K.createKeyboard
  gameLoop 60 renderer tmap2' smap2' kbd gameState

-- ToDo : GameOver > end loop
gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> IO ()
gameLoop frameRate renderer tmap smap kbd (G.GameIn (Fighter i1 n1 (Coord x1 y1) h1 d1 s1) (Fighter i2 n2 (Coord x2 y2) h2 d2 s2) z speed) = do
  startTime <- time
  events <- pollEvents
  --putStrLn $ "x: " <> (show x1) <> " y: " <> (show y1)
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "fighter1") smap)
                                 (fromIntegral x1)
                                 (fromIntegral y1))
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "fighter2") smap)
                                (fromIntegral x2)
                                (fromIntegral y2))
  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  --- update du game state
  let gameState' = G.gameStep (G.GameIn (Fighter i1 n1 (Coord x1 y1) h1 d1 s1) (Fighter i2 n2 (Coord x2 y2) h2 d2 s2) z speed) kbd' deltaTime
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState')
