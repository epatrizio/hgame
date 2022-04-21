{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit (exitSuccess)

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
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 1024 531)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadFighter :: String -> Integer -> Integer -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadFighter sid imgW imgH rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId sid) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId sid) (S.mkArea 0 0 (fromIntegral imgW) (fromIntegral imgH))
  let smap' = SM.addSprite (SpriteId sid) sprite smap
  return (tmap', smap')

fighterAssetId :: Integer -> G.FighterAction -> String
fighterAssetId 1 G.None = "fighter1"
fighterAssetId 1 G.Kick = "fighter1K"
fighterAssetId 2 G.None = "fighter2"
fighterAssetId _ _ = "fighter2K"

fighter2AssetPosX :: G.FighterAction -> Integer
fighter2AssetPosX G.None = 0
fighter2AssetPosX G.Kick = 30   -- (= 110-80)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "PAF Project - Street Fighter 2" $ defaultWindow { windowInitialSize = V2 1024 531 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- load assets
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  (tmap1, smap1) <- loadFighter (fighterAssetId 1 G.None) 80 160 renderer "assets/fighter1.bmp" tmap smap
  (tmap1', smap1') <- loadFighter (fighterAssetId 1 G.Kick) 110 160 renderer "assets/fighter1K.bmp" tmap1 smap1
  (tmap2, smap2) <- loadFighter (fighterAssetId 2 G.None) 80 160 renderer "assets/fighter2.bmp" tmap1' smap1'
  (tmap2', smap2') <- loadFighter (fighterAssetId 2 G.Kick) 110 160 renderer "assets/fighter2K.bmp" tmap2 smap2
  -- init game (#ToDo : sizes, default positions in argument)
  let gameState = G.createGameState "Fighter 1" "Fighter 2"
  let kbd = K.createKeyboard
  gameLoop 60 renderer tmap2' smap2' kbd gameState

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> IO ()
gameLoop _ _ _ _ _ (G.GameOver fid) = do
  putStrLn $ "\ESC[32m" <> show (G.GameOver fid)
  exitSuccess
gameLoop frameRate renderer tmap smap kbd (G.GameIn (Fighter i1 n1 (Coord x1 y1) h1 d1 a1 (G.OK l1)) (Fighter i2 n2 (Coord x2 y2) h2 d2 a2 (G.OK l2)) z speed print) = do
  startTime <- time
  events <- pollEvents
  if (print) then do
    putStrLn $ "\ESC[0mFight in progress"
    putStrLn $ (if l1 < l2 then "\ESC[31m" else "\ESC[0m") <> show (Fighter i1 n1 (Coord x1 y1) h1 d1 a1 (G.OK l1))
    putStrLn $ (if l2 < l1 then "\ESC[31m" else "\ESC[0m") <> show (Fighter i2 n2 (Coord x2 y2) h2 d2 a2 (G.OK l2))
    putStrLn $ ""
  else
    pure ()
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (fighterAssetId 1 a1)) smap)
                                 (fromIntegral x1)
                                 (fromIntegral y1))
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (fighterAssetId 2 a2)) smap)
                                (fromIntegral (x2 - (fighter2AssetPosX a2)))
                                (fromIntegral y2))
  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  --- update gameState
  let gameState' = G.gameStep (G.GameIn (Fighter i1 n1 (Coord x1 y1) h1 d1 a1 (G.OK l1)) (Fighter i2 n2 (Coord x2 y2) h2 d2 a2 (G.OK l2)) z speed False) kbd' deltaTime
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState')
