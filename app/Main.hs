{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hFlush, stdout)

import System.Exit (exitSuccess)

import Data.Foldable (for_)

import Control.Monad.Reader
import Control.Monad.State
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

import Config (Configuration)
import qualified Config as Co

import Utils

-- Warning : Config is hardcoded
loadConfig :: IO Configuration
loadConfig = return (Co.Config { Co.screenW = 1024, Co.screenH = 531})

loadBackground :: Integer -> Integer -> Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground sw sh rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 (fromIntegral sw) (fromIntegral sh))
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadFighter :: String -> Integer -> Integer -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadFighter sid imgW imgH rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId sid) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId sid) (S.mkArea 0 0 (fromIntegral imgW) (fromIntegral imgH))
  let smap' = SM.addSprite (SpriteId sid) sprite smap
  return (tmap', smap')

banner :: IO ()
banner = do
  putStrLn ""
  putStrLn "-----------------------------------------"
  putStrLn "-- PAF Project 2022 - Street Fighter 2 --"
  putStrLn "-----------------------------------------"
  putStrLn "Fighter 1 keyboard = Q:Left S:Right Z:Jump W:Protect Space:Kick"
  putStrLn "Fighter 2 keyboard = <-:Left ->:Right Up:Jump Down:Protect Enter:Kick"
  putStrLn "You can now enter a custom name (Default is 'Fighter 1/2')"
  putStrLn "Let's go and be careful! ;)"

-- | Get fighter asset id by fighter id and fighter action
--  (4 actions, 2 fighters = 8 images)
fighterAssetId :: Integer -> G.FighterAction -> String
fighterAssetId 1 G.None = "fighter1"
fighterAssetId 1 G.Kick = "fighter1K"
fighterAssetId 1 G.Jump = "fighter1J"
fighterAssetId 1 G.Protect = "fighter1P"
fighterAssetId 2 G.None = "fighter2"
fighterAssetId 2 G.Kick = "fighter2K"
fighterAssetId 2 G.Jump = "fighter2J"
fighterAssetId 2 G.Protect = "fighter2P"

-- Warning : images gap are hardcoded
fighter2AssetPosX :: G.FighterAction -> Integer
fighter2AssetPosX G.Kick = 30   -- (= 110-80)
fighter2AssetPosX _ = 0

-- Warning : images gap are hardcoded
fighterAssetPosY :: G.FighterAction -> Integer
fighterAssetPosY G.Jump = 135   -- (Jump image height)
fighterAssetPosY _ = 0

askName :: String -> String -> (String -> String -> Valid String) -> IO String
askName part defaultStr validateStr = do
  putStrLn ""
  putStr $ part ++ " : "
  hFlush stdout
  str <- getLine
  case (validateStr str defaultStr) of
    Validation (Left (error,dStr)) -> do
      putStrLn $ error
      return dStr
    Validation (Right name) ->
      return name

main :: IO ()
main = do
  initializeAll
  conf <- loadConfig
  let sw = runReader (Co.getConf Co.screenW) conf
  let sh = runReader (Co.getConf Co.screenH) conf
  banner
  name1 <- askName "Fighter 1 name" "Fighter 1" validateString
  name2 <- askName "Fighter 2 name" "Fighter 2" validateString
  window <- createWindow "PAF Project - Street Fighter 2" $ defaultWindow { windowInitialSize = V2 (fromIntegral sw) (fromIntegral sh) }
  renderer <- createRenderer window (-1) defaultRenderer
  -- load assets
  (tmap, smap) <- loadBackground sw sh renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  (tmap1, smap1) <- loadFighter (fighterAssetId 1 G.None) 80 160 renderer "assets/fighter1.bmp" tmap smap
  (tmap1', smap1') <- loadFighter (fighterAssetId 1 G.Kick) 110 160 renderer "assets/fighter1K.bmp" tmap1 smap1
  (tmap1'', smap1'') <- loadFighter (fighterAssetId 1 G.Jump) 80 135 renderer "assets/fighter1J.bmp" tmap1' smap1'
  (tmap1''', smap1''') <- loadFighter (fighterAssetId 1 G.Protect) 80 160 renderer "assets/fighter1P.bmp" tmap1'' smap1''
  (tmap2, smap2) <- loadFighter (fighterAssetId 2 G.None) 80 160 renderer "assets/fighter2.bmp" tmap1''' smap1'''
  (tmap2', smap2') <- loadFighter (fighterAssetId 2 G.Kick) 110 160 renderer "assets/fighter2K.bmp" tmap2 smap2
  (tmap2'', smap2'') <- loadFighter (fighterAssetId 2 G.Jump) 80 135 renderer "assets/fighter2J.bmp" tmap2' smap2'
  (tmap2''', smap2''') <- loadFighter (fighterAssetId 2 G.Protect) 80 160 renderer "assets/fighter2P.bmp" tmap2'' smap2''
  let gameState = G.createGameState sw sh name1 name2
  let kbd = K.createKeyboard
  gameLoop 60 renderer tmap2''' smap2''' kbd [] gameState

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Log -> GameState -> IO ()
gameLoop _ _ _ _ _ log (G.GameOver fid) = do
  putStrLn ""
  putStrLn $ "\ESC[34m" <> show (G.GameOver fid)
  putStrLn ""
  putStrLn $ "\ESC[0m--- Game history ---"
  for_ (fmap id (reverse log)) putStrLn
  exitSuccess
gameLoop frameRate renderer tmap smap kbd log g@(G.GameIn f1@(Fighter i1 n1 (Coord x1 y1) h1 d1 a1 (G.OK l1) t1) f2@(Fighter i2 n2 (Coord x2 y2) h2 d2 a2 (G.OK l2) t2) z speed print) = do
  startTime <- time
  events <- pollEvents
  let (_,log') =  if (print) then (runState (pushLog (show g)) log) else ((),log)
  if (print) then do
    putStrLn ""
    putStrLn $ "\ESC[0mFight in progress"
    putStrLn $ (if l1 < l2 then "\ESC[31m" else "\ESC[0m") <> show f1
    putStrLn $ (if l2 < l1 then "\ESC[31m" else "\ESC[0m") <> show f2
  else
    pure ()
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (fighterAssetId 1 a1)) smap)
                                 (fromIntegral x1)
                                 (fromIntegral (y1 - (fighterAssetPosY a1))))
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (fighterAssetId 2 a2)) smap)
                                (fromIntegral (x2 - (fighter2AssetPosX a2)))
                                (fromIntegral (y2 - (fighterAssetPosY a2))))
  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  --- update gameState
  let gameState' = G.gameStep (G.GameIn f1 f2 z speed False) kbd' deltaTime
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' log' gameState')
