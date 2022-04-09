module Game where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

import Coord
import Hitbox

data FighterState  = KO | OK Integer    -- Integer > Life points

newtype FighterId = FighterId Integer
    deriving (Eq,Ord)

data Fighter = Fighter {
    idF :: FighterId,
    nameF :: String,
    posF :: Coordinates,
    hitboxF :: Hitbox,
    dirF :: Direction,
    stateF :: FighterState
}

data GameState =
    GameOver FighterId    -- Winner id
    | GameIn {
        fighter1 :: Fighter,
        fighter2 :: Fighter,
        gameZone :: Zone,
        speed :: Int
    }

instance Show FighterId where
    show (FighterId id) = show id

-- ToDo invariant gameIn -> FighterState OK

createFighter :: Integer -> String -> Integer -> Integer -> Hitbox -> Direction -> Fighter
createFighter id name x y h d = Fighter (FighterId id) name (Coord x y) h d (OK 10)

createGameState :: GameState
createGameState =
    GameIn
        (createFighter 1 "Fighter 1" 200 250 (createHitbox 0 0 0 0) R)
        (createFighter 2 "Fighter 2" 400 250 (createHitbox 0 0 0 0) L)
        (createZone 0 0)
        5

moveD :: Int -> Direction -> GameState -> GameState
moveD _ _ (GameOver fid) = GameOver fid
moveD fid dir (GameIn (Fighter i1 n1 c1 h1 d1 s1) (Fighter i2 n2 c2 h2 d2 s2) z s) =
    case fid of
        1 -> GameIn (Fighter i1 n1 (moveSafe z c1 (Mov dir 1)) h1 d1 s1) (Fighter i2 n2 c2 h2 d2 s2) z s
        _ -> GameIn (Fighter i1 n1 c1 h1 d1 s1) (Fighter i2 n2 (moveSafe z c2 (Mov dir 1)) h2 d2 s2) z s

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif =   (if K.keypressed KeycodeLeft kbd
                then moveD 1 L else id)
                .
                (if K.keypressed KeycodeRight kbd
                then moveD 1 R else id)
                .
                (if K.keypressed KeycodeUp kbd
                then moveD 1 D else id)
                .
                (if K.keypressed KeycodeDown kbd
                then moveD 1 U else id)
                .
                (if K.keypressed KeycodeQ kbd
                then moveD 2 L else id)
                .
                (if K.keypressed KeycodeS kbd
                then moveD 2 R else id)
                .
                (if K.keypressed KeycodeZ kbd
                then moveD 2 D else id)
                .
                (if K.keypressed KeycodeW kbd
                then moveD 2 U else id)

  in modif gstate
