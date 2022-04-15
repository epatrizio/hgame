module Game where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

import Coord
import Hitbox

data FighterState = KO | OK Integer    -- Integer > Life points

data FighterAction = None | Kick

newtype FighterId = FighterId Integer
    deriving (Eq,Ord)

data Fighter = Fighter {
    idF :: FighterId,
    nameF :: String,
    posF :: Coordinates,
    hitboxF :: Hitbox,
    dirF :: Direction,
    actionF :: FighterAction,
    stateF :: FighterState
}

data GameState =
    GameOver FighterId    -- Winner id
    | GameIn {
        fighter1 :: Fighter,
        fighter2 :: Fighter,
        gameZone :: Zone,
        speed :: Int,
        printInfo :: Bool
    }

instance Show FighterState where
    show KO = "Dead!"
    show (OK life) = "Alive! -- " <> (show life) <> " life points"

instance Show FighterId where
    show (FighterId id) = "Fid:" <> show id

instance Show Fighter where
    show (Fighter id name _ _ _ _ st) = (show name) <> " (" <> (show id) <> ") current state: " <> (show st)

instance Show GameState where
    show (GameOver fid) = "GameOver! And the winner is the fighter " <> (show fid) <> " :)"
    show (GameIn f1 f2 _ _ _) = "Fight in progress >> fighter 1: " <> (show f1) <> " fighter 2: " <> (show f2)

-- #ToDo invariant gameIn -> FighterState OK

createFighter :: Integer -> String -> Integer -> Integer -> Hitbox -> Direction -> Fighter
createFighter id name x y h d = Fighter (FighterId id) name (Coord x y) h d None (OK 100)

createGameState :: String -> String -> GameState
createGameState name1 name2 =
    GameIn
        (createFighter 1 name1 200 250 (createHitbox 1 200 250) R)
        (createFighter 2 name2 400 250 (createHitbox 2 400 250) L)
        (createZone 0 0)    -- default size
        5
        True

-- #ToDo moveSafe : check hitbox size

moveD :: Integer -> Direction -> GameState -> GameState
moveD _ _ (GameOver fid) = GameOver fid
moveD 1 dir (GameIn (Fighter i1 n1 c1 h1 d1 a1 s1) f2 z s _) =
    let c = moveSafe z c1 (Mov dir 1) in GameIn (Fighter i1 n1 c (moveHitbox 1 c) d1 a1 s1) f2 z s False
moveD 2 dir (GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 s2) z s _) =
    let c = moveSafe z c2 (Mov dir 1) in GameIn f1 (Fighter i2 n2 c (moveHitbox 2 c) d2 a2 s2) z s False

action :: Integer -> FighterAction -> GameState -> GameState
action _ _ (GameOver fid) = GameOver fid
action 1 None (GameIn (Fighter i1 n1 c1 h1 d1 a1 s1) f2 z s _) = GameIn (Fighter i1 n1 c1 h1 d1 None s1) f2 z s False
action 2 None (GameIn f1 (Fighter i2 n2 c2 h2 d2 a2 s2) z s _) = GameIn f1 (Fighter i2 n2 c2 h2 d2 None s2) z s False
action 1 Kick (GameIn (Fighter i1 n1 c1 h1 d1 a1 s1) (Fighter i2 n2 c2 h2 d2 a2 (OK life)) z s _) =
    case touchHitbox h1 h2 of
        True
            | life <= 1 -> GameOver (FighterId 1)
            | otherwise -> GameIn (Fighter i1 n1 c1 h1 d1 Kick s1) (Fighter i2 n2 c2 h2 d2 a2 (OK (life-1))) z s True
        False -> GameIn (Fighter i1 n1 c1 h1 d1 Kick s1) (Fighter i2 n2 c2 h2 d2 a2 (OK life)) z s False
action 2 Kick (GameIn (Fighter i1 n1 c1 h1 d1 a1 (OK life)) (Fighter i2 n2 c2 h2 d2 a2 s2) z s _) =
    case touchHitbox h2 h1 of
        True
            | life <= 1 -> GameOver (FighterId 2)
            | otherwise -> GameIn (Fighter i1 n1 c1 h1 d1 a1 (OK (life-1))) (Fighter i2 n2 c2 h2 d2 Kick s2) z s True
        False -> GameIn (Fighter i1 n1 c1 h1 d1 a1 (OK life)) (Fighter i2 n2 c2 h2 d2 Kick s2) z s False

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep (GameOver fid) _ _ = GameOver fid
gameStep gstate kbd deltaTime =
  let modif =   (if K.keypressed KeycodeLeft kbd
                then moveD 2 L else id)
                .
                (if K.keypressed KeycodeRight kbd
                then moveD 2 R else id)
                .
                (if K.keypressed KeycodeUp kbd
                then moveD 2 D else id)
                .
                (if K.keypressed KeycodeDown kbd
                then moveD 2 U else id)
                .
                (if K.keypressed KeycodeReturn kbd
                then action 2 Kick else action 2 None)
                .
                (if K.keypressed KeycodeQ kbd
                then moveD 1 L else id)
                .
                (if K.keypressed KeycodeS kbd
                then moveD 1 R else id)
                .
                (if K.keypressed KeycodeZ kbd
                then moveD 1 D else id)
                .
                (if K.keypressed KeycodeW kbd
                then moveD 1 U else id)
                .
                (if K.keypressed KeycodeSpace kbd
                then action 1 Kick else action 1 None)

  in modif gstate
