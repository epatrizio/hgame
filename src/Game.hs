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
    stateF :: FighterState,
    touchF :: Bool
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
    show (Fighter id name _ _ _ _ st _) = (show name) <> " (" <> (show id) <> ") current state: " <> (show st)

instance Show GameState where
    show (GameOver fid) = "GameOver! And the winner is the fighter " <> (show fid) <> " :)"
    show GameIn {
        fighter1 = Fighter { nameF = n1, stateF = (OK l1) },
        fighter2 = Fighter { nameF = n2, stateF = (OK l2) } } =
            (if l1 < l2 then "\ESC[31m" else "\ESC[32m") <> show n1 <> "\ESC[0m (" <> show l1 <> ") - " <>
            (if l2 < l1 then "\ESC[31m" else "\ESC[32m") <> show n2 <> "\ESC[0m (" <> show l2 <> ")"

prop_inv_fighterState :: FighterState -> Bool
prop_inv_fighterState KO = True
prop_inv_fighterState (OK life) = life > 0

prop_inv_fighterTouch :: Fighter -> Bool
prop_inv_fighterTouch Fighter { actionF = Kick } = True
prop_inv_fighterTouch Fighter { actionF = None, touchF = hasTouch } = not hasTouch

prop_inv_gameState :: GameState -> Bool
prop_inv_gameState (GameOver _) = True
prop_inv_gameState GameIn { fighter1 = Fighter { stateF=KO }} = False
prop_inv_gameState GameIn { fighter2 = Fighter { stateF=KO }} = False
prop_inv_gameState GameIn {
    fighter1 = f1@(Fighter { posF=c1, stateF=s1 }),
    fighter2 = f2@(Fighter { posF=c2, stateF=s2 }),
    gameZone = z} =
        prop_inv_fighterState s1 && prop_inv_fighterState s2 &&
        prop_inv_zone_coord z c1 && prop_inv_zone_coord z c2 &&
        prop_inv_fighterTouch f1 && prop_inv_fighterTouch f2

createFighter :: Integer -> String -> Integer -> Integer -> Hitbox -> Direction -> Fighter
createFighter id name x y h d = Fighter (FighterId id) name (Coord x y) h d None (OK 10) False

createGameState :: Integer -> Integer -> String -> String -> GameState
createGameState sw sh name1 name2 =
    GameIn
        (createFighter 1 name1 300 350 (createHitbox 1 300 350) R)
        (createFighter 2 name2 500 350 (createHitbox 2 500 350) L)
        (createZone sw sh)
        5
        True

moveD :: Integer -> Direction -> GameState -> GameState
moveD _ _ (GameOver fid) = GameOver fid
moveD 1 dir (GameIn f1@(Fighter i1 n1 c1 h1 d1 a1 s1 t1) f2 z s p) =
    let c = moveSafe z c1 (Mov dir 1) in
    let hb = moveHitbox 1 c in
        case prop_inv_zone_hitbox z hb of
            True -> GameIn (Fighter i1 n1 c hb d1 a1 s1 t1) f2 z s p
            False -> GameIn f1 f2 z s p
moveD 2 dir (GameIn f1 f2@(Fighter i2 n2 c2 h2 d2 a2 s2 t2) z s p) =
    let c = moveSafe z c2 (Mov dir 1) in
    let hb = moveHitbox 2 c in
        case prop_inv_zone_hitbox z hb of
            True -> GameIn f1 (Fighter i2 n2 c hb d2 a2 s2 t2) z s p
            False -> GameIn f1 f2 z s p

action :: Integer -> FighterAction -> GameState -> GameState
action _ _ (GameOver fid) = GameOver fid
action 1 None (GameIn (Fighter i1 n1 c1 h1 d1 _ s1 _) f2 z s p) = GameIn (Fighter i1 n1 c1 h1 d1 None s1 False) f2 z s p
action 2 None (GameIn f1 (Fighter i2 n2 c2 h2 d2 _ s2 _) z s p) = GameIn f1 (Fighter i2 n2 c2 h2 d2 None s2 False) z s p
action 1 Kick (GameIn (Fighter i1 n1 c1 h1 d1 _ s1 t1) f2@(Fighter i2 n2 c2 h2 d2 a2 (OK life) t2) z s _) =
    case t1 of
        True -> GameIn (Fighter i1 n1 c1 h1 d1 Kick s1 True) f2 z s False
        False -> case touchHitbox h1 h2 of
            True
              | life <= 1 -> GameOver (FighterId 1)
              | otherwise -> GameIn (Fighter i1 n1 c1 h1 d1 Kick s1 True) (Fighter i2 n2 c2 h2 d2 a2 (OK (life-1)) t2) z s True
            False -> GameIn (Fighter i1 n1 c1 h1 d1 Kick s1 False) f2 z s False
action 2 Kick (GameIn f1@(Fighter i1 n1 c1 h1 d1 a1 (OK life) t1) (Fighter i2 n2 c2 h2 d2 _ s2 t2) z s _) =
    case t2 of
        True -> GameIn f1 (Fighter i2 n2 c2 h2 d2 Kick s2 True) z s False
        False -> case touchHitbox h2 h1 of
            True
              | life <= 1 -> GameOver (FighterId 2)
              | otherwise -> GameIn (Fighter i1 n1 c1 h1 d1 a1 (OK (life-1)) t1) (Fighter i2 n2 c2 h2 d2 Kick s2 True) z s True
            False -> GameIn f1 (Fighter i2 n2 c2 h2 d2 Kick s2 False) z s False

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
