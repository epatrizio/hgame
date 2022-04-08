module Game where

import Coord
import Hitbox

data FighterState  = KO | OK Integer    -- Integer > Life points

newtype FighterId = FighterId Integer
    deriving (Eq,Show,Ord)

data Fighter = Fighter {
    idF :: FighterId,
    posF :: Coordinates,
    hitboxF :: Hitbox,
    faceF :: Direction,
    stateF :: FighterState
}

data Game =
    GameOver FighterId    -- le numero du joueur vainqueur
    | CurrentGame {
        fighter1 :: Fighter,
        fighter2 :: Fighter,
        gameZone :: Zone
    }
