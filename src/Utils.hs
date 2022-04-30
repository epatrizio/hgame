module Utils where

import Control.Monad.State

newtype Validation e a = Validation (Either e a)
    deriving (Show, Eq)

type Error = String
type Valid a = Validation (Error,String) a

validateString :: String -> String -> Valid String
validateString [] defaultStr =
    Validation (Left ("String is empty. Default string (" ++ defaultStr ++ ") is applied!",defaultStr))
validateString str _ = Validation (Right str)

type Log = [String]
type LogState a = State Log a

pushLog :: String -> LogState ()
pushLog logStr = state $ \ls -> ((),logStr:ls)

popLog :: LogState String
popLog = state $ \(l:ls) -> (l,ls)
