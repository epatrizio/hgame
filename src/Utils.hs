module Utils where -- ( someFunc )

newtype Validation e a = Validation (Either e a)
    deriving (Show, Eq)

type Error = String
type Valid a = Validation (Error,String) a

validateString :: String -> String -> Valid String
validateString [] defaultStr =
    Validation (Left ("String is empty. Default string (" ++ defaultStr ++ ") is applied!",defaultStr))
validateString str _ = Validation (Right str)
