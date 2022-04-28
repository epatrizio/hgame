module Config where

import Control.Monad.Reader

data Configuration = Config {
    screenW :: Integer,
    screenH :: Integer
}

getConf :: (Configuration -> Integer) -> Reader Configuration Integer
getConf conf_accessor = do
    conf <- ask
    return (conf_accessor conf)
