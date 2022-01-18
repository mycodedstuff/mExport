module Config where

data Config =
  Config
    { _addExclamation :: Bool
    }

getConfig :: Config
getConfig = Config {_addExclamation = True}
