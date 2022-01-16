module Config where

data Config =
  Config
    { _data :: String
    }

getConfig :: Config
getConfig = Config {_data = "Hello World"}
