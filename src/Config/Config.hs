module Config.Config where

data Config =
  Config
    { filterModules :: [String]
    }

getConfig :: Config
getConfig = Config {filterModules = ["^Newton.*$"]}
