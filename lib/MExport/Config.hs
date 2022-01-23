module MExport.Config where

data CodeStyle =
  CodeStyle
    { indent :: Int
    }

data Config =
  Config
    { codeStyle :: CodeStyle
    , writeOnFile :: Bool
    , singleListExport :: Bool
    , projectPath :: String
    , excludeDir :: [String]
    }

getConfig :: Config
getConfig =
  Config
    { codeStyle = CodeStyle {indent = 2}
    , writeOnFile = True
    , singleListExport = False
    , projectPath = "."
    , excludeDir = [".stack-work", ".git"]
    }