module MExport.Config where

data CodeStyle =
  CodeStyle
    { indent :: Int
    , collapseAfter :: Int
    }

data Config =
  Config
    { codeStyle :: CodeStyle
    , writeOnFile :: Bool
    , singleListExport :: Bool
    , projectPath :: String
    , excludeDir :: [String]
    , extensions :: [String]
    }

getConfig :: Config
getConfig =
  Config
    { codeStyle = CodeStyle {indent = 2, collapseAfter = 0}
    , writeOnFile = True
    , singleListExport = False
    , projectPath = "."
    , excludeDir = [".stack-work", ".git"]
    , extensions = []
    }
