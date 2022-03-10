module MExport.Config where

data CodeStyle =
  CodeStyle
    { indent :: Int
    , collapseAfter :: Int
    }
  deriving (Show)

data Config =
  Config
    { codeStyle :: CodeStyle
    , writeOnFile :: Bool
    , singleListExport :: Bool
    , projectPath :: String
    , excludeDir :: [String]
    , extensions :: [String]
    }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { codeStyle = CodeStyle {indent = 2, collapseAfter = 100}
    , writeOnFile = True
    , singleListExport = False
    , projectPath = "."
    , excludeDir = [".stack-work", ".git"]
    , extensions = []
    }
