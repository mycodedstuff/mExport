module Utils.Utils
  ( getAction
  , moduleToPath
  , takeTillIndex
  , dropTillIndex
  , findFirstInText
  ) where

import qualified Config.Config as CC
import Data.Text as T (Text)
import qualified Data.Text.Internal.Search as DTS
import Options.Applicative hiding (action, style)
import Types

getAction :: CC.Config -> IO Action
getAction config =
  execParser (info (options config <**> helper) (header "mExport - minimize export list of haskell modules"))

options :: CC.Config -> Parser Action
options config = flag' ShowVersion (long "version" <> help "Print the version") <|> Run config <$> (Context <$> moduleSrc)
  where
    moduleSrc = strOption (long "src" <> help "Source file of the module" <> metavar "FILE")

moduleToPath :: String -> String
moduleToPath =
  map
    (\c ->
       if c == '.'
         then '/'
         else c)

takeTillIndex :: (Int -> Bool) -> [a] -> [a]
takeTillIndex f a = go f 0 a
  where
    go :: (Int -> Bool) -> Int -> [a] -> [a]
    go _ _ [] = []
    go p i (x:xs)
      | p i = x : go p (i + 1) xs
      | otherwise = []

dropTillIndex :: (Int -> Bool) -> [a] -> [a]
dropTillIndex f a = go f 0 a
  where
    go :: (Int -> Bool) -> Int -> [a] -> [a]
    go _ _ [] = []
    go p i xs@(x:xs')
      | p i = go p (i + 1) xs'
      | otherwise = xs

headMaybe :: [a] -> Maybe a
headMaybe arr =
  if null arr
    then Nothing
    else Just $ head arr

findFirstInText :: Text -> Text -> Maybe Int
findFirstInText needle haystack = headMaybe $ DTS.indices needle haystack
