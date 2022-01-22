module Main where

import Control.Monad

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT
import Test.Hspec

import qualified MExport as ME (mExport)
import qualified MExport.Config as CC (Config(..), getConfig)
import qualified MExport.Types as MT

main :: IO ()
main = do
  project <- test
  putStrLn $ show project
  return ()
  -- hspec $ do
  --   describe "Run mExport for the sample project" $ do
  --     it "returns valid Config exports" $ do
  --       HM.lookup "Config" exportMap `shouldBe` Just "(Config(..), _addExclamation, getConfig)"
  --     it "returns valid Utils exports" $ do HM.lookup "Utils" exportMap `shouldBe` Just "(printString, (>:>))"

test :: IO (MT.Project MT.PrettyModule)
test = do
  let config = CC.getConfig {CC.writeOnFile = False, CC.singleListExport = True, CC.projectPath = "./test/sample"}
  exportMap <- ME.mExport config
  return exportMap

printResult :: (HM.HashMap String DT.Text) -> IO ()
printResult exportMap = do
  putStrLn "Export list of modules:\n"
  void $ HM.traverseWithKey (\k v -> putStrLn $ "module " <> k <> DT.unpack v) exportMap
  return ()
