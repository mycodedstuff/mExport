module Main where

import Control.Monad

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT
import Test.Hspec

import qualified MExport.Config as CC (Config(..), getConfig)
import qualified MExport as ME (mExport)
import qualified MExport.Types as LT (Context(..))

main :: IO ()
main = do
  exportMap <- test
  hspec $ do
    describe "Run mExport for the sample project" $ do
      it "returns valid Config exports" $ do HM.lookup "Config" exportMap `shouldBe` Just "(Config(..), _addExclamation, getConfig)"
      it "returns valid Utils exports" $ do HM.lookup "Utils" exportMap `shouldBe` Just "(printString, (>:>))"

test :: IO (HM.HashMap String DT.Text)
test = do
  let config = CC.getConfig {CC.writeOnFile = False, CC.singleListExport = True}
      srcModule = "./test/sample/src/Main.hs"
      _context = LT.Context {moduleSrc = srcModule}
  exportMap <- ME.mExport config _context
  return exportMap

printResult :: (HM.HashMap String DT.Text) -> IO ()
printResult exportMap = do
  putStrLn "Export list of modules:\n"
  void $ HM.traverseWithKey (\k v -> putStrLn $ "module " <> k <> DT.unpack v) exportMap
  return ()
