module Main where

import Control.Monad

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT

import Test.Hspec
import qualified Text.Pretty.Simple as PS (pPrintNoColor)

import qualified MExport as ME (mExport)
import qualified MExport.Config as CC (Config(..), getConfig)
import qualified MExport.Types as MT

main :: IO ()
main = do
  hspec $ do
    describe "Run mExport for the sample project" $ do
      it "returns valid Project" $ do
        project <- test
        project `shouldBe` expectedProject

test :: IO (MT.Project MT.PrettyModule)
test = do
  let config = CC.getConfig {CC.writeOnFile = False, CC.singleListExport = True, CC.projectPath = "./test/sample"}
  exportMap <- ME.mExport config
  return exportMap

printProject :: MT.Project MT.PrettyModule -> IO ()
printProject project = do
  putStrLn "Project:"
  PS.pPrintNoColor project

printResult :: (HM.HashMap String DT.Text) -> IO ()
printResult exportMap = do
  putStrLn "Export list of modules:\n"
  void $ HM.traverseWithKey (\k v -> putStrLn $ "module " <> k <> DT.unpack v) exportMap
  return ()

expectedProject :: MT.Project MT.PrettyModule
expectedProject =
  MT.Project
    "./test/sample"
    [ MT.PrettyModule "Utils" "./test/sample/src/Utils.hs" "((>:>), printString)" (Just $ MT.XCoord 1 13 1 19)
    , MT.PrettyModule
        "Config"
        "./test/sample/src/Config.hs"
        "(Config(_addExclamation), getConfig)"
        (Just $ MT.XCoord 1 14 1 20)
    , MT.PrettyModule "Main" "./test/sample/src/Main.hs" "" (Just $ MT.XCoord 1 12 1 18)
    ]
