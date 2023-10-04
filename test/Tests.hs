
module Main ( main ) where

import Data.List ( nub )

import qualified Data.Text as T

import System.FilePath ( replaceExtension )

import Test.HUnit ( (@=?) )

import Test.Tasty                    ( testGroup, TestTree )
import Test.Tasty.Silver             ( findByExtension, goldenVsProg )
import Test.Tasty.Silver.Interactive ( defaultMain )
import Test.Tasty.HUnit              ( testCase )

------------------------------------------------------------------------------
-- Local imports

import PDFName.Substitutions ( substTable )

------------------------------------------------------------------------------

-- TODO (2023-01-22): Fix the path.
pdfnameBIN :: String
pdfnameBIN = "pdfname"

goldenTest :: FilePath -> TestTree
goldenTest testFile =
  let goldenFile :: FilePath
      goldenFile = replaceExtension testFile ".golden"

      args :: [String]
      args = [ "--dry-run", testFile ]

  in goldenVsProg testFile goldenFile pdfnameBIN args T.empty

succeedTests :: IO TestTree
succeedTests = do
  files <- findByExtension [".pdf"] "test/succeed"
  return $ testGroup "succeed" $ map goldenTest files

failTests :: IO TestTree
failTests = do
  files <- findByExtension [ ".pdf", ".txt" ] "test/fail"
  return $ testGroup "fail" $ map goldenTest files

cliOptionsTests :: TestTree
cliOptionsTests = testGroup "cli-option" [ noOptions, helpOption ]
  where
  helper :: String -> [String] -> TestTree
  helper file arg = goldenVsProg testFile goldenFile pdfnameBIN arg T.empty
    where
      testFile :: String
      testFile = "test/cli-option/" ++ file

      goldenFile :: String
      goldenFile = testFile ++ ".golden"

  noOptions :: TestTree
  noOptions = helper "no-options" []

  helpOption :: TestTree
  helpOption = helper "help" ["--help"]

internalTests :: TestTree
internalTests = testGroup "internal-tests" [ substTableNub ]
  where
    substTableNub :: TestTree
    substTableNub =
      testCase "substTableNub" $ True @=? substTable == nub substTable

allTests :: IO TestTree
allTests = do
  allSucceedTests <- succeedTests
  allFailTests    <- failTests
  return $ testGroup "tests"
    [ allSucceedTests
    , allFailTests
    , cliOptionsTests
    , internalTests
    ]

main :: IO ()
main = defaultMain =<< allTests
