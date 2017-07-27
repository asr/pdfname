{-# LANGUAGE UnicodeSyntax #-}

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

import Substitutions ( unicodeSubst )

------------------------------------------------------------------------------

pdfnameBIN ∷ String
pdfnameBIN = "../dist/build/pdfname/pdfname"

tastyTest ∷ FilePath → TestTree
tastyTest testFile =
  let goldenFile ∷ FilePath
      goldenFile = replaceExtension testFile ".golden"

      args ∷ [String]
      args = [ "--dry-run", testFile ]

  in goldenVsProg testFile goldenFile pdfnameBIN args T.empty

succeedTests ∷ IO TestTree
succeedTests = do
  files ← findByExtension [".pdf"] "succeed"
  return $ testGroup "succeed" $ map tastyTest files

failTests ∷ IO TestTree
failTests = do
  files ← findByExtension [ ".pdf", ".txt" ] "fail"
  return $ testGroup "fail" $ map tastyTest files

clOptionsTests ∷ TestTree
clOptionsTests = testGroup "cl-option" [ noOptions, helpOption ]
  where
  helper ∷ String → [String] → TestTree
  helper file arg = goldenVsProg testFile goldenFile pdfnameBIN arg T.empty
    where
      testFile ∷ String
      testFile = "cl-option/" ++ file

      goldenFile ∷ String
      goldenFile = testFile ++ ".golden"

  noOptions ∷ TestTree
  noOptions = helper "no-options" []

  helpOption ∷ TestTree
  helpOption = helper "help" ["--help"]

internalTests ∷ TestTree
internalTests = testGroup "internal-tests" [ unicodeSubstNub ]
  where
    unicodeSubstNub ∷ TestTree
    unicodeSubstNub =
      testCase "unicodeSubstNub" $ True @=? unicodeSubst == nub unicodeSubst

allTests ∷ IO TestTree
allTests = do
  allSucceedTests ← succeedTests
  allFailTests    ← failTests
  return $ testGroup "tests"
    [ allSucceedTests
    , allFailTests
    , clOptionsTests
    , internalTests
    ]

main ∷ IO ()
main = defaultMain =<< allTests
