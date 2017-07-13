{-# LANGUAGE UnicodeSyntax #-}

module Main ( main ) where

import qualified Data.Text as T

import System.FilePath ( replaceExtension )

import Test.Tasty                    ( testGroup, TestTree )
import Test.Tasty.Silver             ( findByExtension, goldenVsProg )
import Test.Tasty.Silver.Interactive ( defaultMain )

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
  return $ testGroup "Succeed" $ map tastyTest files

failTests ∷ IO TestTree
failTests = do
  files ← findByExtension [ ".pdf", ".txt" ] "fail"
  return $ testGroup "Fail" $ map tastyTest files

allTests ∷ IO TestTree
allTests = do
  allSucceedTests ← succeedTests
  allFailTests    ← failTests
  return $ testGroup "Tests" [ allSucceedTests, allFailTests ]

main ∷ IO ()
main = defaultMain =<< allTests
