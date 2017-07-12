{-# LANGUAGE UnicodeSyntax #-}

module Main ( main ) where

import qualified Data.Text as T

import System.FilePath ( replaceExtension )

import Test.Tasty                    ( testGroup, TestTree )
import Test.Tasty.Silver             ( findByExtension, goldenVsProg )
import Test.Tasty.Silver.Interactive ( defaultMain )

pdfnameBIN ∷ String
pdfnameBIN = "../dist/build/pdfname/pdfname"

succeedTests ∷ IO TestTree
succeedTests = do
  files ← findByExtension [".pdf"] "succeed"
  return $ testGroup "Succeed" $ map oneSucceedTest files

oneSucceedTest ∷ FilePath → TestTree
oneSucceedTest testFile =
  let goldenFile ∷ FilePath
      goldenFile = replaceExtension testFile ".golden"

      args ∷ [String]
      args = [ "--dry-run", testFile ]

  in goldenVsProg testFile goldenFile pdfnameBIN args T.empty

allTests ∷ IO TestTree
allTests = do
  tests ← succeedTests
  return $ testGroup "Tests" [ tests ]

main ∷ IO ()
main = defaultMain =<< allTests
