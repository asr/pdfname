-- | pdfname: Name a PDF file using the information from the `pdfinfo`
-- command.

module Main where

import qualified Data.Text as T

import Options.Applicative ( execParser )

import System.FilePath ( (</>) )

import System.IO
  ( hPrint
  , stderr
  )

import Text.PDF.Info ( pdfInfo )

------------------------------------------------------------------------------
-- Local imports

import CreateFile
  ( createFile
  , generateFileName
  )

import Options
  ( options
  , Options( optDryRun
           , optInputFile
           )
  , outputDir
  )

import Utilities
  ( (+++)
  , die
  , isPDF
  , unlessM
  )

------------------------------------------------------------------------------

main ∷ IO ()
main = do
  opts ← execParser options

  let file ∷ FilePath
      file = optInputFile opts

  unlessM (isPDF file) $ die $ T.pack file +++ " is not a PDF file"

  info ← pdfInfo file
  case info of
    Right i → do
      newFile ← generateFileName i
      if optDryRun opts
        then putStrLn $ "The full path name will be " ++ outputDir </> newFile
        else createFile file newFile

    Left  err → do
      hPrint stderr err
      die "PDF file or its metadata information is damaged."
