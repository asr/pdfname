{-# LANGUAGE OverloadedStrings #-}

-- | pdfname: Name a PDF file using the information from the `pdfinfo`
-- command.

module Main ( main ) where

import Options.Applicative ( execParser )

import Pdf.Document.Pdf      ( document, withPdfFile )
import Pdf.Document.Document ( documentInfo )

import System.FilePath ( (</>) )

------------------------------------------------------------------------------
-- Local imports

import PDFName.CreateFile
  ( createFile
  , generateFileName
  )

import PDFName.Options
  ( options
  , Options( optDryRun
           , optInputFile
           )
  , outputDir
  )

import PDFName.Utilities ( die )

------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser options

  let file :: FilePath
      file = optInputFile opts

  withPdfFile file $ \pdf -> do
    doc <- document pdf
    info <- documentInfo doc
    case info of
      Just i -> do
        newFile <- generateFileName i
        if optDryRun opts
        then putStrLn $ "The full path name will be " ++ outputDir </> newFile
        else createFile file newFile

      Nothing -> die "PDF file or its metadata information is damaged"
