-- | pdfname: Name a PDF file using the information from the `pdfinfo`
-- command.

module Main where

import qualified Data.Text.IO as T

import Options.Applicative ( execParser )

import System.FilePath ( (</>) )

import System.IO
  ( hPrint
  , stderr
  )

import Text.PDF.Info
  ( pdfInfo
  , PDFInfoError(ProcessError, ProcessFailure)
  )

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

import Utilities ( die )

------------------------------------------------------------------------------

main ∷ IO ()
main = do
  opts ← execParser options

  let file ∷ FilePath
      file = optInputFile opts

  info ← pdfInfo file
  case info of
    Right i → do
      newFile ← generateFileName i
      if optDryRun opts
        then putStrLn $ "The full path name will be " ++ outputDir </> newFile
        else createFile file newFile

    Left pdfinfoErr → do
      case pdfinfoErr of
        ProcessFailure err → T.hPutStr stderr err
        ProcessError err   → hPrint stderr err
        _                  → T.hPutStr stderr "TODO: Missing error message"
      die "PDF file or its metadata information is damaged"
