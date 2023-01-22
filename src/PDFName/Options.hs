-- | Process the command-line arguments.

module PDFName.Options
  ( options
  , Options( Options -- Improve Haddock information.
           , optDryRun
           , optInputFile
           )
  , outputDir
  ) where

import Options.Applicative
  ( argument
  , info
  , infoOption
  , help
  , helper
  , long
  , metavar
  , Parser
  , ParserInfo
  , short
  , str
  , switch
  )

------------------------------------------------------------------------------
-- Local imports

import PDFName.Utilities ( progNameVersion )

------------------------------------------------------------------------------
-- | Program command-line options.
data Options = Options
  { optDryRun    :: Bool
  , optInputFile :: FilePath
  }

pOptDryRun :: Parser Bool
pOptDryRun =
  switch ( long "dry-run"
           <> help ("Do not create the PDF file only print which would be "
                    ++ "its full path name")
         )

-- | Parse a version flag.
pOptVersion :: Parser (a -> a)
pOptVersion = infoOption progNameVersion $
  long "version"
  <> short 'V'
  <> help "Show version number"

pOptInputFile :: Parser FilePath
pOptInputFile = argument str (metavar "FILE")

pOptions :: Parser Options
pOptions = Options <$> pOptDryRun <*> pOptInputFile

options :: ParserInfo Options
options = info (helper <*> pOptVersion <*> pOptions) mempty

outputDir :: FilePath
outputDir = "/tmp/"
