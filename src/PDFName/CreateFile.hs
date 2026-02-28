{-# LANGUAGE OverloadedStrings #-}

-- | Creation of the PDF file.

module PDFName.CreateFile
  ( createFile
  , generateFileName
  ) where

import Data.Char  ( isAsciiLower, isDigit )

import Data.Text ( Text )
import qualified Data.Text as T

import Pdf.Document.Info ( Info, infoAuthor, infoTitle )

import System.Directory ( copyFile )
import System.FilePath  ( (</>) )

------------------------------------------------------------------------------
-- Local imports

import PDFName.Options ( outputDir )

import PDFName.Substitutions ( replaceUnicode )
import PDFName.Utilities ( die )

------------------------------------------------------------------------------

defaultAuthor, defaultTitle, defaultYear :: Text
defaultAuthor = "no-author"
defaultTitle  = "no-title"
defaultYear   = "no-year"

-- getYear :: Text -> Text
-- getYear xs = if T.null xs then defaultYear else T.take 4 xs

getMetadata :: Text -> Text -> Text
getMetadata defaultValue xs =
  if T.null xs
  then defaultValue
  else
    -- NB that the substitutions are not commutative.
    ( T.toLower
      . replaceUnicode
    ) xs

getAuthor :: Text -> Text
getAuthor = getMetadata defaultAuthor

getTitle :: Text -> Text
getTitle = getMetadata defaultTitle

generateFileName :: Info -> IO FilePath
generateFileName info = do

  authorHelper <- maybe defaultAuthor getAuthor <$> infoAuthor info

  author <- if isValidName authorHelper
           then return authorHelper
           else die "Invalid character in author field"

  -- TODO (2023-09-29): Missing `infoCreationDate` in `Pdf.Document.Info`
  let year :: Text
      year = defaultYear

  titleHelper <- maybe defaultTitle getTitle <$> infoTitle info

  title <- if isValidName titleHelper
            then return titleHelper
            else die "Invalid character in author field"

  let fileName :: FilePath
      fileName = T.unpack $ foldl1 T.append
                   [ author
                   , T.singleton '-'
                   , year
                   , T.singleton '.'
                   , title
                   , ".pdf"
                   ]

  return fileName

-- TODO (2017-07-11): Is there @Data.Text@ functions instead of
-- @Data.Char@ functions for implementing this function?
isValidChar :: Char -> Bool
isValidChar c = isAsciiLower c || isDigit c || (c == '-')

isValidName :: Text -> Bool
isValidName t = all (== True) $ map isValidChar $ T.unpack t

createFile :: FilePath -> FilePath -> IO ()
createFile f newF =
  copyFile f outputPath >> putStrLn ("Created " ++ outputPath)
  where
  outputPath :: FilePath
  outputPath = outputDir </> newF
