-- | Creation of the PDF file.

module CreateFile
  ( createFile
  , generateFileName
  ) where

import Data.Char  ( isAsciiLower, isDigit )
import Data.Maybe ( fromMaybe )

import Data.Text ( Text )
import qualified Data.Text as T

import System.Directory ( copyFile )
import System.FilePath  ( (</>) )

import Text.PDF.Info
  ( PDFInfo
  , pdfInfoAuthor
  , pdfInfoCreationDate
  , pdfInfoTitle
  )

------------------------------------------------------------------------------
-- Local imports

import Options ( outputDir )

import Substituions
  ( authorSubst
  , chSubst
  , commonSubst
  , titleSubst
  )

import Utilities
  ( (+++)
  , die
  , replace
  )

------------------------------------------------------------------------------
defaultAuthor, defaultTitle, defaultYear ∷ Text
defaultAuthor = "no-author"
defaultTitle  = "no-title"
defaultYear   = "no-year"

getAuthor ∷ Text → Text
getAuthor xs =
  if T.null xs
  then defaultAuthor
  else
    -- NB that the substituions are not commutative.
    ( T.toLower
      . replace chSubst
      . T.intercalate (T.singleton '-')
      . map (T.reverse . T.takeWhile (' ' /=) . T.reverse)
      . T.split (',' ==)
      . replace authorSubst
      . replace commonSubst
    ) xs

getYear ∷ Text → Text
getYear xs = if T.null xs then defaultYear else T.take 4 xs

getTitle ∷ Text → Text
getTitle xs =
  if T.null xs
  then defaultTitle
  else
    -- NB that the substituions are not commutative.
    ( T.toLower
      . replace chSubst
      . replace titleSubst
      . replace commonSubst
    ) xs

generateFileName ∷ PDFInfo → IO FilePath
generateFileName info = do

  let authorHelper ∷ Text
      authorHelper = maybe defaultAuthor getAuthor $ pdfInfoAuthor info

  author ← if isValidName authorHelper
           then return authorHelper
           else die $ "could not generate the author from "
                       +++ "`"
                       +++ fromMaybe "missing author field" (pdfInfoAuthor info)
                       +++ "`"

  let year ∷ Text
      year = maybe defaultYear (getYear . T.pack . show ) $ pdfInfoCreationDate info

  let titleHelper ∷ Text
      titleHelper = maybe defaultTitle getTitle $ pdfInfoTitle info

  title ← if isValidName titleHelper
            then return titleHelper
            else die $ "could not generate the title from "
                       +++ "`"
                       +++ fromMaybe "missing title field" (pdfInfoTitle info)
                       +++ "`"

  let fileName ∷ FilePath
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
isValidChar ∷ Char → Bool
isValidChar c = isAsciiLower c || isDigit c || (c == '-')

isValidName ∷ Text → Bool
isValidName t = all (== True) $ map isValidChar $ T.unpack t

createFile ∷ FilePath → FilePath → IO ()
createFile f newF =
  copyFile f outputPath >> putStrLn ("Created " ++ outputPath)
  where
  outputPath ∷ FilePath
  outputPath = outputDir </> newF
