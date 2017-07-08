-- | Creation of the PDF file.

module CreateFile
  ( createFile
  , generateFileName
  ) where

import Data.Char  ( isAscii )
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
  , titleSubst
  )

import Utilities
  ( (+++)
  , die
  , replace
  )

------------------------------------------------------------------------------
defaultValue ∷ Text
defaultValue = "xx"

getAuthor ∷ Text → Text
getAuthor xs =
  if T.null xs
  then defaultValue
  else
    -- NB that the substituions are not commutative.
    ( replace chSubst
      . T.intercalate (T.singleton '-')
      . map (T.toLower . T.reverse . T.takeWhile (' ' /=) . T.reverse)
      . T.split (',' ==)
      . replace authorSubst
    ) xs

getYear ∷ Text → Text
getYear xs = if T.null xs then defaultValue else T.take 4 xs

getTitle ∷ Text → Text
getTitle xs =
  if T.null xs
  then defaultValue
  else
    -- NB that the substituions are not commutative.
    ( T.toLower
      . replace chSubst
      . replace titleSubst
    ) xs

generateFileName ∷ PDFInfo → IO FilePath
generateFileName info = do

  let authorHelper ∷ Text
      authorHelper = maybe defaultValue getAuthor $ pdfInfoAuthor info

  author ← if validateName authorHelper
           then return authorHelper
           else die $ "could not generate the author from "
                       +++ "`"
                       +++ fromMaybe "missing author field" (pdfInfoAuthor info)
                       +++ "`"

  let year ∷ Text
      year = maybe defaultValue (getYear . T.pack . show ) $ pdfInfoCreationDate info

  let titleHelper ∷ Text
      titleHelper = maybe defaultValue getTitle $ pdfInfoTitle info

  title ← if validateName titleHelper
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

-- TODO (2017-07-04): Is there Data.Text functions instead of
-- Data.Char functions for implementing this function?
validateName ∷ Text → Bool
validateName t = all (== True) $ map isAscii $ T.unpack t

createFile ∷ FilePath → FilePath → IO ()
createFile f newF =
  copyFile f outputPath >> putStrLn ("Created " ++ outputPath)
  where
  outputPath ∷ FilePath
  outputPath = outputDir </> newF
