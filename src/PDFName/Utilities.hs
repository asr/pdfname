{-# LANGUAGE OverloadedStrings #-}

-- | Utilities.

module PDFName.Utilities
 ( (+++)
 , die
 , ifM
 , progNameVersion
 , unless_
 , unlessM
 , upperFirst
 ) where

import Data.Char ( toUpper )

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Version ( showVersion )

import Control.Monad ( unless, void )

import Paths_pdfname ( version )

import System.Environment ( getProgName )
import System.Exit        ( exitFailure )
import System.IO          ( stderr )

------------------------------------------------------------------------------
-- Utilities from the Agda library

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c m m' = do
  b <- c
  if b then m else m'

-- | @unless_@ is just @Control.Monad.unless@ with a more general type.
unless_ :: Monad m => Bool -> m a -> m ()
unless_ b m = unless b $ void m

unlessM :: Monad m => m Bool -> m a -> m ()
unlessM c m = c >>= (`unless_` m)

------------------------------------------------------------------------------
-- Utilities

die :: Text -> IO a
die err = do
  progName <- getProgName
  T.hPutStrLn stderr (T.pack progName +++ ": " +++ err)
  exitFailure

-- | Uppercase the first character of a text.
upperFirst :: Text -> Text
upperFirst xs =
  case T.uncons xs of
    Nothing        -> T.empty
    Just (x', xs') -> T.cons (toUpper x') (T.toLower xs')

-- | Append synonymous.
(+++) :: Text -> Text -> Text
(+++) = T.append

-- | Return version information.
-- TODO (2017-07-06): To use `getProgName`.
progNameVersion :: String
progNameVersion = "pdfname" ++ " version " ++ showVersion version
