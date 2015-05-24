{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

-- | An extended prelude to avoid typing all the standard default imports.
-- The exports are chosen such that the chance for conflicts is low.
--
-- This has been taken adapted from https://github.com/silkapp/base-extended
module Prelude
  ( module StdPrelude

  -- * Control modules
  , module Control.Applicative
  , module Control.Arrow       -- Partial
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans.Class

  -- * Data modules
  , module Data.Char           -- Partial
  , module Data.Data           -- Partial
  , module Data.Either
  , module Data.Function
  , module Data.List           -- Partial
  , module Data.Maybe
  , module Data.Monoid         -- Partial
  , module Data.Ord
  , module Data.Time           -- Partial
  , module Data.Tuple

  -- * Safe
  , module Safe

  -- * System modules
  , module Data.Time.Locale.Compat -- Partial

  ) where

import qualified "base" Prelude as StdPrelude

import Control.Applicative
import Control.Arrow (first, second, (&&&), (***), (+++), (|||))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Char hiding (GeneralCategory (..))
import Data.Data (Data (..), Typeable)
import Data.Either
import Data.Function (on)
import Data.List hiding (delete)
import Data.Maybe
import Data.Monoid (Monoid (..), (<>))
import Data.Ord
import Data.Time (FormatTime, NominalDiffTime, ParseTime, UTCTime, addUTCTime, diffUTCTime, formatTime, getCurrentTime, parseTime)
-- Misc orphan instances
import Data.Time.Clock ()
import Data.Tuple

import Safe hiding (abort)

import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
