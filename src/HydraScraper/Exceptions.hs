{-# LANGUAGE QuasiQuotes #-}

module HydraScraper.Exceptions where

import Data.String.Interpolate (i, __i)
import HydraScraper.Types
import Relude

data FetchTagsError = FetchTagsError Url SomeException
  deriving stock (Show)

data WriteJsonFileError = WriteJsonFileError FilePath SomeException
  deriving stock (Show)

newtype ScrapeJobError = ScrapeJobError Url
  deriving stock (Show)

newtype ScrapeEmacsOverlayCommitHashError = ScrapeEmacsOverlayCommitHashError Url
  deriving stock (Show)

newtype ScrapeNewestEvalError = ScrapeNewestEvalError Url
  deriving stock (Show)

instance Exception FetchTagsError where
  displayException (FetchTagsError url exception) =
    [__i|
      Failed to fetch tags from #{display url} with exception:
        #{displayException exception}
    |]

instance Exception WriteJsonFileError where
  displayException (WriteJsonFileError filePath exception) =
    [__i|
      Failed to write results to json file #{filePath} with exception:
        #{displayException exception}
    |]

instance Exception ScrapeJobError where
  displayException (ScrapeJobError evalUrl) =
    [i|Failed to scrape any job from eval #{display evalUrl}|]

instance Exception ScrapeEmacsOverlayCommitHashError where
  displayException (ScrapeEmacsOverlayCommitHashError evalUrl) =
    [i|Failed to scrape emacs-overlay commit hash for eval #{evalUrl}|]

instance Exception ScrapeNewestEvalError where
  displayException (ScrapeNewestEvalError jobsetUrl) =
    [i|Failed to scrape newest eval for jobset #{jobsetUrl}|]
