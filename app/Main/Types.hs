module Main.Types where

import Colog (Severity)
import HydraScraper.Types
import Relude

data Options = Options
  { optionsCommand :: Command,
    optionsOutput :: PrintFormat,
    optionsLogLevel :: Severity -- TODO wrap Severity
  }
  deriving stock (Show)

data Command
  = PredefinedQuery PredefinedQueryOptions
  | ManualQuery ManualQueryOptions
  deriving stock (Show)

newtype PredefinedQueryOptions = PredefinedQueryOptions
  {predefinedQueryOptionsTarget :: PredefinedQueryOptionsTarget}
  deriving stock (Show)

data PredefinedQueryOptionsTarget = EmacsOverlay | StagingNext | NixosUnstable
  deriving stock (Show, Read, Enum, Bounded)

data ManualQueryOptions = ManualQueryOptions
  { manualQueryOptionsJobsetUrl :: Url,
    manualQueryOptionsJobPrefixFilter :: JobPrefixFilter,
    manualQueryOptionsNoPostFilter :: Bool
  }
  deriving stock (Show)
