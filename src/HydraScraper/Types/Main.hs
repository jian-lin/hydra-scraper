{-# LANGUAGE DeriveAnyClass #-}

module HydraScraper.Types.Main where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Data.Aeson (ToJSON)
import Relude
import Text.HTML.TagSoup (Tag)

data JobKind = OldFailure | NewFailure | AbortedOrTimeout | Unfinished
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

newtype JobPrefixFilter = JobPrefixFilter Text
  deriving stock (Show)
  deriving newtype (ToText, IsString)

newtype Url = Url Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToString, IsString, ToText)
  deriving anyclass (ToJSON)

newtype FailureMode = FailureMode Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

newtype JobName = JobName Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToText)
  deriving anyclass (ToJSON)

newtype PackagePname = PackagePname Text
  deriving newtype (ToText)

newtype System = System Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving anyclass (ToJSON)

newtype PackageName = PackageName Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

data FailedJob = FailedJob
  { jobFailureMode :: FailureMode,
    jobKind :: JobKind,
    jobName :: JobName,
    jobSystem :: System,
    jobUrl :: Url,
    jobPackageName :: PackageName
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

newtype Tags = Tags [Tag Text]
  deriving stock (Show)

newtype CommitHash = CommitHash Text

data PrintFormat = HumanReadable | MachineReadable FilePath
  deriving stock (Show)

newtype SuccessfulBuildNumberDelta = SuccessfulBuildNumberDelta Integer
  deriving stock (Eq)
  deriving newtype (Num)

-- | Like Show, but for human
class Display a where
  display :: a -> Text

instance Display JobKind where
  display = show

instance Display JobPrefixFilter where
  display (JobPrefixFilter jobPrefixFilter) = jobPrefixFilter

instance Display Url where
  display (Url url) = url

instance Display FailureMode where
  display (FailureMode failureMode) = failureMode

instance Display JobName where
  display (JobName jobName) = jobName

instance Display System where
  display (System system) = system

instance Display PackageName where
  display (PackageName packageName) = packageName

instance Display CommitHash where
  display (CommitHash commitHash) = commitHash

-- FIXME 3 should be displayed as +3
instance Display SuccessfulBuildNumberDelta where
  display (SuccessfulBuildNumberDelta delta) = show delta

instance ToString JobKind where
  toString OldFailure = "tabs-still-fail"
  toString NewFailure = "tabs-now-fail"
  toString AbortedOrTimeout = "tabs-aborted"
  toString Unfinished = "tabs-unfinished"

class (MonadCatch m, MonadThrow m) => MonadFetch m where
  fetchTagsM :: Url -> m Tags

class (Monad m) => MonadWriteTerminal m where
  outputAsTableM :: Maybe [FailedJob] -> m ()
  outputAsMarkdownM :: Maybe [FailedJob] -> m ()
  outputTextM :: Text -> m ()

class (MonadCatch m, MonadThrow m) => MonadWriteFile m where
  outputAsJsonFileM :: FilePath -> Maybe [FailedJob] -> m ()

class HasOutputFormat env where
  getOutputFormat :: env -> PrintFormat
