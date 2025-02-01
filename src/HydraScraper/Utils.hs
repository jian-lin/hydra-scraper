module HydraScraper.Utils where

import Data.Text qualified as T
import HydraScraper.Types
import Relude
import Text.HTML.Scalpel (URL)

toScalpelUrl :: Url -> URL
toScalpelUrl = toString

emacsOverlayJobsetUrl :: Url
emacsOverlayJobsetUrl = "https://hydra.nix-community.org/jobset/emacs-overlay/master"

stagingNextJobsetUrl :: Url
stagingNextJobsetUrl = "https://hydra.nixos.org/jobset/nixpkgs/staging-next"

nixosUnstableJobsetUrl :: Url
nixosUnstableJobsetUrl = "https://hydra.nixos.org/jobset/nixos/trunk-combined"

emacsOverlayJobPrefixFilter :: JobPrefixFilter
emacsOverlayJobPrefixFilter = "x86_64-linux.unstable.packages."

stagingNextJobPrefixFilter :: JobPrefixFilter
stagingNextJobPrefixFilter = "emacsPackages."

nixosUnstableJobPrefixFilter :: JobPrefixFilter
nixosUnstableJobPrefixFilter = "emacsPackages."

emacsOverlayPostFilter :: JobPrefixFilter -> FailedJob -> Bool
emacsOverlayPostFilter jobPrefixFilter failedJob =
  jobSystem failedJob == "x86_64-linux" && maybe False isToplevelPackagePname (jobNameToMaybePackagePname $ jobName failedJob)
  where
    jobNameToMaybePackagePname = (PackagePname <$>) . T.stripPrefix (toText jobPrefixFilter) . toText
    isToplevelPackagePname = not . T.elem '.' . toText

x86SystemPostFilter :: FailedJob -> Bool
x86SystemPostFilter failedJob = jobSystem failedJob == "x86_64-linux"

stagingNextPostFilter :: FailedJob -> Bool
stagingNextPostFilter = x86SystemPostFilter

nixosUnstablePostFilter :: FailedJob -> Bool
nixosUnstablePostFilter = x86SystemPostFilter

isEmacsOverlayJobsetUrl :: Url -> Bool
isEmacsOverlayJobsetUrl = T.isInfixOf "emacs-overlay" . toText
