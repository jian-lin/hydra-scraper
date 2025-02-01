module Main where

import Colog (HasLog (..), LogAction, filterBySeverity, msgSeverity, simpleMessageAction)
import Control.Exception.Uncaught (setDisplayExceptionHandler)
import HydraScraper
import HydraScraper.App
import HydraScraper.Types
import HydraScraper.Utils
import Main.Cli
import Main.Types
import Relude

data Env m msg = Env
  { envOutputFormat :: PrintFormat,
    envLogAction :: LogAction m msg
  }

instance HasOutputFormat (Env m msg) where
  getOutputFormat = envOutputFormat

instance HasLog (Env m msg) msg m where
  getLogAction = envLogAction
  setLogAction logAction env = env {envLogAction = logAction}

main :: IO ()
main = do
  setDisplayExceptionHandler
  cliOptions <- parseCliOptions
  -- TODO remove [module#line] from log
  let logAction = filterBySeverity (optionsLogLevel cliOptions) msgSeverity simpleMessageAction
      env =
        Env
          { envOutputFormat = optionsOutput cliOptions,
            envLogAction = logAction
          }
  case optionsCommand cliOptions of
    PredefinedQuery (PredefinedQueryOptions target) ->
      case target of
        EmacsOverlay -> run scrapeEpkgsOnEmacsOverlay env
        StagingNext -> run scrapeEpkgsOnStagingNext env
        NixosUnstable -> run scrapeEpkgsOnNixosUnstable env
    ManualQuery (ManualQueryOptions jobsetUrl jobPrefixFilter noPostFilter) ->
      run (scrapeHydra jobsetUrl jobPrefixFilter postFilter) env
      where
        postFilter
          | noPostFilter = const True
          | isEmacsOverlayJobsetUrl jobsetUrl = emacsOverlayPostFilter jobPrefixFilter
          | otherwise = x86SystemPostFilter
