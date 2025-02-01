{-# LANGUAGE QuasiQuotes #-}

module HydraScraper
  ( scrapeHydra,
    scrapeEpkgsOnEmacsOverlay,
    scrapeEpkgsOnStagingNext,
    scrapeEpkgsOnNixosUnstable,
  )
where

import Colog (Message, WithLog, logDebug, logError, logInfo, logWarning)
import Control.Exception.Safe (MonadThrow, throwM)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import HydraScraper.Exceptions
import HydraScraper.Types
import HydraScraper.Utils
import Relude
import Text.HTML.Scalpel ((//), (@:), (@=))
import Text.HTML.Scalpel qualified as S

scrapeHydra ::
  ( MonadFetch m,
    MonadWriteTerminal m,
    MonadWriteFile m,
    WithLog env Message m,
    MonadReader env m,
    HasOutputFormat env,
    MonadThrow m
  ) =>
  Url ->
  JobPrefixFilter ->
  (FailedJob -> Bool) ->
  m ()
scrapeHydra jobsetUrl jobPrefixFilter postFilter = do
  logInfo [i|Jobset url: #{display jobsetUrl}|]
  logInfo [i|Filter string: #{display jobPrefixFilter}|]
  jobsetTags <- fetchTagsM jobsetUrl
  case scrapeNewestEval jobsetTags of
    Just (evalUrl, queueSize, delta) -> do
      -- TODO better way to construct (concat) a url
      let newEvalUrl = Url $ T.concat [toText evalUrl, "?filter=", toText jobPrefixFilter]
      logInfo [i|Newest eval url: #{display newEvalUrl}|]
      unless (queueSize == 0)
        $ logDebug [i|#{display queueSize} unfinished, unfiltered jobs|]
      unless (delta == 0)
        $ logInfo [i|Built #{display delta} more successful jobs|]
      evalTags <- fetchTagsM newEvalUrl
      let mUnfinishedJobs = scrapeFailedJobs Unfinished postFilter evalTags
          mOldFailedJobs = scrapeFailedJobs OldFailure postFilter evalTags
          mNewFailedJobs = scrapeFailedJobs NewFailure postFilter evalTags
          mAbortedOrTimeoutJobs = scrapeFailedJobs AbortedOrTimeout postFilter evalTags
          mAllScrapedJobs = mOldFailedJobs <> mNewFailedJobs <> mAbortedOrTimeoutJobs
      case mUnfinishedJobs of
        Nothing -> logError "Failed to scrape unfinished jobs"
        Just [] -> logDebug "No unfinished jobs"
        Just unfinishedJobs ->
          logWarning [i|#{length unfinishedJobs} unfinished jobs, maybe query again later|]
      logScrapedJobs ("old failed jobs" :: Text) mOldFailedJobs
      logScrapedJobs ("new failed jobs" :: Text) mNewFailedJobs
      logScrapedJobs ("aborted or timeout jobs" :: Text) mAbortedOrTimeoutJobs
      when (isNothing mAllScrapedJobs) $ throwM $ ScrapeJobError newEvalUrl
      printFormat <- asks getOutputFormat
      case printFormat of
        HumanReadable -> do
          outputAsTableM $ mOldFailedJobs <> mNewFailedJobs
          -- output aborted or timeout jobs separately
          -- because their failure may be caused by human and they actually build fine
          outputAsTableM mAbortedOrTimeoutJobs
          when (isEmacsOverlayJobsetUrl jobsetUrl) $ do
            let mCommitHash = scrapeEmacsOverlayCommitHash evalTags
                commitHashDisplayed = display $ fromMaybe (CommitHash "unknown") mCommitHash
            outputTextM [i|- emacs-overlay commit: #{commitHashDisplayed}|]
            outputTextM [i|- [hydra](#{display newEvalUrl})|]
            outputTextM [i|- new failed toplevel builds:|]
            outputAsMarkdownM mAllScrapedJobs
            when (isNothing mCommitHash)
              $ throwM
              $ ScrapeEmacsOverlayCommitHashError newEvalUrl
        MachineReadable filePath ->
          outputAsJsonFileM filePath mAllScrapedJobs
    Nothing -> throwM $ ScrapeNewestEvalError jobsetUrl
  where
    logScrapedJobs name mJobs = do
      case mJobs of
        Nothing -> logError [i|Failed to scrape #{name}|]
        Just [] -> logInfo [i|No #{name}|]
        _ -> pure ()

-- TODO use two types for jobset url and eval url

scrapeNewestEval :: Tags -> Maybe (Url, BuildQueueSize, SuccessfulBuildNumberDelta)
scrapeNewestEval (Tags tags) =
  flip S.scrape tags
    $ S.chroot ("div" @: ["id" @= "tabs-evaluations"] // "tbody" // "tr")
    $ do
      numbers <- S.chroots ("td" @: [S.hasClass "nowrap", "align" @= "right"]) (S.text S.anySelector)
      case numbers of
        [_, _, queueSize, delta] -> do
          evalUrl <- Url <$> S.attr "href" ("a" @: [S.hasClass "row-link"])
          case (parseInteger queueSize, parseInteger delta) of
            (Just queueSize', Just delta') ->
              case mkBuildQueueSize queueSize' of
                Left err -> fail err
                Right queueSize'' ->
                  pure (evalUrl, queueSize'', SuccessfulBuildNumberDelta delta')
            _ -> fail "failed to parse queueSize and delta as Integer"
        _ -> fail "queueSize and delta not found"
  where
    stripSpaceAndPrefixPlus str =
      let str' = T.strip str
       in case T.uncons str' of
            Just (c, cs) -> if c == '+' then cs else str'
            Nothing -> T.empty
    parseInteger str = case stripSpaceAndPrefixPlus str of
      "" -> Just 0
      str' -> readMaybe (toString str')

scrapeFailedJobs ::
  JobKind ->
  (FailedJob -> Bool) ->
  Tags ->
  Maybe [FailedJob]
scrapeFailedJobs jobKind postFilter (Tags tags) =
  flip S.scrape tags
    $ S.chroots ("div" @: ["id" @= toString jobKind] // "tbody" // "tr")
    $ do
      jobName <- JobName <$> S.text jobSelector
      jobUrl <- Url <$> S.attr "href" jobSelector
      jobFailureMode <- FailureMode <$> S.attr "title" ("img" @: [S.hasClass "build-status"])
      tmp <- S.chroots "td" (S.text S.anySelector)
      (packageName, system) <- case tmp of
        [_, _, _, _, packageName, system] -> pure (PackageName packageName, System system)
        _ -> error (show tmp) -- fail "packageName and system not found"
      let failedJob = FailedJob jobFailureMode jobKind jobName system jobUrl packageName
      guard $ postFilter failedJob
      pure failedJob
  where
    jobSelector = "a" @: [S.notP (S.hasClass "row-link")]

scrapeEmacsOverlayCommitHash :: Tags -> Maybe CommitHash
scrapeEmacsOverlayCommitHash (Tags tags) = flip S.scrape tags $ do
  str <- S.text "p"
  case mapMaybe (T.stripPrefix "github:nix-community/emacs-overlay/") (words str) of
    [tmp] -> case T.split (== '?') tmp of
      [commitHash, _] -> pure $ CommitHash commitHash
      _ -> fail "emacs-overlay commit hash not found"
    _ -> fail "emacs-overlay commit hash not found"

scrapeEpkgsOnEmacsOverlay ::
  ( MonadFetch m,
    MonadWriteTerminal m,
    MonadWriteFile m,
    WithLog env Message m,
    MonadReader env m,
    HasOutputFormat env,
    MonadThrow m
  ) =>
  m ()
scrapeEpkgsOnEmacsOverlay =
  scrapeHydra
    emacsOverlayJobsetUrl
    emacsOverlayJobPrefixFilter
    (emacsOverlayPostFilter emacsOverlayJobPrefixFilter)

scrapeEpkgsOnStagingNext ::
  ( MonadFetch m,
    MonadWriteTerminal m,
    MonadWriteFile m,
    WithLog env Message m,
    MonadReader env m,
    HasOutputFormat env,
    MonadThrow m
  ) =>
  m ()
scrapeEpkgsOnStagingNext =
  scrapeHydra
    stagingNextJobsetUrl
    stagingNextJobPrefixFilter
    stagingNextPostFilter

scrapeEpkgsOnNixosUnstable ::
  ( MonadFetch m,
    MonadWriteTerminal m,
    MonadWriteFile m,
    WithLog env Message m,
    MonadReader env m,
    HasOutputFormat env,
    MonadThrow m
  ) =>
  m ()
scrapeEpkgsOnNixosUnstable =
  scrapeHydra
    nixosUnstableJobsetUrl
    nixosUnstableJobPrefixFilter
    nixosUnstablePostFilter
