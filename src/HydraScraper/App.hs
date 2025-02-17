{-# LANGUAGE QuasiQuotes #-}

module HydraScraper.App where

import Control.Exception.Safe (MonadCatch, MonadThrow, catchAny, throwM)
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import HydraScraper.Exceptions
import HydraScraper.Types
import HydraScraper.Utils (toScalpelUrl)
import Relude
import Text.HTML.Scalpel qualified as S
import Text.Layout.Table qualified as Table

newtype App (env :: (Type -> Type) -> Type -> Type) msg a = App
  { runApp :: ReaderT (env (App env msg) msg) IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (env (App env msg) msg),
      MonadThrow,
      MonadCatch,
      MonadIO
    )

run :: App env msg a -> env (App env msg) msg -> IO a
run app = runReaderT $ runApp app

instance MonadFetch (App env msg) where
  fetchTagsM url =
    catchAny
      (liftIO $ fmap Tags $ S.fetchTags $ toScalpelUrl url)
      $ \exception -> throwM $ FetchTagsError url exception

instance MonadWriteTerminal (App env msg) where
  outputAsTableM mFailedJobs =
    case mFailedJobs of
      Nothing -> pure ()
      Just [] -> pure ()
      Just failedJobs ->
        putStrLn
          $ Table.tableString
          $ Table.simpleTableS Table.unicodeRoundS
          $ fmap
            ( Table.rowG
                . ( \job ->
                      [ display $ jobUrl job,
                        -- display $ jobName job,
                        display $ jobSystem job,
                        display $ jobFailureMode job,
                        display $ jobPackageName job,
                        display $ jobKind job
                      ]
                  )
            )
            (sort failedJobs)
  outputAsMarkdownM mFailedJobs =
    case mFailedJobs of
      Nothing -> pure ()
      Just [] -> pure ()
      Just failedJobs -> mapM_ (putTextLn . mkJobItem) (sort failedJobs)
    where
      mkJobItem job = [i|  - [ ] [#{display (jobPackageName job)}](#{display (jobUrl job)})|]
  outputTextM = putTextLn

instance MonadWriteFile (App env msg) where
  outputAsJsonFileM filePath mFailedJobs =
    case mFailedJobs of
      Nothing -> pure ()
      Just [] -> pure ()
      Just failedJobs -> catchAny (liftIO $ Aeson.encodeFile filePath failedJobs)
        $ \exception -> throwM $ WriteJsonFileError filePath exception
