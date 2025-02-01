{-# LANGUAGE QuasiQuotes #-}

module HydraScraper.Types.BuildQueueSize
  ( BuildQueueSize,
    mkBuildQueueSize,
  )
where

import Data.String.Interpolate (i)
import HydraScraper.Types.Main (Display (..))
import Relude

newtype BuildQueueSize = BuildQueueSize Integer
  deriving stock (Eq)
  deriving newtype (Num)

mkBuildQueueSize :: Integer -> Either String BuildQueueSize
mkBuildQueueSize queueSize
  | queueSize < 0 = Left [i|BuildQueueSize cannot be negative: #{queueSize}|]
  | otherwise = Right $ BuildQueueSize queueSize

instance Display BuildQueueSize where
  display (BuildQueueSize queueSize) = show queueSize
