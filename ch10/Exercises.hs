{-# LANGUAGE BangPatterns #-}

module Exercises where

import Data.Map.Strict qualified as Map

-- Exercise 10.3

data MetricsStore = MetricsStore
  { successCount :: !Int,
    failureCount :: !Int,
    callDuration :: !(Map.Map String Int)
  }
  deriving (Eq, Show)
