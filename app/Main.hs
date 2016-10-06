{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Numeric.LinearAlgebra
import Control.DeepSeq
import Control.Concurrent
import Control.Monad
import Control.Exception
import System.Mem
import Control.Concurrent.Async

main :: IO ()
main = do
  let gcs = do
        performMinorGC
        performMajorGC
        threadDelay 1000
  caps <- getNumCapabilities
  void $ concurrently gcs $ flip mapConcurrently [1..caps] $ \i0 -> do
    let matrices :: Int -> [Matrix Double]
        matrices i = if i > 100
          then []
          else let
            a :: Matrix Double = (3 >< 3) (take 9 [fromIntegral i..])
            in a : matrices (i+1)
    forM_ (cycle (matrices i0)) $ \a -> do
      let (l, m, r) = svd a
      evaluate (force (l <> diagRect 0 m 3 3 <> tr r))
