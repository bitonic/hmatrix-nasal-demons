{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Numeric.LinearAlgebra
import Control.DeepSeq
import Control.Concurrent
import Control.Monad
import Control.Exception
import System.Mem
import Control.Concurrent.Async
import System.Environment

main :: IO ()
main = do
  [matSizeS] <- getArgs
  let matSize = read matSizeS
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
            a :: Matrix Double = (matSize >< matSize) (take (matSize * matSize) [fromIntegral i..])
            in a : matrices (i+1)
    forM_ (cycle (matrices i0)) $ \a -> do
      let (l, m, r) = svd a
      evaluate (force (l <> diagRect 0 m matSize matSize <> tr r))
