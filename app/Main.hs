{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import Control.DeepSeq
import Control.Concurrent
import Control.Monad
import Control.Exception
import System.Mem
import Control.Concurrent.Async
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import qualified Data.Vector.Storable as Vector

infixr 5 :>, ::>, ..>
type (:>)  t r = CV t r
type (::>) t r = OM t r
type (..>) t r = CM t r

type CM b r = CInt -> CInt -> Ptr b -> r
type CV b r = CInt -> Ptr b -> r
type OM b r = CInt -> CInt -> CInt -> CInt -> Ptr b -> r

type CIdxs r = CV CInt r
type Ok = IO CInt

type Extr x = CInt -> CInt -> CIdxs (CIdxs (OM x (OM x (IO CInt))))

foreign import ccall unsafe "svd_l_Rdd" dgesdd :: TSVD R
foreign import ccall unsafe "extractD" c_extractD :: Extr Double

type TSVD t = t ::> t ::> R :> t ::> Ok

infixl 1 #
a # b = apply a b
{-# INLINE (#) #-}

_MATRIX_SIZE = 30

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
            a :: Matrix Double = (_MATRIX_SIZE >< _MATRIX_SIZE) (take (_MATRIX_SIZE * _MATRIX_SIZE) [fromIntegral i..])
            in a : matrices (i+1)
    forM_ (cycle (matrices i0)) $ \a -> do
      -- let (l, m, r) = svd a
      -- evaluate (force (l <> diagRect 0 m _MATRIX_SIZE _MATRIX_SIZE <> tr r))
      evaluate (force (svd2 a))


svd2 :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
svd2 = svdAux dgesdd "svdRdd"

svdAux f st x = unsafePerformIO $ do
    a <- copy ColumnMajor x
    u <- createMatrix ColumnMajor r r
    s <- createVector (min r c)
    v <- createMatrix ColumnMajor c c
    f # a # u # s # v #| st
    return (u,s,v)
  where
    r = rows x
    c = cols x

copy ord m = extractAux c_extractD ord m 0 (idxs[0,rows m-1]) 0 (idxs[0,cols m-1])

-- | Number of elements
dim :: (Storable t) => Vector t -> Int
dim = Vector.length

(@>) :: Storable t => Vector t -> Int -> t
infixl 9 @>
v @> n
    | n >= 0 && n < dim v = at' v n
    | otherwise = error "vector index out of range"
{-# INLINE (@>) #-}

extractAux f ord m moder vr modec vc = do
    let nr = if moder == 0 then fromIntegral $ vr@>1 - vr@>0 + 1 else dim vr
        nc = if modec == 0 then fromIntegral $ vc@>1 - vc@>0 + 1 else dim vc
    r <- createMatrix ord nr nc
    f moder modec # vr # vc # m # r  #|"extract"
    return r
