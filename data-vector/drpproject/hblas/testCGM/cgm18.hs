{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
import HBLAS.Class
import Data.Time
import Control.DeepSeq


{-# INLINE conjugateGradient #-}
conjugateGradient :: (Num n, Fractional n, Ord n, Floating n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> V.Vector n

conjugateGradient !a !b !vec0= cgm n ( r, y, z, s, t, x)
  where !tol = 1e-10
        !n = length b
        !minus1 = negate 1
        !ab = gemv' a b
        !r = axpy' minus1 ab b vec0
        !y = scal minus1 r
        !z = gemv' a y
        !s = dot y z
        !t = (dot y r) / s
        !x = axpy' t y b vec0
        {-# INLINE cgm #-}
        cgm 0 ( _ , _ , _ , _ , _ , x') =  x'
        cgm !m  ( !r', !y', !z', !s', !t', !x') =
          case (nrm2 r' < tol) of
            True  -> x'
            False ->
              let !minust =  minus1 * t'
                  !rr = axpy' minust z' r' vec0
              in case (nrm2 rr < tol) of
                   True  ->  x'
                   False ->
                     let !bb = (dot rr z') / s'
                         !by = scal bb y'
                         !yy = axpy' minus1 rr by vec0
                         !zz = gemv' a yy
                         !ss = dot yy zz
                         !tt = (dot rr yy) / ss
                         !xx = axpy' tt yy x' vec0
                     in cgm (m-1) ( rr, yy, zz, ss, tt, xx)

main :: IO ()
main = do putStr "\n\nCGM 18:\n"
          let alpha, beta :: Float

              alpha = 1.0
              beta = 0.0
              r = 3

              vecX :: V.Vector Float
              vecY  :: V.Vector Float
              a :: V.Vector (V.Vector Float)
 

              vecX = createVecX' 89580
              vecY = createVec0 89580
              a = createSymSq 0 89580

          t0 <- getCurrentTime
          t1 <- deepseq (V.length vecX) getCurrentTime
          t2 <- deepseq (V.length vecY) getCurrentTime
          t3 <- deepseq (V.length a) getCurrentTime

          putStrLn $ "Discard Test : " ++ (show ((  (diffUTCTime t3 t2)+(diffUTCTime t1 t0)) /8))

          t0 <- getCurrentTime
          t1 <- deepseq (conjugateGradient a vecX vecY) getCurrentTime
          t2 <- deepseq (conjugateGradient a vecX vecY) getCurrentTime
          t3 <- deepseq (conjugateGradient a vecX vecY) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
         

