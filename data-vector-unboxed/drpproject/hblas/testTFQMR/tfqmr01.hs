{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector.Unboxed as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
import Data.Time
import Control.DeepSeq

{-# INLINE tfqmr #-}
tfqmr :: (Num n, Fractional n, Ord n, Floating n, V.Unbox n) => V.Vector n -> V.Vector n ->V.Vector n -> n -> V.Vector n

tfqmr !a !b !vec0 !tol= qmr k (m, r, w, y1, d, v, u1, theta, eta, tau, rho, x)
  where !x = vec0 
        !r = b
        !w = r
        !y1 = r 
        !k = 1
        !d = vec0
        !v = gemvFlat a y1
        !u1 = v
        !theta = 0
        !eta = 0
        !tau = nrm2 r 
        !rho = tau * tau
        !m = 0
        {-# INLINE qmr #-}
        qmr 100 (_, _, _, _, _, _, _,  _, _, _, _, !x') =  x'
        qmr !k'  (!m', !r',  !w', !y1', !d', !v', !u1', !theta', !eta', !tau', !rho', !x') =
          let !sigma =  dot r' v'  
              !alpha = rho' / sigma 
              !j = 1
              !mm' = 2 * k' - 2 + j 
              !ww' = axpy ((negate 1) * alpha)  u1' w' 
              !dd' = axpy (theta' * theta' * eta'/ alpha) d' y1'    
              !ttheta' = (nrm2 ww') / tau' 
              !c = 1 / sqrt (1 + ttheta' * ttheta') 
              !ttau' = tau' * ttheta' * c 
              !eeta' = c * c * alpha 
              !xx' = axpy eeta'  dd' x'
          in case ((ttau' * sqrt (mm' + 1)) <=  tol) of
              True  -> xx'
              False ->
                let !jj = 2
                    !yy2 = axpy ((negate 1) * alpha)  v' y1'
                    !uu2 = gemvFlat a yy2
                    !mm = 2 * k' - 2 + jj
                    !ww = axpy ((negate 1) * alpha) uu2 ww' 
                    !dd = axpy (ttheta' * ttheta' * eeta'/ alpha) dd' yy2  
                    !ttheta = nrm2 ww / ttau'
                    !cc = 1 / sqrt (1 + ttheta * ttheta) 
                    !ttau = ttau' * ttheta * cc
                    !eeta = cc * cc * alpha 
                    !xx = axpy eeta  dd xx'  
                in case ((ttau * sqrt (mm + 1)) <=  tol) of
                     True  ->  xx
                     False ->
                       let !rhon = dot r' ww
                           !beta = rhon / rho'
                           !rrho = rhon
                           !yy1 = axpy beta yy2 ww   
                           !uu1 = gemvFlat a yy1
                           !vv = axpyaxpy' beta beta  v' uu2 uu1 
                       in qmr (k'+1) (mm, r', ww, yy1, dd, vv, uu1, ttheta, eeta, ttau, rrho, xx)


main :: IO ()
main = do putStr "\n\nTFQMR 01:\n"
          let alpha, beta :: Float

              alpha = 1.0
              tol = 1e-10
              beta = 0.0
              r = 3

              vecX :: V.Vector Float
              vecY  :: V.Vector Float
              a :: V.Vector Float 

              vecX = createVecX' 1000
              vecY = createVec0 1000
              a = createVecX' (1000^2)

          t0 <- getCurrentTime
          t1 <- deepseq (V.length vecX) getCurrentTime
          t2 <- deepseq (V.length vecY) getCurrentTime
          t3 <- deepseq (V.length a) getCurrentTime

          putStrLn $ "Discard Test : " ++ (show ((  (diffUTCTime t3 t2)+(diffUTCTime t1 t0)) ))

          t0 <- getCurrentTime
          t1 <- deepseq (tfqmr a vecX vecY tol) getCurrentTime
          t2 <- deepseq (tfqmr a vecX vecY tol) getCurrentTime
          t3 <- deepseq (tfqmr a vecX vecY tol) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
         

