{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.Vector.Unboxed as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
-- import HBLAS.Class
import Data.Time

--a function to round a floating point number to atmost 4 decimal places
round4dp :: (Num n, RealFrac n,  Fractional n, Ord n, Floating n)=>n -> n
round4dp x= fromIntegral (round $ x * 1e4) / 1e4

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action

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
main = do putStr "\n\nTFQMR 04-06:\n"
          let alpha, beta :: Float

              alpha = 1.0
              tol = 1e-10
              beta = 0.0
              r = 3

              vecX1 :: V.Vector Float
              vecY1  :: V.Vector Float
              a1 :: V.Vector Float 
              
              vecX2 :: V.Vector Float
              vecY2  :: V.Vector Float
              a2 :: V.Vector Float 
              
              vecX3 :: V.Vector Float
              vecY3  :: V.Vector Float
              a3 :: V.Vector Float 
              
              vecX4 :: V.Vector Float
              vecY4  :: V.Vector Float
              a4 :: V.Vector Float 
              
              vecX5 :: V.Vector Float
              vecY5  :: V.Vector Float
              a5 :: V.Vector Float 
              
              vecX6 :: V.Vector Float
              vecY6  :: V.Vector Float
              a6 :: V.Vector Float 
              
              vecX7 :: V.Vector Float
              vecY7  :: V.Vector Float
              a7 :: V.Vector Float 
              
              vecX8 :: V.Vector Float
              vecY8  :: V.Vector Float
              a8 :: V.Vector Float 
              
              vecX9 :: V.Vector Float
              vecY9  :: V.Vector Float
              a9 :: V.Vector Float 
              
              vecX10 :: V.Vector Float
              vecY10  :: V.Vector Float
              a10 :: V.Vector Float 
              
              vecX11 :: V.Vector Float
              vecY11  :: V.Vector Float
              a11 :: V.Vector Float 
              
              vecX12 :: V.Vector Float
              vecY12  :: V.Vector Float
              a12 :: V.Vector Float 
              
              vecX1 = createVecX' (2^1)
              vecY1 = createVec0 (2^1)
              a1 = createFlatMatrix 0 (2^1)

              vecX2 = createVecX' (2^2)
              vecY2 = createVec0 (2^2)
              a2 = createFlatMatrix 0 (2^2)

              vecX3 = createVecX' (2^3)
              vecY3 = createVec0 (2^3)
              a3 = createFlatMatrix 0 (2^3)

              vecX4 = createVecX' (2^4)
              vecY4 = createVec0 (2^4)
              a4 = createFlatMatrix 0 (2^4)

              vecX5 = createVecX' (2^5)
              vecY5 = createVec0 (2^5)
              a5 = createFlatMatrix 0 (2^5)

              vecX6 = createVecX' (2^6)
              vecY6 = createVec0 (2^6)
              a6 = createFlatMatrix 0 (2^6)

              vecX7 = createVecX' (2^7)
              vecY7 = createVec0 (2^7)
              a7 = createFlatMatrix 0 (2^7)

              vecX8 = createVecX' (2^8)
              vecY8 = createVec0 (2^8)
              a8 = createFlatMatrix 0 (2^8)

              vecX9 = createVecX' (2^9)
              vecY9 = createVec0 (2^9)
              a9 = createFlatMatrix 0 (2^9)

              vecX10 = createVecX' (2^10)
              vecY10 = createVec0 (2^10)
              a10 = createFlatMatrix 0 (2^10)

              vecX11 = createVecX' (2^11)
              vecY11 = createVec0 (2^11)
              a11 = createFlatMatrix 0 (2^11)

              vecX12 = createVecX' (2^12)
              vecY12 = createVec0 (2^12)
              a12 = createFlatMatrix 0 (2^12)


          t0 <- getCurrentTime
          -- t1 <- seq (gemv a1 vecX1 vecY1 alpha beta) getCurrentTime
          -- t2 <- seq (gemv a2 vecX2 vecY2 alpha beta) getCurrentTime
          -- t3 <- seq (gemv a3 vecX3 vecY3 alpha beta) getCurrentTime
          t4 <- seq (gemv a4 vecX4 vecY4 alpha beta) getCurrentTime
          t5 <- seq (gemv a5 vecX5 vecY5 alpha beta) getCurrentTime
          t6 <- seq (gemv a6 vecX6 vecY6 alpha beta) getCurrentTime        
          -- t0 <- getCurrentTime
          -- t5 <- seq (gemv a5 vecX5 vecY5 alpha beta) getCurrentTime
          -- t6 <- seq (gemv a6 vecX6 vecY6 alpha beta) getCurrentTime
          -- t7 <- seq (gemv a7 vecX7 vecY7 alpha beta) getCurrentTime
          -- t8 <- seq (gemv a8 vecX8 vecY8 alpha beta) getCurrentTime
          -- t9 <- seq (gemv a9 vecX9 vecY9 alpha beta) getCurrentTime
          
          -- t0 <- getCurrentTime
          -- t10 <- seq (gemv a10 vecX10 vecY10 alpha beta) getCurrentTime
          -- t11 <- seq (gemv a11 vecX11 vecY11 alpha beta) getCurrentTime
          -- t12 <- seq (gemv a12 vecX12 vecY12 alpha beta) getCurrentTime
          
          -- t0 <- getCurrentTime
          -- t13 <- seq (gemv a13 vecX13 vecY13 alpha beta) getCurrentTime
          -- t14 <- seq (gemv a14 vecX14 vecY14 alpha beta) getCurrentTime

          -- t0 <- getCurrentTime
          -- t15 <- seq (gemv a15 vecX15 vecY15 alpha beta) getCurrentTime
          -- t16 <- seq (gemv a16 vecX16 vecY16 alpha beta) getCurrentTime
          
          putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t6 t5) + (diffUTCTime t4 t0)) /8))
          -- putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t9 t8) +   (diffUTCTime t7 t6)+   (diffUTCTime t5 t0)) /8))
          
          -- putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t12 t11)  +  (diffUTCTime t10 t0)) /8))

          -- putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t14 t13)+ (diffUTCTime t13 t0)) /8))

          -- putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t16 t15)+ (diffUTCTime t15 t0)) /8))
          
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a1 vecX1 vecY1 tol) getCurrentTime
          -- t2 <- seq (tfqmr a1 vecX1 vecY1 tol) getCurrentTime
          -- t3 <- seq (tfqmr a1 vecX1 vecY1 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))       
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a2 vecX2 vecY2 tol) getCurrentTime
          -- t2 <- seq (tfqmr a2 vecX2 vecY2 tol) getCurrentTime
          -- t3 <- seq (tfqmr a2 vecX2 vecY2 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a3 vecX3 vecY3 tol) getCurrentTime
          -- t2 <- seq (tfqmr a3 vecX3 vecY3 tol) getCurrentTime
          -- t3 <- seq (tfqmr a3 vecX3 vecY3 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          t0 <- getCurrentTime
          t1 <- seq (tfqmr a4 vecX4 vecY4 tol) getCurrentTime
          t2 <- seq (tfqmr a4 vecX4 vecY4 tol) getCurrentTime
          t3 <- seq (tfqmr a4 vecX4 vecY4 tol) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))
          t0 <- getCurrentTime
          t1 <- seq (tfqmr a5 vecX5 vecY5 tol) getCurrentTime
          t2 <- seq (tfqmr a5 vecX5 vecY5 tol) getCurrentTime
          t3 <- seq (tfqmr a5 vecX5 vecY5 tol) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          t0 <- getCurrentTime
          t1 <- seq (tfqmr a6 vecX6 vecY6 tol) getCurrentTime
          t2 <- seq (tfqmr a6 vecX6 vecY6 tol) getCurrentTime
          t3 <- seq (tfqmr a6 vecX6 vecY6 tol) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a7 vecX7 vecY7 tol) getCurrentTime
          -- t2 <- seq (tfqmr a7 vecX7 vecY7 tol) getCurrentTime
          -- t3 <- seq (tfqmr a7 vecX7 vecY7 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a8 vecX8 vecY8 tol) getCurrentTime
          -- t2 <- seq (tfqmr a8 vecX8 vecY8 tol) getCurrentTime
          -- t3 <- seq (tfqmr a8 vecX8 vecY8 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a9 vecX9 vecY9 tol) getCurrentTime
          -- t2 <- seq (tfqmr a9 vecX9 vecY9 tol) getCurrentTime
          -- t3 <- seq (tfqmr a9 vecX9 vecY9 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r)) 


          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a10 vecX10 vecY10 tol) getCurrentTime
          -- t2 <- seq (tfqmr a10 vecX10 vecY10 tol) getCurrentTime
          -- t3 <- seq (tfqmr a10 vecX10 vecY10 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a11 vecX11 vecY11 tol) getCurrentTime
          -- t2 <- seq (tfqmr a11 vecX11 vecY11 tol) getCurrentTime
          -- t3 <- seq (tfqmr a11 vecX11 vecY11 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a12 vecX12 vecY12 tol) getCurrentTime
          -- t2 <- seq (tfqmr a12 vecX12 vecY12 tol) getCurrentTime
          -- t3 <- seq (tfqmr a12 vecX12 vecY12 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          

          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a13 vecX13 vecY13 tol) getCurrentTime
          -- t2 <- seq (tfqmr a13 vecX13 vecY13 tol) getCurrentTime
          -- t3 <- seq (tfqmr a13 vecX13 vecY13 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a14 vecX14 vecY14 tol) getCurrentTime
          -- t2 <- seq (tfqmr a14 vecX14 vecY14 tol) getCurrentTime
          -- t3 <- seq (tfqmr a14 vecX14 vecY14 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))


          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a15 vecX15 vecY15 tol) getCurrentTime
          -- t2 <- seq (tfqmr a15 vecX15 vecY15 tol) getCurrentTime
          -- t3 <- seq (tfqmr a15 vecX15 vecY15 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (tfqmr a16 vecX16 vecY16 tol) getCurrentTime
          -- t2 <- seq (tfqmr a16 vecX16 vecY16 tol) getCurrentTime
          -- t3 <- seq (tfqmr a16 vecX16 vecY16 tol) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))
          


-- main :: IO ()
-- main = do putStr "\n\nTFQMRs: \nMatrix size: "
--           let !n=2^2
--               !symSq=createUnSymSq 0 n
--               !vecX=createVecX' n
--               !vec0=createVec0 n
--           putStr $ "" ++ (show ( vecX)) ++ "X"
--           putStrLn $ "" ++ (show ( symSq))
--           t <- getCurrentTime
--           --t' <- seq (repeatNTimes 1 (conjgrad symSq vecX vec0)) getCurrentTime
--           t' <- seq (tfqmr symSq vecX 1e-10 vec0) getCurrentTime
--           putStrLn $ "" ++ (show  (tfqmr symSq vecX 1e-10 vec0))
--           putStrLn $ "Time taken (ms): " ++ (show ((diffUTCTime t' t)*1000000) )
--           --putStrLn $ "" ++ (show  (round4dp pi ))


-- test-suite tfqmr
--    ghc-options:         -O2
--                         -threaded
--                         -eventlog
--                         -rtsopts
--    type:                exitcode-stdio-1.0
--    default-language:    Haskell2010
--    main-is:             examples/tfqmr.hs
--    build-depends:       base,
--                         vector,
--                         hblas,
--                         time >= 1.6

