{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
import HBLAS.Class
import Data.Time

--a function to round a floating point number to atmost 4 decimal places
round4dp :: (Num n, RealFrac n,  Fractional n, Ord n, Floating n)=>n -> n
round4dp x= fromIntegral (round $ x * 1e4) / 1e4

{-# INLINE repeatNTimes #-}
repeatNTimes 0 _ = return ()
repeatNTimes !n !action =
 do
  action
  repeatNTimes (n-1) action

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
main = do putStr "\n\nCGM 01-09:\n"
          let alpha, beta :: Float

              alpha = 1.0
              beta = 0.0
              r = 3

              vecX1 = createVecX' (2^1)
              vecY1 = createVec0 (2^1)
              a1 = createSymSq 0 (2^1)

              vecX2 = createVecX' (2^2)
              vecY2 = createVec0 (2^2)
              a2 = createSymSq 0 (2^2)

              vecX3 = createVecX' (2^3)
              vecY3 = createVec0 (2^3)
              a3 = createSymSq 0 (2^3)

              vecX4 = createVecX' (2^4)
              vecY4 = createVec0 (2^4)
              a4 = createSymSq 0 (2^4)

              vecX5 = createVecX' (2^5)
              vecY5 = createVec0 (2^5)
              a5 = createSymSq 0 (2^5)

              vecX6 = createVecX' (2^6)
              vecY6 = createVec0 (2^6)
              a6 = createSymSq 0 (2^6)

              vecX7 = createVecX' (2^7)
              vecY7 = createVec0 (2^7)
              a7 = createSymSq 0 (2^7)

              vecX8 = createVecX' (2^8)
              vecY8 = createVec0 (2^8)
              a8 = createSymSq 0 (2^8)

              vecX9 = createVecX' (2^9)
              vecY9 = createVec0 (2^9)
              a9 = createSymSq 0 (2^9)

              vecX10 = createVecX' (2^10)
              vecY10 = createVec0 (2^10)
              a10 = createSymSq 0 (2^10)

              vecX11 = createVecX' (2^11)
              vecY11 = createVec0 (2^11)
              a11 = createSymSq 0 (2^11)

              vecX12 = createVecX' (2^12)
              vecY12 = createVec0 (2^12)
              a12 = createSymSq 0 (2^12)

              vecX13 = createVecX' (2^13)
              vecY13 = createVec0 (2^13)
              a13 = createSymSq 0 (2^13)

              vecX14 = createVecX' (2^14)
              vecY14 = createVec0 (2^14)
              a14 = createSymSq 0 (2^14)

              vecX15 = createVecX' (2^15)
              vecY15 = createVec0 (2^15)
              a15 = createSymSq 0 (2^15)

              vecX16 = createVecX' (2^16)
              vecY16 = createVec0 (2^16)
              a16 = createSymSq 0 (2^16)


          t0 <- getCurrentTime
          t1 <- seq (gemv a1 vecX1 vecY1 alpha beta) getCurrentTime
          t2 <- seq (gemv a2 vecX2 vecY2 alpha beta) getCurrentTime
          t3 <- seq (gemv a3 vecX3 vecY3 alpha beta) getCurrentTime
          t4 <- seq (gemv a4 vecX4 vecY4 alpha beta) getCurrentTime        
          t0 <- getCurrentTime
          t5 <- seq (gemv a5 vecX5 vecY5 alpha beta) getCurrentTime
          t6 <- seq (gemv a6 vecX6 vecY6 alpha beta) getCurrentTime
          t7 <- seq (gemv a7 vecX7 vecY7 alpha beta) getCurrentTime
          t8 <- seq (gemv a8 vecX8 vecY8 alpha beta) getCurrentTime
          t9 <- seq (gemv a9 vecX9 vecY9 alpha beta) getCurrentTime
          
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
          
          putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t4 t3) + (diffUTCTime t2 t1) + (diffUTCTime t1 t0)) /8))
          putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t9 t8) +   (diffUTCTime t7 t6)+   (diffUTCTime t5 t0)) /8))
          
          -- putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t12 t11)  +  (diffUTCTime t10 t0)) /8))

          -- putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t14 t13)+ (diffUTCTime t13 t0)) /8))

          -- putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t16 t15)+ (diffUTCTime t15 t0)) /8))
          
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a1 vecX1 vecY1) getCurrentTime
          t2 <- seq (conjugateGradient a1 vecX1 vecY1) getCurrentTime
          t3 <- seq (conjugateGradient a1 vecX1 vecY1) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))       
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a2 vecX2 vecY2) getCurrentTime
          t2 <- seq (conjugateGradient a2 vecX2 vecY2) getCurrentTime
          t3 <- seq (conjugateGradient a2 vecX2 vecY2) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a3 vecX3 vecY3) getCurrentTime
          t2 <- seq (conjugateGradient a3 vecX3 vecY3) getCurrentTime
          t3 <- seq (conjugateGradient a3 vecX3 vecY3) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a4 vecX4 vecY4) getCurrentTime
          t2 <- seq (conjugateGradient a4 vecX4 vecY4) getCurrentTime
          t3 <- seq (conjugateGradient a4 vecX4 vecY4) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a5 vecX5 vecY5) getCurrentTime
          t2 <- seq (conjugateGradient a5 vecX5 vecY5) getCurrentTime
          t3 <- seq (conjugateGradient a5 vecX5 vecY5) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a6 vecX6 vecY6) getCurrentTime
          t2 <- seq (conjugateGradient a6 vecX6 vecY6) getCurrentTime
          t3 <- seq (conjugateGradient a6 vecX6 vecY6) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a7 vecX7 vecY7) getCurrentTime
          t2 <- seq (conjugateGradient a7 vecX7 vecY7) getCurrentTime
          t3 <- seq (conjugateGradient a7 vecX7 vecY7) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a8 vecX8 vecY8) getCurrentTime
          t2 <- seq (conjugateGradient a8 vecX8 vecY8) getCurrentTime
          t3 <- seq (conjugateGradient a8 vecX8 vecY8) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))
          t0 <- getCurrentTime
          t1 <- seq (conjugateGradient a9 vecX9 vecY9) getCurrentTime
          t2 <- seq (conjugateGradient a9 vecX9 vecY9) getCurrentTime
          t3 <- seq (conjugateGradient a9 vecX9 vecY9) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r)) 


          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a10 vecX10 vecY10) getCurrentTime
          -- t2 <- seq (conjugateGradient a10 vecX10 vecY10) getCurrentTime
          -- t3 <- seq (conjugateGradient a10 vecX10 vecY10) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a11 vecX11 vecY11) getCurrentTime
          -- t2 <- seq (conjugateGradient a11 vecX11 vecY11) getCurrentTime
          -- t3 <- seq (conjugateGradient a11 vecX11 vecY11) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a12 vecX12 vecY12) getCurrentTime
          -- t2 <- seq (conjugateGradient a12 vecX12 vecY12) getCurrentTime
          -- t3 <- seq (conjugateGradient a12 vecX12 vecY12) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          

          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a13 vecX13 vecY13) getCurrentTime
          -- t2 <- seq (conjugateGradient a13 vecX13 vecY13) getCurrentTime
          -- t3 <- seq (conjugateGradient a13 vecX13 vecY13) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a14 vecX14 vecY14) getCurrentTime
          -- t2 <- seq (conjugateGradient a14 vecX14 vecY14) getCurrentTime
          -- t3 <- seq (conjugateGradient a14 vecX14 vecY14) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))


          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a15 vecX15 vecY15) getCurrentTime
          -- t2 <- seq (conjugateGradient a15 vecX15 vecY15) getCurrentTime
          -- t3 <- seq (conjugateGradient a15 vecX15 vecY15) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a16 vecX16 vecY16) getCurrentTime
          -- t2 <- seq (conjugateGradient a16 vecX16 vecY16) getCurrentTime
          -- t3 <- seq (conjugateGradient a16 vecX16 vecY16) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))
          





          




          {-
          putStr $ "" ++ (show (length vecX)) ++ "X"
          putStrLn $ "" ++ (show (length symSq))
          t <- getCurrentTime
          --t' <- seq (repeatNTimes 1 (conjgrad symSq vecX vec0)) getCurrentTime
          t' <- seq (conjugateGradient symSq vecX vec0) getCurrentTime
          -- putStrLn $ "" ++ (show  (conjugateGradient symSq vecX))
          putStrLn $ "Time taken (ms): " ++ (show ((diffUTCTime t' t)) )
          --putStrLn $ "" ++ (show  (round4dp pi ))-}


-- test-suite cg
--    ghc-options:         -O2
--                         -threaded
--                         -eventlog
--                         -rtsopts
--    type:                exitcode-stdio-1.0
--    default-language:    Haskell2010
--    main-is:             examples/cgm.hs
--    build-depends:       base,
--                         vector,
--                         hblas,
--                         time >= 1.6


{-

1. Because of laziness, it does not evaluate the parameters first.
add:: Int->Int->Int
add m n = m+n

now if I call add function with a= add 2 (1+4)
b= add a 5

then GHC will replace b=add (add 2 (1+4)) 5
b= add (add 2 5) 5
b= add (2+5) 5
b= add 7 5
b= 7+5
b=12

GHC does all these unnecessary lines because of laziness, GHC does not want to evaluate anything if it does not need to.

So, to force it to work, use bang pattern on each parameters of a function

2. Because fusion does not happen unless if you doent inline it

we know fusion happens when it gets this pattern∷
map f (map g l) = map (f.g) l

so if it gets f= map g l
 and if it also gets  map f g,GHC cant math to the fusion pattern.

 so, in every function type definition, write {-# INLINE function_name #-}
-}


