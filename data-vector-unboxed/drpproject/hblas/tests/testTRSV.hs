{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector.Unboxed as V
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


main :: IO ()
main = do putStr "\n\nTRSV:\n"
          let alpha, beta :: Float

              alpha = 1.0
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
              vecY1 = vecX1
              a1 = createSymLowSq 0 (2^1)

              vecX2 = createVecX' (2^2)
              vecY2 = vecX2
              a2 = createSymLowSq 0 (2^2)

              vecX3 = createVecX' (2^3)
              vecY3 = vecX3
              a3 = createSymLowSq 0 (2^3)

              vecX4 = createVecX' (2^4)
              vecY4 = vecX4
              a4 = createSymLowSq 0 (2^4)

              vecX5 = createVecX' (2^5)
              vecY5 = vecX5
              a5 = createSymLowSq 0 (2^5)

              vecX6 = createVecX' (2^6)
              vecY6 = vecX6
              a6 = createSymLowSq 0 (2^6)

              vecX7 = createVecX' (2^7)
              vecY7 = vecX7
              a7 = createSymLowSq 0 (2^7)

              vecX8 = createVecX' (2^8)
              vecY8 = vecX8
              a8 = createSymLowSq 0 (2^8)

              vecX9 = createVecX' (2^9)
              vecY9 = vecX9
              a9 = createSymLowSq 0 (2^9)

              vecX10 = createVecX' (2^10)
              vecY10 = vecX10
              a10 = createSymLowSq 0 (2^10)

              vecX11 = createVecX' (2^11)
              vecY11 = vecX11
              a11 = createSymLowSq 0 (2^11)

              vecX12 = createVecX' (2^12)
              vecY12 = vecX12
              a12 = createSymLowSq 0 (2^12)

              


          t0 <- getCurrentTime
          t1 <- seq (gemvFlat a1 vecX1 ) getCurrentTime
          t2 <- seq (gemvFlat a2 vecX2 ) getCurrentTime
          t3 <- seq (gemvFlat a3 vecX3 ) getCurrentTime
          t4 <- seq (gemvFlat a4 vecX4 ) getCurrentTime        
          
          t0 <- getCurrentTime
          t5 <- seq (gemvFlat a5 vecX5 ) getCurrentTime
          t6 <- seq (gemvFlat a6 vecX6 ) getCurrentTime
          t7 <- seq (gemvFlat a7 vecX7 ) getCurrentTime
          t8 <- seq (gemvFlat a8 vecX8 ) getCurrentTime
          t9 <- seq (gemvFlat a9 vecX9) getCurrentTime
          
          t0 <- getCurrentTime
          t10 <- seq (gemvFlat a10 vecX10 ) getCurrentTime
          t11 <- seq (gemvFlat a11 vecX11 ) getCurrentTime
          t12 <- seq (gemvFlat a12 vecX12 ) getCurrentTime


           
          -- putStr $ "" ++ (show ( a2)) 
          -- putStr $ "" ++ (show ( vecX2))
          
          putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t4 t3) + (diffUTCTime t2 t1) + (diffUTCTime t1 t0)) *1000000/8))
          putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t9 t8) +   (diffUTCTime t7 t6)+   (diffUTCTime t5 t0)) *1000000/8))
          
          putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t12 t11)  +  (diffUTCTime t10 t0)) *1000000/8))

          -- putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t14 t13)+ (diffUTCTime t13 t0)) *1000000/8))

          -- putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t16 t15)+ (diffUTCTime t15 t0)) *1000000/8))
          
          t0 <- getCurrentTime
          t1 <- seq (trsv a1 vecX1 'l' ) getCurrentTime
          t2 <- seq (trsv a1 vecX1 'l' ) getCurrentTime
          t3 <- seq (trsv a1 vecX1 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))       
          t0 <- getCurrentTime
          t1 <- seq (trsv a2 vecX2 'l' ) getCurrentTime
          t2 <- seq (trsv a2 vecX2 'l' ) getCurrentTime
          t3 <- seq (trsv a2 vecX2 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          -- putStr $ "" ++ (show ( trsv a2 vecX2 'l'))
          t0 <- getCurrentTime
          t1 <- seq (trsv a3 vecX3 'l' ) getCurrentTime
          t2 <- seq (trsv a3 vecX3 'l' ) getCurrentTime
          t3 <- seq (trsv a3 vecX3 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          t0 <- getCurrentTime
          t1 <- seq (trsv a4 vecX4 'l' ) getCurrentTime
          t2 <- seq (trsv a4 vecX4 'l' ) getCurrentTime
          t3 <- seq (trsv a4 vecX4 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          t0 <- getCurrentTime
          t1 <- seq (trsv a5 vecX5 'l' ) getCurrentTime
          t2 <- seq (trsv a5 vecX5 'l' ) getCurrentTime
          t3 <- seq (trsv a5 vecX5 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          t0 <- getCurrentTime
          t1 <- seq (trsv a6 vecX6 'l' ) getCurrentTime
          t2 <- seq (trsv a6 vecX6 'l' ) getCurrentTime
          t3 <- seq (trsv a6 vecX6 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          t0 <- getCurrentTime
          t1 <- seq (trsv a7 vecX7 'l' ) getCurrentTime
          t2 <- seq (trsv a7 vecX7 'l' ) getCurrentTime
          t3 <- seq (trsv a7 vecX7 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          t0 <- getCurrentTime
          t1 <- seq (trsv a8 vecX8 'l' ) getCurrentTime
          t2 <- seq (trsv a8 vecX8 'l' ) getCurrentTime
          t3 <- seq (trsv a8 vecX8 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          t0 <- getCurrentTime
          t1 <- seq (trsv a9 vecX9 'l' ) getCurrentTime
          t2 <- seq (trsv a9 vecX9 'l' ) getCurrentTime
          t3 <- seq (trsv a9 vecX9 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r)) 


          t0 <- getCurrentTime
          t1 <- seq (trsv a10 vecX10 'l' ) getCurrentTime
          t2 <- seq (trsv a10 vecX10 'l' ) getCurrentTime
          t3 <- seq (trsv a10 vecX10 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          t0 <- getCurrentTime
          t1 <- seq (trsv a11 vecX11 'l' ) getCurrentTime
          t2 <- seq (trsv a11 vecX11 'l' ) getCurrentTime
          t3 <- seq (trsv a11 vecX11 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          t0 <- getCurrentTime
          t1 <- seq (trsv a12 vecX12 'l' ) getCurrentTime
          t2 <- seq (trsv a12 vecX12 'l' ) getCurrentTime
          t3 <- seq (trsv a12 vecX12 'l' ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          

          -- t0 <- getCurrentTime
          -- t1 <- seq (trsv a13 vecX13 'l' ) getCurrentTime
          -- t2 <- seq (trsv a13 vecX13 'l' ) getCurrentTime
          -- t3 <- seq (trsv a13 vecX13 'l' ) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (trsv a14 vecX14 'l' ) getCurrentTime
          -- t2 <- seq (trsv a14 vecX14 'l' ) getCurrentTime
          -- t3 <- seq (trsv a14 vecX14 'l' ) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))


          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a15 vecX15 vecY15) getCurrentTime
          -- t2 <- seq (conjugateGradient a15 vecX15 vecY15) getCurrentTime
          -- t3 <- seq (conjugateGradient a15 vecX15 vecY15) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))          
          -- t0 <- getCurrentTime
          -- t1 <- seq (conjugateGradient a16 vecX16 vecY16) getCurrentTime
          -- t2 <- seq (conjugateGradient a16 vecX16 vecY16) getCurrentTime
          -- t3 <- seq (conjugateGradient a16 vecX16 vecY16) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          





          




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





