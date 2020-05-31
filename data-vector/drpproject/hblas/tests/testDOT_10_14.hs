module Main where

import Data.Time
import qualified Data.Vector as Vec
import HBLAS.Level1
import HBLAS.IO.Vector
import Data.Time.Clock.POSIX (getPOSIXTime)

round4dp :: (Num n, RealFrac n,  Fractional n, Ord n, Floating n)=>n -> n
round4dp x= fromIntegral (round $ x * 1e4) / 1e4

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action


main :: IO ()
main = do putStr "\n\nDOT 10-14:\n"
          let alpha :: Float
              r = 3
              alpha=1.0

              vecX10 = createVecX' (2^10)
              vecY10 = vecX10

              vecX11 = createVecX' (2^11)
              vecY11 = vecX11

              vecX12 = createVecX' (2^12)
              vecY12 = vecX12

              vecX13 = createVecX' (2^13)
              vecY13 = vecX13

              vecX14 = createVecX' (2^14)
              vecY14 = vecX14

              vecX15 = createVecX' (2^15)
              vecY15 = vecX15

              vecX16 = createVecX' (2^16)
              vecY16 = vecX16

              vecX17 = createVecX' (2^17)
              vecY17 = vecX17

              vecX18 = createVecX' (2^18)
              vecY18 = vecX18

              vecX19 = createVecX' (2^19)
              vecY19 = vecX19

              vecX20 = createVecX' (2^20)
              vecY20 = vecX20

              vecX21 = createVecX' (2^21)
              vecY21 = vecX21

              vecX22 = createVecX' (2^22)
              vecY22 = vecX22

              vecX23 = createVecX' (2^23)
              vecY23 = vecX23

              vecX24 = createVecX' (2^24)
              vecY24 = vecX24

--10-14
          t0 <- getCurrentTime
          t10 <- seq (axpy alpha  vecX10 vecY10) getCurrentTime
          t11 <- seq (axpy alpha  vecX11 vecY11) getCurrentTime
          t12 <- seq (axpy alpha  vecX12 vecY12) getCurrentTime
          t13 <- seq (axpy alpha  vecX13 vecY13) getCurrentTime
          t14 <- seq (axpy alpha  vecX14 vecY14) getCurrentTime

--15-18
          -- t0 <- getCurrentTime
          -- t15 <- seq (axpy alpha  vecX15 vecY15) getCurrentTime
          -- t16 <- seq (axpy alpha  vecX16 vecY16) getCurrentTime
          -- t17 <- seq (axpy alpha  vecX17 vecY17) getCurrentTime
          -- t18 <- seq (axpy alpha  vecX18 vecY18) getCurrentTime

--19-21
          -- t0 <- getCurrentTime
          -- t19 <- seq (axpy alpha  vecX19 vecY19) getCurrentTime
          -- t20 <- seq (axpy alpha  vecX20 vecY20) getCurrentTime
          -- t21 <- seq (axpy alpha  vecX21 vecY21) getCurrentTime

--22-24
          -- t0 <- getCurrentTime
          -- t22 <- seq (axpy alpha  vecX22 vecY22) getCurrentTime
          -- t23 <- seq (axpy alpha  vecX23 vecY23) getCurrentTime
          -- t24 <- seq (axpy alpha  vecX24 vecY24) getCurrentTime


          putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t14 t13) + (diffUTCTime t12 t11) + (diffUTCTime t10 t0)) *1000000/8))
          -- putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t18 t17) +   (diffUTCTime t16 t15)+   (diffUTCTime t15 t0)) *1000000/8))
          -- putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t21 t20)  +  (diffUTCTime t19 t0)) *1000000/8))
          -- putStrLn $ "Discard Test : " ++ (show (( (diffUTCTime t24 t23)+ (diffUTCTime t22 t0)) *1000000/8))

--10-14
          t0 <- getCurrentTime
          t1 <- seq (dot vecX10 vecY10) getCurrentTime
          t2 <- seq (dot vecX10 vecY10) getCurrentTime
          t3 <- seq (dot vecX10 vecY10) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (dot vecX11 vecY11) getCurrentTime
          t2 <- seq (dot vecX11 vecY11) getCurrentTime
          t3 <- seq (dot vecX11 vecY11) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (dot vecX12 vecY12) getCurrentTime
          t2 <- seq (dot vecX12 vecY12) getCurrentTime
          t3 <- seq (dot vecX12 vecY12) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (dot vecX13 vecY13) getCurrentTime
          t2 <- seq (dot vecX13 vecY13) getCurrentTime
          t3 <- seq (dot vecX13 vecY13) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (dot vecX14 vecY14) getCurrentTime
          t2 <- seq (dot vecX14 vecY14) getCurrentTime
          t3 <- seq (dot vecX14 vecY14) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
--15-18 
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX15 vecY15) getCurrentTime
          -- t2 <- seq (dot vecX15 vecY15) getCurrentTime
          -- t3 <- seq (dot vecX15 vecY15) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX16 vecY16) getCurrentTime
          -- t2 <- seq (dot vecX16 vecY16) getCurrentTime
          -- t3 <- seq (dot vecX16 vecY16) getCurrentTime
          -- putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX17 vecY17) getCurrentTime
          -- t2 <- seq (dot vecX17 vecY17) getCurrentTime
          -- t3 <- seq (dot vecX17 vecY17) getCurrentTime
          -- putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX18 vecY18) getCurrentTime
          -- t2 <- seq (dot vecX18 vecY18) getCurrentTime
          -- t3 <- seq (dot vecX18 vecY18) getCurrentTime
          -- putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
-- --19-21 
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX19 vecY19) getCurrentTime
          -- t2 <- seq (dot vecX19 vecY19) getCurrentTime
          -- t3 <- seq (dot vecX19 vecY19) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX20 vecY20) getCurrentTime
          -- t2 <- seq (dot vecX20 vecY20) getCurrentTime
          -- t3 <- seq (dot vecX20 vecY20) getCurrentTime
          -- putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX21 vecY21) getCurrentTime
          -- t2 <- seq (dot vecX21 vecY21) getCurrentTime
          -- t3 <- seq (dot vecX21 vecY21) getCurrentTime
          -- putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
-- --22-24          
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX22 vecY22) getCurrentTime
          -- t2 <- seq (dot vecX22 vecY22) getCurrentTime
          -- t3 <- seq (dot vecX22 vecY22) getCurrentTime
          -- putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
 
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX23 vecY23) getCurrentTime
          -- t2 <- seq (dot vecX23 vecY23) getCurrentTime
          -- t3 <- seq (dot vecX23 vecY23) getCurrentTime
          -- putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          -- t0 <- getCurrentTime
          -- t1 <- seq (dot vecX24 vecY24) getCurrentTime
          -- t2 <- seq (dot vecX24 vecY24) getCurrentTime
          -- t3 <- seq (dot vecX24 vecY24) getCurrentTime
          -- putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          
-- test-suite axpy
--    ghc-options:         -O2
--                         -threaded
--                         -eventlog
--                         -rtsopts
--    type:                exitcode-stdio-1.0
--    default-language:    Haskell2010
--    main-is:             tests/testAxpy.hs
--    build-depends:       base,
--                         vector,
--                         containers,
--                         hblas,
--                         time >= 1.6
