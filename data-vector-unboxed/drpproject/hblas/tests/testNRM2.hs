module Main where

import Data.Time
import qualified Data.Vector.Unboxed as Vec
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
main = do putStr "\n\nNRM2:\n"
          let alpha :: Float
              r = 3
              alpha=1.0

              vecX10 = createVecX' (2^10)

              vecX11 = createVecX' (2^11)

              vecX12 = createVecX' (2^12)

              vecX13 = createVecX' (2^13)

              vecX14 = createVecX' (2^14)

              vecX15 = createVecX' (2^15)

              vecX16 = createVecX' (2^16)

              vecX17 = createVecX' (2^17)

              vecX18 = createVecX' (2^18)

              vecX19 = createVecX' (2^19)

              vecX20 = createVecX' (2^20)

              vecX21 = createVecX' (2^21)

              vecX22 = createVecX' (2^22)

              vecX23 = createVecX' (2^23)

              vecX24 = createVecX' (2^24)

          t0 <- getCurrentTime
          t10 <- seq (scal  alpha vecX10) getCurrentTime
          t11 <- seq (scal  alpha vecX11) getCurrentTime
          t12 <- seq (scal  alpha vecX12) getCurrentTime
          t13 <- seq (scal  alpha vecX13) getCurrentTime
          t14 <- seq (scal  alpha vecX14) getCurrentTime
          t15 <- seq (scal  alpha vecX15) getCurrentTime
          t16 <- seq (scal  alpha vecX16) getCurrentTime
          t17 <- seq (scal  alpha vecX17) getCurrentTime
          t18 <- seq (scal  alpha vecX18) getCurrentTime
          t19 <- seq (scal  alpha vecX19) getCurrentTime
          t20 <- seq (scal  alpha vecX20) getCurrentTime
          t21 <- seq (scal  alpha vecX21 ) getCurrentTime
          t22 <- seq (scal  alpha vecX22 ) getCurrentTime
          t23 <- seq (scal  alpha vecX23 ) getCurrentTime
          t24 <- seq (scal  alpha vecX24 ) getCurrentTime
          putStrLn $ "Discard Test : " ++ (show (((diffUTCTime t24 t23) + (diffUTCTime t22 t21) + (diffUTCTime t20 t19) + (diffUTCTime t18 t17) + (diffUTCTime t16 t15) + (diffUTCTime t14 t13) + (diffUTCTime t12 t11) + (diffUTCTime t10 t0)) *1000000/8))

          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX10 ) getCurrentTime
          t2 <- seq (nrm2 vecX10 ) getCurrentTime
          t3 <- seq (nrm2 vecX10 ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX11 ) getCurrentTime
          t2 <- seq (nrm2 vecX11 ) getCurrentTime
          t3 <- seq (nrm2 vecX11 ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX12 ) getCurrentTime
          t2 <- seq (nrm2 vecX12 ) getCurrentTime
          t3 <- seq (nrm2 vecX12 ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX13 ) getCurrentTime
          t2 <- seq (nrm2 vecX13 ) getCurrentTime
          t3 <- seq (nrm2 vecX13 ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX14 ) getCurrentTime
          t2 <- seq (nrm2 vecX14 ) getCurrentTime
          t3 <- seq (nrm2 vecX14 ) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX15 ) getCurrentTime
          t2 <- seq (nrm2 vecX15 ) getCurrentTime
          t3 <- seq (nrm2 vecX15 ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX16 ) getCurrentTime
          t2 <- seq (nrm2 vecX16 ) getCurrentTime
          t3 <- seq (nrm2 vecX16 ) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX17 ) getCurrentTime
          t2 <- seq (nrm2 vecX17 ) getCurrentTime
          t3 <- seq (nrm2 vecX17 ) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX18 ) getCurrentTime
          t2 <- seq (nrm2 vecX18 ) getCurrentTime
          t3 <- seq (nrm2 vecX18 ) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX19 ) getCurrentTime
          t2 <- seq (nrm2 vecX19 ) getCurrentTime
          t3 <- seq (nrm2 vecX19 ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX20 ) getCurrentTime
          t2 <- seq (nrm2 vecX20 ) getCurrentTime
          t3 <- seq (nrm2 vecX20 ) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX21 ) getCurrentTime
          t2 <- seq (nrm2 vecX21 ) getCurrentTime
          t3 <- seq (nrm2 vecX21 ) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX22 ) getCurrentTime
          t2 <- seq (nrm2 vecX22 ) getCurrentTime
          t3 <- seq (nrm2 vecX22 ) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX23 ) getCurrentTime
          t2 <- seq (nrm2 vecX23 ) getCurrentTime
          t3 <- seq (nrm2 vecX23 ) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX24 ) getCurrentTime
          t2 <- seq (nrm2 vecX24 ) getCurrentTime
          t3 <- seq (nrm2 vecX24 ) getCurrentTime
          putStrLn $  (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))
          
          
-- test-suite scal
--    ghc-options:         -O2
--                         -threaded
--                         -eventlog
--                         -rtsopts
--    type:                exitcode-stdio-1.0
--    default-language:    Haskell2010
--    main-is:             tests/testScal.hs
--    build-depends:       base,
--                         vector,
--                         containers,
--                         hblas,
--                         time >= 1.6


