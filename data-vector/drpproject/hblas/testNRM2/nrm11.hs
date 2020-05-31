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
main = do putStr "\n\n NRM2  11:\n"
          let alpha :: Float
              r = 3
              alpha=1.0

              vecX = createVecX' (5264000000)
              vecY = vecX

          t0 <- getCurrentTime
          t1 <- seq (length vecX) getCurrentTime
          putStrLn $ "Discard Test : " ++ (show ( diffUTCTime t1 t0))

          t0 <- getCurrentTime
          t1 <- seq (nrm2 vecX) getCurrentTime
          t2 <- seq (nrm2 vecX) getCurrentTime
          t3 <- seq (nrm2 vecX) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0))/r))
          
          
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
