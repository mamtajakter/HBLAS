module Main where

import Data.Time
import qualified Data.Vector.Unboxed as Vec
import HBLAS.Level1
import HBLAS.IO.Vector
import Data.Time.Clock.POSIX (getPOSIXTime)

-- vec1,vec2 :: FilePath
-- vec1 = "../data/vecX10.csv"
-- vec2 = "../data/vecY10.csv"
repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action


main :: IO ()
main = do putStr "\n\nLevel-1 Test: \nList size: "
          let c, alpha, beta :: Float
              n1 = 20
              c = 0.5
              r = 3
              alpha=1.0
              beta=1.0
              n=2^n1
              vecX :: Vec.Vector Float
              vecX = createVecX' n
              vecY :: Vec.Vector Float
              vecY = vecX
              vecZ :: Vec.Vector Float
              vecZ = vecX
              vec0 :: Vec.Vector Float
              vec0= createVec0 n
          putStrLn $ "Vector size: 2^" ++ show n1 ++ ": "

          t <- getCurrentTime
          t' <- seq ( ( seq ( seq (axpyaxpy' alpha beta vecX vecY vecZ) (axpyaxpy' alpha beta vecX vecY vecZ)) (axpyaxpy' alpha beta vecX vecY vecZ)) ) getCurrentTime
          putStrLn $ "AXPYAXPY : " ++ (show ((diffUTCTime t' t) *1000000/r))

          t <- getCurrentTime
          t' <- seq (axpyaxpy' alpha beta vecX vecY vecZ) getCurrentTime
          putStrLn $ "AXPYAXPY : " ++ (show ((diffUTCTime t' t) *1000000))

          -- t <- getCurrentTime
          -- t' <- seq (repeatNTimes r (axpy alpha vecX vecY)) getCurrentTime
          -- putStrLn $ "AXPY: " ++ (show ((diffUTCTime t' t) *1000000/r))

          t <- getCurrentTime
          t' <- seq (axpy alpha vecX vecY) getCurrentTime
          putStrLn $ "AXPY: " ++ (show ((diffUTCTime t' t) *1000000))

          -- t <- getCurrentTime
          -- t' <- seq (repeatNTimes r (scal alpha vecX)) getCurrentTime
          -- putStrLn $ "SCAL: " ++ (show ((diffUTCTime t' t) *1000000/r))

          t <- getCurrentTime
          t' <- seq (scal alpha vecX) getCurrentTime
          putStrLn $ "SCAL: " ++ (show ((diffUTCTime t' t) *1000000))
 
          t <- getCurrentTime
          t' <- seq (dot vecX vecY) getCurrentTime
          putStrLn $ "DOT: " ++ (show ((diffUTCTime t' t) *1000000))

          t <- getCurrentTime
          t' <- seq ( nrm2 vecX) getCurrentTime
          putStrLn $ "NRM2: " ++ (show ((diffUTCTime t' t) *1000000))

          t <- getCurrentTime
          t' <- seq (asum vecX) getCurrentTime
          putStrLn $ "ASUM: " ++ (show ((diffUTCTime t' t) *1000000))

          t <- getCurrentTime
          t' <- seq (idamax vecX) getCurrentTime
          putStrLn $ "I_AMAX: " ++ (show ((diffUTCTime t' t) *1000000))

          -- t <- getCurrentTime
          -- t' <- seq (repeatNTimes r (rot vecX vecY c)) getCurrentTime
          -- putStrLn $ "ROT: " ++ (show ((diffUTCTime t' t) *1000000/r))

          t <- getCurrentTime
          t' <- seq (rot vecX vecY c) getCurrentTime
          putStrLn $ "ROT: " ++ (show ((diffUTCTime t' t) *1000000))

-- test-suite level1
--    ghc-options:         -O2
--                         -threaded
--                         -eventlog
--                         -rtsopts
--    type:                exitcode-stdio-1.0
--    default-language:    Haskell2010
--    main-is:             tests/testLevel1.hs
--    build-depends:       base,
--                         vector,
--                         containers,
--                         hblas,
--                         time >= 1.6