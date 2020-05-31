module Main where

import Data.Time
import qualified Data.Vector.Unboxed as Vec
import HBLAS.Level1
import HBLAS.IO.Vector
import Data.Time.Clock.POSIX (getPOSIXTime)

-- vec1,vec2 :: FilePath
-- vec1 = "../data/vecX10.csv"
-- vec2 = "../data/vecY10.csv"

round4dp :: (Num n, RealFrac n,  Fractional n, Ord n, Floating n)=>n -> n
round4dp x= fromIntegral (round $ x * 1e4) / 1e4

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action


main :: IO ()
main = do putStr "\n\nLevel-1 Test: \nList size: "
          let c, alpha, beta :: Float
              c = 0.5
              r = 3
              alpha=1.0
              beta=1.0
              n=2^1
              vecX :: Vec.Vector Float
              vecX = createVecX' n
              vecY :: Vec.Vector Float
              vecY = vecX
              vecZ :: Vec.Vector Float
              vecZ = vecX
              vec0 :: Vec.Vector Float
              vec0= createVec0 n
              n1=2^1
              vecX1 :: Vec.Vector Float
              vecX1 = createVecX' n1
              vecY1 :: Vec.Vector Float
              vecY1 = vecX
              vecZ1 :: Vec.Vector Float
              vecZ1 = vecX
              vec01 :: Vec.Vector Float
              vec01= createVec0 n1
              n2=2^2
              vecX2 :: Vec.Vector Float
              vecX2 = createVecX' n2
              vecY2 :: Vec.Vector Float
              vecY2 = vecX
              vecZ2 :: Vec.Vector Float
              vecZ2 = vecX
              vec02 :: Vec.Vector Float
              vec02= createVec0 n2
              n3=2^3
              vecX3 :: Vec.Vector Float
              vecX3 = createVecX' n3
              vecY3 :: Vec.Vector Float
              vecY3 = vecX
              vecZ3 :: Vec.Vector Float
              vecZ3 = vecX
              vec03 :: Vec.Vector Float
              vec03= createVec0 n3
          putStrLn $ "Vector size: " ++ show n3 ++ ": "

          t0 <- getCurrentTime
          t1 <- seq (axpyaxpy' alpha beta vecX1 vecY1 vecZ1) getCurrentTime
          t2 <- seq (axpyaxpy' alpha beta vecX1 vecY1 vecZ1) getCurrentTime
          t3 <- seq (axpyaxpy' alpha beta vecX1 vecY1 vecZ1) getCurrentTime
          --t' <- seq ( ( seq ( seq (axpyaxpy' alpha beta vecX1 vecY1 vecZ1) (axpyaxpy' alpha beta vecX1 vecY1 vecZ1)) (axpyaxpy' alpha beta vecX1 vecY1 vecZ1)) ) getCurrentTime
          putStrLn $ "AXPYAXPY 2^1 : " ++ (show (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/3))

          t0 <- getCurrentTime
          t1 <- seq (axpyaxpy' alpha beta vecX2 vecY2 vecZ2) getCurrentTime
          t2 <- seq (axpyaxpy' alpha beta vecX2 vecY2 vecZ2) getCurrentTime
          t3 <- seq (axpyaxpy' alpha beta vecX2 vecY2 vecZ2) getCurrentTime
          --t' <- seq ( ( seq ( seq (axpyaxpy' alpha beta vecX2 vecY2 vecZ2) (axpyaxpy' alpha beta vecX2 vecY2 vecZ2)) (axpyaxpy' alpha beta vecX2 vecY2 vecZ2)) ) getCurrentTime
          putStrLn $ "AXPYAXPY 2^2 : " ++ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))

          t <- getCurrentTime
          t1 <- seq (axpyaxpy' alpha beta vecX3 vecY3 vecZ3) getCurrentTime
          t2 <- seq (axpyaxpy' alpha beta vecX3 vecY3 vecZ3) getCurrentTime
          t3 <- seq (axpyaxpy' alpha beta vecX3 vecY3 vecZ3) getCurrentTime
          --t' <- seq ( ( seq ( seq (axpyaxpy' alpha beta vecX3 vecY3 vecZ3) (axpyaxpy' alpha beta vecX3 vecY3 vecZ3)) (axpyaxpy' alpha beta vecX3 vecY3 vecZ3)) ) getCurrentTime
          putStrLn $ "AXPYAXPY 2^3 : " ++ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) *1000000/r))

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