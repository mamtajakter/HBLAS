{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector.Unboxed as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
import Data.Time



main :: IO ()
main = do putStr "\n\nGEMV 15:\n"
          let alpha, beta :: Float

              alpha = 1.0
              beta = 0.0
              r = 3
              vecX :: V.Vector Float
              vecY  :: V.Vector Float
              a :: V.Vector Float 


              vecX = createVecX' 73950
              vecY = vecX
              a = createVecX' (73950^2)

          t0 <- getCurrentTime
          t1 <- seq (V.length vecX) getCurrentTime
          t2 <- seq (V.length vecY) getCurrentTime
          t3 <- seq (V.length a) getCurrentTime

          putStrLn $ "Discard Test : " ++ (show ((  (diffUTCTime t3 t2)+(diffUTCTime t1 t0)) /8))

          t0 <- getCurrentTime
          t1 <- seq (gemv a vecX vecY alpha beta) getCurrentTime
          t2 <- seq (gemv a vecX vecY alpha beta) getCurrentTime
          t3 <- seq (gemv a vecX vecY alpha beta) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))       
         

