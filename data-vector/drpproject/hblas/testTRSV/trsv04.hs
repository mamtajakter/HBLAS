{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
import Data.Time



main :: IO ()
main = do putStr "\n\nTRSV 04:\n"
          let alpha, beta :: Float

              alpha = 1.0
              beta = 0.0
              r = 3



              vecX = createVecX' 16630
              vecY = createVec0 16630
              a = createSymLowSq 0 16630

          t0 <- getCurrentTime
          t1 <- deepseq (V.length vecX) getCurrentTime
          t2 <- deepseq (V.length vecY) getCurrentTime
          t3 <- deepseq (V.length a) getCurrentTime

          putStrLn $ "Discard Test : " ++ (show (  (diffUTCTime t3 t2)+(diffUTCTime t1 t0)))

          t0 <- getCurrentTime
          t1 <- seq (trsv a vecX 'l') getCurrentTime
          t2 <- seq (trsv a vecX 'l') getCurrentTime
          t3 <- seq (trsv a vecX 'l') getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))           
         

