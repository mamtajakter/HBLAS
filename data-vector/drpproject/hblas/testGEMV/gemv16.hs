{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector as V
import HBLAS.IO.Vector
import HBLAS.Level1
import HBLAS.Level2
import Data.Time
import Control.DeepSeq



main :: IO ()
main = do putStr "\n\nGEMV 16:\n"
          let alpha, beta :: Float

              alpha = 1.0
              beta = 0.0
              r = 3



              vecX = createVecX' 79160
              vecY = createVec0 79160
              a = createSymSq 0 79160

          t0 <- getCurrentTime
          t1 <- seq (V.length vecX) getCurrentTime
          t2 <- seq (V.length vecY) getCurrentTime
          t3 <- seq (V.length a) getCurrentTime

          putStrLn $ "Discard Test : " ++ (show ((  (diffUTCTime t3 t2)+(diffUTCTime t1 t0)) /8))

          t0 <- getCurrentTime
          t1 <- deepseq (gemv a vecX vecY alpha beta) getCurrentTime
          t2 <- deepseq (gemv a vecX vecY alpha beta) getCurrentTime
          t3 <- deepseq (gemv a vecX vecY alpha beta) getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))           
         

