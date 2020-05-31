{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.List as V
import HBLAS.IO.List
import HBLAS.Level1
import HBLAS.Level2
import Data.Time


main :: IO ()
main = do putStr "\n\nSYMV 03:\n"
          let alpha, beta :: Float

              alpha = 1.0
              beta = 0.0
              r = 3



              vecX = createVecX' 11420 
              vecY = createVec0 11420
              a = createSymLowTri 0 11420

          t0 <- getCurrentTime
          t1 <- seq (V.length vecX) getCurrentTime
          t2 <- seq (V.length vecY) getCurrentTime
          t3 <- seq (V.length a) getCurrentTime

          putStrLn $ "Discard Test : " ++ (show (  (diffUTCTime t3 t2)+(diffUTCTime t1 t0)))

          t0 <- getCurrentTime
          t1 <- seq (symv a vecX vecY alpha beta 'l') getCurrentTime
          t2 <- seq (symv a vecX vecY alpha beta 'l') getCurrentTime
          t3 <- seq (symv a vecX vecY alpha beta 'l') getCurrentTime
          putStrLn $ (show  (((diffUTCTime t3 t2) + (diffUTCTime t2 t1) +(diffUTCTime t1 t0)) /r))           
         

