{-# LANGUAGE BangPatterns #-}
module HBLAS.IO.List where

import Data.List as V
import Data.Monoid

-- vecFromFile :: (Floating n, Read n) => FilePath -> IO (Vector n)
-- vecFromFile f = fmap read . fromList . lines <$> readFile f
--
-- matrixVecFromFile :: (Floating n, Read n) => FilePath -> IO (Vector (Vector n))
-- matrixVecFromFile f =   fromList
--                  .   fmap (fromList . read . (<>"]") . ("["<>)) . lines
--                  <$> readFile f
{-
createb :: (Num n) =>  Vector n
createb = pure (fromIntegral ( 1) ) <> pure (fromIntegral (2) ) <> empty

createx :: (Num n) =>  Vector n
createx = pure (fromIntegral (0) ) <> pure (fromIntegral (0) ) <> empty

createA1 :: (Num n) =>  Vector n
createA1 = pure (fromIntegral (1) ) <> pure (fromIntegral (2) ) <> empty

createA2 :: (Num n) =>  Vector n
createA2 = pure (fromIntegral (2) ) <> pure (fromIntegral (3) ) <> empty

createA :: (Num n) =>  Vector (Vector n)
createA = pure (createA1 ) <> pure (createA2 ) <> empty
-}
-- ---- commented on Octobre 16th
{-# INLINE ccreateVecX' #-}
ccreateVecX' :: (Num n) => Int ->Int-> [n]
ccreateVecX' !i !n
             | i<n  =  1 : (ccreateVecX' (i+1) n)
             | otherwise= []

{-# INLINE createVecX #-}
createVecX :: (Num n) => Int->Int ->Int-> [n]
createVecX !i !j !n
             | i<n  =  1 : (createVecX (i+1) (j+1) n)
             | otherwise= []



{-# INLINE createVecX' #-}
createVecX' :: (Num n) => Int-> [n]
createVecX' !n =V.replicate n 1

{-# INLINE createVec0 #-}
createVec0 :: (Num n) => Int-> [n]
createVec0 !n =V.replicate n 0

{-# INLINE createUnSymSq #-}
createUnSymSq :: (Num n) => Int->Int  -> [[n]]
createUnSymSq !i !n
             | i<n  =   (createVecX 0 1 n) : (createUnSymSq (i+1) n)
             | otherwise = []

---- commented on Octobre 16th
{-# INLINE createSymSq #-}
createSymSq :: (Num n) => Int->Int  -> [[n]]
createSymSq !i !n
             | i<n  =   (createVecX 0 1 n) : (createSymSq (i+1) n)
             | otherwise = []

{-# INLINE createFlatMatrix #-}
createFlatMatrix :: (Num n) => Int->Int  -> [n]
createFlatMatrix !i !n
             | i<n  =  createVecX 0 1 n ++ (createFlatMatrix (i+1) n)
             | otherwise = []
--


-- commented on Octobre 16th
{-# INLINE createSymLowSq #-}
createSymLowSq :: (Num n) => Int->Int  -> [[n]]
createSymLowSq !i !n
             | i<n  =   (helperSymLowSq 0 1 i n) : (createSymLowSq (i+1) n)
             | otherwise = []

---- commented on Octobre 16th
{-# INLINE helperSymLowSq #-}
helperSymLowSq :: (Num n) => Int->Int ->Int->Int-> [n]
helperSymLowSq !i !j !k !n
             | i<=k  =  1 : (helperSymLowSq (i+1) (j+1) k n)
             | i<n  =  0 : (helperSymLowSq (i+1) (j+1) k n)
             | otherwise= []

-- commented on Octobre 16th
{-# INLINE helperSymUpSq #-}
helperSymUpSq :: (Num n) => Int->Int ->Int->Int-> [n]
helperSymUpSq !i !j !k !n
             | i<k  =  (0) : (helperSymUpSq (i+1) (j+1) k n)
             | i<n  =  (1) : (helperSymUpSq (i+1) (j+1) k n)
             | otherwise= []

-- commented on Octobre 16th
{-# INLINE createSymUpSq #-}
createSymUpSq :: (Num n) => Int->Int  -> [[n]]
createSymUpSq !i !n
             | i<n  =   (helperSymUpSq 0 1 i n) : (createSymUpSq (i+1) n)
             | otherwise = []
-----------------------------
-- commented on Octobre 16th
{-# INLINE createSymLowTri #-}
createSymLowTri :: (Num n) => Int->Int  -> [[n]]
createSymLowTri !i !n
             | i<n  =   (helperSymLowTri 0 1 i n) : (createSymLowTri (i+1) n)
             | otherwise = []

---- commented on Octobre 16th
{-# INLINE helperSymLowTri #-}
helperSymLowTri :: (Num n) => Int->Int ->Int->Int-> [n]
helperSymLowTri !i !j !k !n
             | i<=k  =  (1) : (helperSymLowTri (i+1) (j+1) k n)
             | i<n  =  (0) : (helperSymLowTri (i+1) (j+1) k n)
             | otherwise= []
------------------------------

-- commented on Octobre 16th
{-# INLINE helperSymUpTri #-}
helperSymUpTri :: (Num n) => Int->Int ->Int->Int-> [n]
helperSymUpTri !i !j !k !n
             | i<k     =  (0 ) : (helperSymUpTri (i+1) (j+1) k n)
             | i<n  =  (1 ) : (helperSymUpTri (i+1) (j+1) k n)
             | otherwise= []

-- commented on Octobre 16th
{-# INLINE createSymUpTri #-}
createSymUpTri :: (Num n) => Int->Int  -> [[n]]
createSymUpTri !i !n
             |i<n  =   (V.drop i (helperSymUpTri 0 1 i n)) : (createSymUpTri (i+1) n)
             | otherwise = []

{-

update :: ( Num n)=> [n]-> Int-> n-> [n]
update (x:xs) index value
       | index>0       = x : (update xs (index-1) value)
       | otherwise = value : xs

-}


-- createVecX :: (Num n) => Int ->Int-> [n]
-- createVecX !i !n
--              | i<n  =  1 : (createVecX (i+1) n)
--              | otherwise= []

-- createVecX' :: (Num n) => Int-> [n]
-- createVecX' !n =V.replicate n 1
