{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}

module HBLAS.Level2 where

import qualified Data.List as V
-- import qualified Data.Matrix as M
import HBLAS.Level1
import Prelude hiding (zipWith,(!!), foldl')
import Data.Monoid
---- commented on Octobre 16th
{-# INLINE gemv' #-}
gemv' :: (Num n) => [[n]] -> [n] -> [n]
gemv' !matA !vecX = fmap (dot vecX) matA
------ commented on Octobre 16th
{-# INLINE gemv #-}
gemv :: (Num n) => [[n]] -> [n] -> [n] -> n -> n -> [n]
gemv !matA !vecX !vecY !alpha !beta=
    let !x1 = fmap (dot vecX) matA
        !y1= scal beta vecY
    in  axpy alpha x1 y1

{-# INLINE trmv #-}
trmv :: ( Num n, Fractional n) => [[n]]  -> [n] ->Char -> [n]
trmv !a !b !uplo
          | uplo== 'l'= trmvLower a (V.replicate n 0) b (n-n) n
          | otherwise = trmvUpper a (V.replicate n 0) b (n-n) n
        where !n= V.length b

-- commented on Octobre 16th
{-# INLINE trmvLower #-}
trmvLower :: ( Num n, Fractional n) => [[n]]  -> [n] -> [n] -> Int->Int-> [n]
trmvLower !a !x !b !i !n
              | i>=n      =  x
              | otherwise = let !ai= V.take (i+1) (a  V.!! i)
                                !bi= V.take (i+1) b
                                !xi = trmvHelper ai bi
                                !new_x  = update x i xi-- new x= [1,2,0]
                            in trmvLower a new_x b (i+1) n

-- commented on Octobre 16th
{-# INLINE trmvUpper #-}
trmvUpper :: ( Num n, Fractional n) => [[n]]  -> [n] -> [n] -> Int->Int-> [n]
trmvUpper !a !x !b !i !n
              | i>=n      =  x
              | otherwise = let !ai= V.drop i (a  V.!! i)
                                !bi= V.drop i b
                                !xi = trmvHelper ai bi
                                !new_x  = update x i xi-- new x= [1,2,0]
                            in trmvUpper a new_x b (i+1) n

-- commented on Octobre 16th
{-# INLINE trmvHelper #-}
trmvHelper :: ( Num n) =>  [n] ->   [n] ->  n
trmvHelper !veca !vecx =  sum ( V.zipWith (\x y -> x*y) veca vecx)


{-# INLINE trsv #-}
trsv :: ( Num n, Fractional n) => [[n]]  -> [n] ->Char -> [n]
trsv !a !b !uplo
          | uplo== 'l'= fst(trsvLower a (V.replicate n 0) b (n-n) n)
          | otherwise = fst(trsvUpper a (V.replicate n 0) b (n-1) (n-n-1))
        where !n= V.length b

-- commented on Octobre 16th
{-# INLINE trsvLower #-}
trsvLower :: ( Num n, Fractional n) => [[n]]  -> [n] -> [n] -> Int->Int-> ([n], [n])
trsvLower !a !x !b !i !n
              | i>=n      =  (x,b)
              | otherwise = let !new_bi = trsvHelper (a  V.!! i) x (b  V.!! i)-- newbi=6
                                !new_b= update b i new_bi
                                !aii = ((a  V.!! i)  V.!! i)
                                !new_x  = update x i (new_bi/aii)-- new x= [1,2,0]
                            in trsvLower a new_x new_b (i+1) n

-- commented on Octobre 16th
{-# INLINE trsvUpper #-}
trsvUpper :: ( Num n, Fractional n) => [[n]] -> [n] -> [n] -> Int->Int-> ([n], [n])
trsvUpper !a !x !b !i !n
              |  i<=n = (x,b)
              |  otherwise = let !new_bi = trsvHelper (a  V.!! i) x (b  V.!! i)-- newbi=6
                                 !new_b= update b i new_bi
                                 !aii = ((a  V.!! i)  V.!! i)
                                 !new_x  = update x i (new_bi/aii)
                             in trsvUpper a new_x new_b (i-1) n

-- commented on Octobre 16th
{-# INLINE trsvHelper #-}
trsvHelper :: ( Num n) =>  [n] ->   [n]->  n ->  n
trsvHelper !veca !vecx !valueb= valueb -  sum ( V.zipWith (\x y -> x*y) veca vecx)

{-# INLINE symv #-}
symv :: (Num n) => [[n]] -> [n] -> [n] -> n -> n -> Char-> [n]
symv !matA !vecX !vecY !alpha !beta !uplo
                          | uplo=='l'= symvLower matA vecX vecY alpha beta
                          | otherwise= symvUpper matA vecX vecY alpha beta

-- commented on Octobre 16th
{-# INLINE symvLower #-}
symvLower :: (Num n) => [[n]] -> [n] -> [n] -> n -> n -> [n]
symvLower !matA !vecX !vecY !alpha !beta =
    let !n= V.length vecX
        !mat= getLowerTotalMatrix matA 0 n
        !x1 = fmap (dot vecX) mat
        !y1= scal beta vecY
    in  axpy alpha x1 y1

-- commented on Octobre 16th
{-# INLINE symvUpper #-}
symvUpper :: (Num n) => [[n]] -> [n] -> [n] -> n -> n -> [n]
symvUpper !matA !vecX !vecY !alpha !beta =
    let !n= V.length vecX
        !mat= getUpperTotalMatrix matA 0 n
        !x1 = fmap (dot vecX) mat
        !y1= scal beta vecY
    in  axpy alpha x1 y1

-- commented on Octobre 16th
{-# INLINE getLowerTotalMatrix #-}
getLowerTotalMatrix :: ( Num n) =>[[n]]-> Int->Int->[[n]]
getLowerTotalMatrix !a !i !n
            | i>= n= [[]]
            | otherwise = (getLowerTotalRow a x i j n)  : (getLowerTotalMatrix a (i+1) n)
        where !x= a V.!! i
              !j=i


-- commented on Octobre 16th
-- x = a V.!! i
{-# INLINE getLowerTotalRow #-}
getLowerTotalRow:: ( Num n) =>  [[n]]-> [n] -> Int ->Int->  Int-> [n]
getLowerTotalRow !a !x !i !j !n
           | i>= n = x
           | otherwise= let !aij= ((a  V.!! i)  V.!! j)
                            !x1= V.take i x
                            !new_x=  x1 ++ [aij]
                         in getLowerTotalRow a new_x (i+1) j n

-- commented on Octobre 16th
{-# INLINE getUpperTotalMatrix #-}
getUpperTotalMatrix :: ( Num n) =>[[n]]-> Int->Int->[[n]]
getUpperTotalMatrix !a !i !n
            | i>= n= [[]]
            | otherwise =  (getUpperTotalRow a x i 0 (n-n))  : (getUpperTotalMatrix a (i+1) n)
        where !x= a V.!! i

--n=0
--j=0

-- commented on Octobre 16th
{-# INLINE getUpperTotalRow #-}
getUpperTotalRow:: ( Num n) =>  [[n]]->[n]-> Int ->Int->Int-> [n]
getUpperTotalRow !a !x !i !j !n
           | i< n = x
           | j==0  = getUpperTotalRow a x (i-1) (j+1) n
           | otherwise= let !aij= ((a  V.!! i)  V.!! j)
                            !new_x=   aij : x
                         in getUpperTotalRow a new_x (i-1) (j+1) n


{-# INLINE update #-}
update :: ( Num n)=> [n]-> Int-> n-> [n]
update !(x:xs) !index !value
       | index>0       = x : (update xs (index-1) value)
       | otherwise = value : xs