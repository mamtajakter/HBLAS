{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module HBLAS.Level2 where

import qualified Data.Vector.Unboxed as V
import qualified Data.Matrix as M
import HBLAS.Level1
import Prelude hiding (zipWith,(!!), foldl')
import Data.Monoid


{--------------------------------}
{-------GEMV---------------------}
{--------------------------------}
------ commented on Octobre 16th
{-# INLINE gemv #-}
gemv :: (Num n,  V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> n -> n -> V.Vector n
gemv !matA !vecX !vecY !alpha !beta= 
    let !n = V.length vecX
        !x1 = gemvFlatHelper matA (V.replicate n 0) vecX (n-n) n
        !y1= scal beta vecY
    in  axpy alpha x1 y1

{-# INLINE gemvFlat #-}
gemvFlat :: (Num n,  V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n
gemvFlat !matA !vecX = gemvFlatHelper matA (V.replicate n 0) vecX (n-n) n
               where !n= V.length vecX

-- ---- commented on Octobre 16th
{-# INLINE gemvFlatHelper #-}
gemvFlatHelper :: ( Num n,V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> Int->Int-> V.Vector n
gemvFlatHelper !a !x !b !i !n
              | i>=n      =  x
              | otherwise = let !ai= V.take n a
                                !xi = gemvFlatHelper2 ai b
                                !new_x  = V.update x (V.singleton (i, xi))-- new x= [1,2,0]
                                !newa = V.drop n a
                            in gemvFlatHelper newa new_x b (i+1) n



{-# INLINE gemvFlatHelper2 #-}
gemvFlatHelper2 :: ( Num n, V.Unbox n) =>  V.Vector n ->   V.Vector n ->  n
gemvFlatHelper2 !veca !vecx =  V.sum ( V.zipWith (\x y -> x*y) veca vecx)

{--------------------------------}
{-------TRSV---------------------}
{--------------------------------}

trsv :: ( Num n, Fractional n, V.Unbox n) => V.Vector n  -> V.Vector n ->Char -> V.Vector n
trsv !a !b !uplo
          | uplo== 'l'= fst(trsvLower a (V.replicate n 0) b 0 n)
          | otherwise = fst(trsvUpper a (V.replicate n 0) b (n-1) (n-n-1))
        where !n= V.length b

-- commented on Octobre 16th

trsvLower :: ( Num n, Fractional n, V.Unbox n) => V.Vector n  -> V.Vector n -> V.Vector n -> Int->Int-> (V.Vector n, V.Vector n)
trsvLower !a !x !b !i !n
              | i>=n      =  (x,b)
              | otherwise = let !new_bi = trsvHelper (V.slice (i*n) n a)  x (b  V.! i)-- newbi=6
                                !new_b= V.update b (V.singleton (i, new_bi))
                                !aii = ((V.slice (i*n) n a)  V.! i)
                                !new_x  = V.update x (V.singleton (i, (new_bi/aii)))-- new x= [1,2,0]
                            in trsvLower a new_x new_b (i+1) n

-- commented on Octobre 16th
trsvUpper :: ( Num n, Fractional n, V.Unbox n) => V.Vector n  -> V.Vector n -> V.Vector n -> Int->Int-> (V.Vector n, V.Vector n)
trsvUpper !a !x !b !i !n
              |  i<=n = (x,b)
              |  otherwise = let !new_bi = trsvHelper (V.slice (i*n) n a) x (b  V.! i)-- newbi=6
                                 !new_b= V.update b (V.singleton (i, new_bi))
                                 !aii = ((V.slice (i*n) n a)  V.! i)
                                 !new_x  = V.update x (V.singleton (i, (new_bi/aii)))
                             in trsvUpper a new_x new_b (i-1) n

-- commented on Octobre 16th
trsvHelper :: ( Num n, V.Unbox n) =>  V.Vector n ->   V.Vector n->  n ->  n
trsvHelper !veca !vecx !valueb= valueb -  V.sum ( V.zipWith (\x y -> x*y) veca vecx)
{--------------------------------}
{-------TRMV---------------------}
{--------------------------------}
trmv :: ( Num n, Fractional n, V.Unbox n) => V.Vector n  -> V.Vector n ->Char -> V.Vector n
trmv !a !b !uplo
          | uplo== 'l'= trmvLower a (V.replicate n 0) b (n-n) n
          | otherwise = trmvUpper a (V.replicate n 0) b (n-n) n
        where !n= V.length b

-- commented on Octobre 16th
trmvLower :: ( Num n, Fractional n, V.Unbox n) => V.Vector n  -> V.Vector n -> V.Vector n -> Int->Int-> V.Vector n
trmvLower !a !x !b !i !n
              | i>=n      =  x
              | otherwise = let !ai= V.take (i+1) (V.slice (i*n) n a)
                                !bi= V.take (i+1) b
                                !xi = trmvHelper ai bi
                                !new_x  = V.update x (V.singleton (i, xi))-- new x= [1,2,0]
                            in trmvLower a new_x b (i+1) n

-- commented on Octobre 16th
trmvUpper :: ( Num n, Fractional n, V.Unbox n) => V.Vector n  -> V.Vector n -> V.Vector n -> Int->Int-> V.Vector n
trmvUpper !a !x !b !i !n
              | i>=n      =  x
              | otherwise = let !ai= V.drop i (V.slice (i*n) n a)
                                !bi= V.drop i b
                                !xi = trmvHelper ai bi
                                !new_x  = V.update x (V.singleton (i, xi))-- new x= [1,2,0]
                            in trmvUpper a new_x b (i+1) n

-- commented on Octobre 16th
trmvHelper :: ( Num n, V.Unbox n) =>  V.Vector n ->   V.Vector n ->  n
trmvHelper !veca !vecx =  V.sum ( V.zipWith (\x y -> x*y) veca vecx)

{--------------------------------}
{-------SYMV---------------------}
{--------------------------------}
{-# INLINE symv #-}
symv :: (Num n, V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> n -> n -> Char-> V.Vector n
symv !matA !vecX !vecY !alpha !beta !uplo
                          | uplo=='l'= symvLowerH matA vecX vecY alpha beta
                          | otherwise= symvUpperH matA vecX vecY alpha beta

{-# INLINE symvLowerH #-}
symvLowerH :: (Num n,  V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> n -> n -> V.Vector n
symvLowerH !matA !vecX !vecY !alpha !beta= 
    let !n = V.length vecX
        !x1 = symvLowerHFlatHelper matA (V.replicate n 0) vecX (n-n) n
        !y1= scal beta vecY
    in  axpy alpha x1 y1

-- ---- commented on Octobre 16th
{-# INLINE symvLowerHFlatHelper #-}
symvLowerHFlatHelper :: ( Num n,V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> Int->Int-> V.Vector n
symvLowerHFlatHelper !a !x !b !i !n
              | i>=n      =  x
              | otherwise = let !ai= V.take n a
                                !xi = symvFlatHelper2 ai b
                                !new_x  = V.update x (V.singleton (i, xi))-- new x= [1,2,0]
                                !newa = V.drop n a
                            in symvLowerHFlatHelper newa new_x b (i+1) n


{-# INLINE symvUpperH #-}
symvUpperH :: (Num n,  V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> n -> n -> V.Vector n
symvUpperH !matA !vecX !vecY !alpha !beta= 
    let !n = V.length vecX
        !x1 = symvUpperHFlatHelper matA (V.replicate n 0) vecX (n-n) n
        !y1= scal beta vecY
    in  axpy alpha x1 y1

-- ---- commented on Octobre 16th
{-# INLINE symvUpperHFlatHelper #-}
symvUpperHFlatHelper :: ( Num n,V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> Int->Int-> V.Vector n
symvUpperHFlatHelper !a !x !b !i !n
              | i>=n      =  x
              | otherwise = let !ai= V.take n a
                                !xi = symvFlatHelper2 ai b
                                !new_x  = V.update x (V.singleton (i, xi))-- new x= [1,2,0]
                                !newa = V.drop n a
                            in symvUpperHFlatHelper newa new_x b (i+1) n



{-# INLINE symvFlatHelper2 #-}
symvFlatHelper2 :: ( Num n, V.Unbox n) =>  V.Vector n ->   V.Vector n ->  n
symvFlatHelper2 !veca !vecx =  V.sum ( V.zipWith (\x y -> x*y) veca vecx)

-- symv :: (Num n, V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> n -> n -> Char-> V.Vector n
-- symv matA vecX vecY alpha beta uplo = symvLower matA vecX vecY alpha beta

-- -- symv :: (Num n, V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> n -> n -> Char-> V.Vector n
-- -- symv matA vecX vecY alpha beta uplo
-- --                           | uplo=='l'= symvLower matA vecX vecY alpha beta
-- --                           | otherwise= symvUpper matA vecX vecY alpha beta

-- -- commented on Octobre 16th
-- symvLower :: (Num n, V.Unbox n, Fractional n) => V.Vector n -> V.Vector n -> V.Vector n -> n -> n -> V.Vector n
-- symvLower matA vecX vecY alpha beta =
--     let n= V.length vecX
--         mat= getLowerTotalMatrix matA 0 n
--         x1 = gemvFlat mat vecX
--         y1= scal beta vecY
--     in  axpy alpha x1 y1

-- getLowerTotalMatrix :: ( Num n, V.Unbox n) =>V.Vector n-> Int->Int->V.Vector n
-- getLowerTotalMatrix a i n
--             | i>= n= V.empty V.++ V.empty
--             | otherwise =  (getLowerTotalRow x i n)  V.++ getLowerTotalMatrix a (i+1) n
--         where x= V.slice (i*n) (i+1) a -- x = [1] x=[3,2]
--               j=i
-- {-# INLINE getLowerTotalRow #-}
-- getLowerTotalRow :: (Num n, V.Unbox n) => V.Vector n-> Int->Int->V.Vector n
-- getLowerTotalRow !x !i !n
--              | i < n  =  V.snoc x 0
--              | otherwise= V.empty

-- getLowerTotalRow:: ( Num n, V.Unbox n) => V.Vector n-> V.Vector n -> Int ->Int->  Int-> V.Vector n
-- getLowerTotalRow a x i j n
--            | i>= n = x
--            | otherwise= let aij= ((V.slice (i*n) (i+1) a)  V.! j)
--                             x1= V.take i x
--                             new_x=  V.snoc x1 aij
--                          in getLowerTotalRow a new_x (i+1) j n



{-






{----------------FINISH-----------------------------------------}

syr2 :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> Char-> V.Vector (V.Vector n)
syr2 matA vecX vecY alpha uplo
                          | uplo=='l'= V.init (syr2Lower1 matA vecX vecY alpha)
                          | otherwise= V.init (syr2Upper1 matA vecX vecY alpha)

-- commented on Octobre 16th
syr2Upper1 :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
syr2Upper1 matA vecX vecY alpha=
    let mat= syr2Upper2 matA vecX vecY alpha
        x1 = scal alpha vecY
        y1= getXYTranspose x1 vecX
    in V.zipWith (V.zipWith (+)) mat y1

-- commented on Octobre 16th
syr2Upper2 :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
syr2Upper2 matA vecX vecY alpha=
    let n= V.length vecX
        mat= getUpperTotalMatrix matA 0 n
        x1 = scal alpha vecX
        y1= getXYTranspose x1 vecY
    in V.zipWith (V.zipWith (+)) mat y1

-- commented on Octobre 16th
syr2Lower1 :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
syr2Lower1 matA vecX vecY alpha=
    let mat= syr2Lower2 matA vecX vecY alpha
        x1 = scal alpha vecY
        y1= getXYTranspose x1 vecX
    in V.zipWith (V.zipWith (+)) mat y1

-- commented on Octobre 16th
syr2Lower2 :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
syr2Lower2 matA vecX vecY alpha=
    let n= V.length vecX
        mat= getLowerTotalMatrix matA 0 n
        x1 = scal alpha vecX
        y1=  getXYTranspose x1 vecY
    in V.zipWith (V.zipWith (+)) mat y1

syr :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> n -> Char-> V.Vector (V.Vector n)
syr matA vecX alpha uplo
                          | uplo=='l'= V.init (syrLower matA vecX alpha)
                          | otherwise= V.init (syrUpper matA vecX alpha)


-- commented on Octobre 16th
syrUpper :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> n -> V.Vector (V.Vector n)
syrUpper matA vecX alpha=
    let n= V.length vecX
        mat= getUpperTotalMatrix matA 0 n
        x1 = scal alpha vecX
        y1= getXYTranspose x1 vecX
    in V.zipWith (V.zipWith (+)) mat y1

syrLower :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> n -> V.Vector (V.Vector n)
syrLower matA vecX alpha=
    let n= V.length vecX
        mat= getLowerTotalMatrix matA 0 n
        x1 = scal alpha vecX
        y1= getXYTranspose x1 vecX
    in V.zipWith (V.zipWith (+)) mat y1

-- commented on Octobre 16th
ger :: (Num n, V.Unbox n) => V.Vector (V.Vector n) -> V.Vector n -> V.Vector n -> n -> V.Vector (V.Vector n)
ger matA vecX vecY alpha=
    let x1 = scal alpha vecX
        y1= getXYTranspose x1 vecY
    in V.zipWith (V.zipWith (+)) matA y1

--getXYTranspose gets vector x and vector y and returns the matrix= x * (y transpose)

-- commented on Octobre 16th
getXYTranspose :: ( Num n, V.Unbox n) => V.Vector n ->V.Vector n -> (V.Vector (V.Vector n))
getXYTranspose x y
              | null x =  V.empty <> V.empty
              | otherwise =  pure (scal (V.head x) y) <> getXYTranspose (V.tail x) y

-}              