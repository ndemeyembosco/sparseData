{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language BangPatterns #-}

import Prelude 
-- import Data.Vector hiding (break)
import qualified Data.Vector as U 
-- import  qualified Data.Vector.Mutable as V   
import System.Environment (getArgs)
import Control.Monad 
import Data.Sparse.SpMatrix
import Data.Sparse.SpVector
import Data.Sparse.Common 
import Numeric.LinearAlgebra.Class
import qualified System.Random.PCG.Fast.Pure as SR 
import Data.Maybe 
import System.CPUTime 
import Text.Printf 



data ProblemSize = ProblemSize {
   n :: !Int 
 , niter    :: !Int 
 , non_zero :: !Int 
 , lam      :: !Double  
}


genRandomVals_d :: Int -> Double -> Double -> SR.GenIO -> IO (U.Vector Double)
{-# INLINE genRandomVals_d #-}
genRandomVals_d !n !lo !hi !gen = U.replicateM n $ SR.uniformR (lo, hi) gen 

genRandomVals_i :: Int -> Int -> Int -> SR.GenIO -> IO (U.Vector Int)
{-# INLINE genRandomVals_i #-}
genRandomVals_i !n !lo !hi !gen = U.replicateM n $ SR.uniformR (lo, hi) gen



class_a, class_b, class_s :: ProblemSize
class_s = ProblemSize {n=1400, niter=15, non_zero=7, lam=10}
class_a = ProblemSize {n=14000, niter=15, non_zero=11, lam=20}
class_b = ProblemSize {n=75000, niter=75, non_zero=13, lam=60}


getClass :: Char -> Either String ProblemSize
getClass c = case c of 
  'A'  -> Right class_a
  'B'  -> Right class_b
  'S'  -> Right class_s
  _   -> Left "undefined problem size"


solve_sys :: Int -> SpMatrix Double ->  SpVector Double -> (Double, SpVector Double)
{-# INLINE solve_sys #-}
solve_sys !size = solve_sys' size 25 (zerosSV size)
    

solve_sys' :: Int -> Int -> SpVector Double -> SpMatrix Double ->  SpVector Double -> (Double, SpVector Double)
{-# INLINE solve_sys' #-}
solve_sys' !size !iters !z !a !x = 
    let 
        !r     = x 
        !p     = r  
        !rho   = r <.> r 
    in go a z x rho p r 0 
    where
        {-# INLINE go #-}
        go !a !z !x !d !p !r !i | i == iters     =  
                                    let 
                                        !residual = (a #> z) ^-^ x 
                                        !to_ret = sqrt $ residual <.> residual 
                                    in (to_ret, z) 
                                | otherwise =  
                                    let 
                                        !q         = a #> p 
                                        !alpha     = d / (p <.> q) 
                                        !new_z     = z ^+^ (alpha .* p) 
                                        !new_r     = r ^-^ (alpha .* q)  
                                        !new_d     = new_r <.> new_r 
                                        !beta      = new_d / d  
                                        !new_p     = new_r ^+^ (beta .* p) 
                                    in go a new_z x new_d new_p new_r (i + 1)



make_matrix ::  Int -> Int -> IO (SpMatrix Double)
make_matrix n nzs = do 
                    let !seed = 314159265
                    my_gen    <- SR.initialize seed
                    let !ratio = (0.1 / 1.0)**(1.0/fromIntegral n) :: Double
                    -- generate and place seven random numbers in (0, 1)
                    -- calculate the matrix as the sum of n outer products of 
                    -- the generated vector with itself multiplied by omega values (generated in a geometric sequence
                    -- whose first element is 1 and nth element is 0.1)
                    -- make sure to make the ith element of the vector 0.5
                    -- finally, add 0.1 to the diagonal of the created matrix.
                    !matA <- (flip.flip foldM) (zeroSM n n) [0..n-1] $ \m ind -> do 
                                let 
                                    !mat = m 
                                    !i   = ind 
                                !x_vec_locs <- genRandomVals_i nzs 0 n my_gen
                                !x_vec_vals <- genRandomVals_d nzs 0 1 my_gen
                                let 
                                    !x_vec_zip = (U.zip x_vec_locs x_vec_vals) 
                                    !x_vec_sp  = insertSpVector i 0.5 $ fromListSV n x_vec_zip
                                    !to_add    = x_vec_sp >< x_vec_sp   -- outer product 
                                    !to_ret    = (mat ^+^ ((ratio**(fromIntegral i)) .* to_add))
                                return to_ret 
                    !matL <- forM [0..(n - 1)] $ \ind -> do 
                                let 
                                    !i = ind 
                                    !e = matA @@! (i, i) 
                                    !to_ret = insertSpMatrix i i (e + 0.1) matA
                                if i == (n - 1) then 
                                    return $ Just to_ret
                                else 
                                    return $ Nothing
                    return $ fromJust $! last matL

                    
main :: IO ()
main = do 
    (p:_) <- getArgs
    case getClass $ head p of 
        Left err   -> print err 
        Right (ProblemSize {n=size, niter=iters, non_zero=n_zs, lam=shift}) -> do 
            start_i <- getCPUTime 
            !a <- make_matrix size n_zs 
            end_i  <- getCPUTime
            print $ nzSM a 
            let x = onesSV size  
            print $ nzSV x 
            printf "\n\n\n"
            printf "NAS Parallel Benchmarks - CG Benchmark \n"
            printf "Size: %d\n" size
            printf "Iterations: %d\n\n" iters 
            printf "Initialization time: %0.9f\n\n" ((fromIntegral (end_i - start_i)) / (10^12) :: Double)
            printf "iteration\t\t||r||\t\tzeta\n"
            start_i2 <- getCPUTime
            go size shift iters 1 x a
            end_e <- getCPUTime
            printf "Overall time: %0.9f\n\n" ((fromIntegral (end_e - start_i2)) / 10^12 :: Double) 
    where 
        go :: Int -> Double -> Int -> Int -> SpVector Double -> SpMatrix Double -> IO ()
        {-# INLINE go#-}
        go !size !shift !nit !n !x !a | n == (nit + 1)   = return ()
                                      | otherwise        = do
                                                let 
                                                    (!r_norm, !new_z) = solve_sys size a x 
                                                    !zeta = shift + (1 / (x <.> new_z)) 
                                                printf "%d\t\t%0.9f\t\t%0.9f\n" n r_norm zeta 
                                                let 
                                                    !new_x =  normalize2 new_z  
                                                go size shift nit (n + 1) new_x a  
                
           
