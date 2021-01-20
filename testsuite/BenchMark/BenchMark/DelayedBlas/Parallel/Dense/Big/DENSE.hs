{-# LANGUAGE DataKinds, RankNTypes, BangPatterns, ScopedTypeVariables, FlexibleContexts, KindSignatures #-}

module BenchMark.DelayedBlas.Parallel.Dense.Big.DENSE where 

import qualified Data.Vector.Unboxed as UNB 
import BenchMark.DelayedBlas.Parallel.PKernels ( gemv )
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Matrix(MatrixData), RepIndex(U), toVector, fromVector ) 
import DelayedBlas.Data.Matrix.Parallel.Dense.DENSE
    (  MatrixData(DNS), DNS ) 
import Control.DeepSeq (deepseq)
import System.Environment (getArgs)
import GHC.TypeLits ( KnownNat ) 
import Data.Proxy () 
import Control.Monad.ST ( runST ) 
import Data.Maybe () 
import System.Random.PCG ( create, Variate(uniformR) ) 


genRandMatrixPCG ::  Int -> Int -> (forall n1 n2. (KnownNat n1, KnownNat n2) => MatrixData DNS U n1 n2 Double)  
genRandMatrixPCG width height  = let to_ret = UNB.generate (width * height) (\_ -> 12.3245) in (DNS to_ret) :: MatrixData DNS U widthT heightT Double

sum_mat :: MatrixData DNS U n1 n2 Double -> Double
sum_mat (DNS m) = UNB.sum m 



bench_dns_big :: IO () 
bench_dns_big = do
   (dimension': _) <- getArgs 
   let 
       !dimension = (read dimension') :: Int 
       !m         = (genRandMatrixPCG dimension dimension :: MatrixData DNS U 20000 20000 Double)
       !v1        = UNB.replicate dimension 3.4568 :: UNB.Vector Double
       !v2        = UNB.replicate dimension 1.2345  :: UNB.Vector Double 
       !v_func1   = fromVector v1
       !v_func2   = fromVector v2 
       !alpha     = 1.0 
       !beta      = 1.0 
       {-# SCC ans_vec "force_gemv_computation" #-} 
       !ans_vec = toVector $ gemv alpha beta m v_func1 v_func2
   print $ "sum after gemv: " ++ (show $ UNB.sum ans_vec) ++ "\n"
   print $ "sum v1 after gemv: " ++ (show $ UNB.sum v1) ++ "\n"
   print $ "sum v2 after gemv: " ++ (show $ UNB.sum v2) ++ "\n"
   print $ "sum m after gemv: " ++ (show $ sum_mat m) ++ "\n"
   return ()


