{-# LANGUAGE DataKinds, RankNTypes, BangPatterns, ScopedTypeVariables, FlexibleContexts, KindSignatures #-}

module BenchMark.DelayedBlas.Parallel.Dense.Big.DENSE where 

import qualified Data.Vector.Unboxed as UNB 
import BenchMark.DelayedBlas.Parallel.PKernels
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic 
import DelayedBlas.Data.Matrix.Parallel.Dense.DENSE 
import Control.DeepSeq (deepseq)
import System.Environment (getArgs)
import GHC.TypeLits 
import Data.Proxy 
import Control.Monad.ST 
import Data.Maybe 
import System.Random.PCG 


genRandMatrixPCG ::  Int -> Int -> Double -> Double -> (forall n1 n2. (KnownNat n1, KnownNat n2) => MatrixData DNS U n1 n2 Double)  
genRandMatrixPCG width height start end = runST $ do 
    let 
        widthT  = fromJust $ someNatVal $ toInteger width 
        heightT = fromJust $ someNatVal $ toInteger height 
    gen    <- create 
    to_ret <- UNB.mapM (\(t :: Int) -> uniformR (start :: Double, end :: Double) gen >>= \a -> return (a :: Double)) $ UNB.enumFromN 0 (width * height)
    return $ (DNS to_ret :: MatrixData DNS U widthT heightT Double) 



bench_dns_big :: IO () 
bench_dns_big = do
   (dimension' : max_rand' : _) <- getArgs 
   let 
       !dimension = (read dimension') :: Int 
       !max_rand  = (read max_rand')  :: Int  
       !m         = (genRandMatrixPCG dimension dimension (1.0 :: Double) (fromIntegral $ max_rand) :: MatrixData DNS U 10000 10000 Double)
       !v1        = UNB.replicate dimension 1.0 :: UNB.Vector Double
       !v2        = UNB.replicate dimension 1.0  :: UNB.Vector Double 
       !v_func1   = fromVector v1
       !v_func2   = fromVector v2 
       {-# SCC ans_vec "force_gemv_computation" #-} 
       !ans_vec = toVector $ gemv 1.0 1.0 m v_func1 v_func2
   return $ ans_vec `deepseq` ()


