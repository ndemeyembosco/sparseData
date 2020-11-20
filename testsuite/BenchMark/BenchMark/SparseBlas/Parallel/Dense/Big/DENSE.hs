{-# LANGUAGE DataKinds, BangPatterns, ScopedTypeVariables, FlexibleContexts #-}

module BenchMark.SparseBlas.Parallel.Dense.Big.DENSE where 

import Criterion.Main 
import Criterion.Types 
import Criterion.Measurement (initializeTime, getTime, getCPUTime )
import BenchMark.SparseBlas.Parallel.PKernels
-- import qualified Kernels as K 
import Util.DataLoader
import qualified Data.Vector as U 
import qualified Data.Vector.Unboxed as UNB 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic 
import SparseBlas.Data.Matrix.Parallel.Sparse.COO 
import SparseBlas.Data.Matrix.Parallel.Dense.DENSE 
import SparseBlas.Data.Matrix.Parallel.Sparse.CSR 
import SparseBlas.Data.Matrix.Parallel.Sparse.ELL 
import SparseBlas.Data.Matrix.Parallel.Sparse.CSC 
-- import qualified SparseBlas.Data.Matrix.Dense.DENSE as D 
-- import qualified SparseBlas.Data.Matrix.Generic.Generic as G 
import Data.Maybe
-- import System.Random 
import System.Random.PCG 
import Control.DeepSeq (deepseq, force)
import Control.Parallel (pseq)
import Control.Exception 
import Control.Monad.ST 
import System.Environment (getArgs)


genRandMatrixPCG :: Int -> Int -> Double -> Double -> SparseData DNS U Double  
genRandMatrixPCG width height start end = runST $ do 
    gen    <- create 
    to_ret <- UNB.mapM (\(t :: Int) -> uniformR (start :: Double, end :: Double) gen >>= \a -> return (a :: Double)) $ UNB.enumFromN 0 (width * height)
    return $ DNS to_ret width height 


-- genRandMatricesPCG :: Variate a => [Int] -> [SparseData DNS U a] 
-- genRandMatricesPCG = Prelude.map (\w -> genRandMatrixPCG w w) 


config :: Config 
config = defaultConfig 
    -- Config {
    --     timeLimit = 200.0
    -- }



bench_dns_big :: IO () 
bench_dns_big = do 
   (dimension' : max_rand' : _) <- getArgs 
   let 
       dimension = (read dimension') :: Int 
       max_rand  = (read max_rand')  :: Int 
   initializeTime 
   print "timer initialized!"
   genTimeStart <- getCPUTime
   print ("generation start time: " ++ show genTimeStart)

   let  
       (m :: SparseData DNS U Double) = genRandMatrixPCG dimension dimension (1.0 :: Double) (fromIntegral $ max_rand)
       (v1 :: UNB.Vector Double) = UNB.replicate dimension 1.0 
       (v2 :: UNB.Vector Double) = UNB.replicate dimension 1.0   
   m `deepseq` v1 `deepseq` v2 `deepseq` genTimeStart `seq` return ()
   genTimeEnd  <- getCPUTime
   let diff1 = genTimeEnd - genTimeStart 

   genTimeEnd `seq` diff1 `seq` return () 
   print ("total random generation time: " ++ show diff1 ++ "\n")

   let 
       v_func1  = from_vector v1
       v_func2  = from_vector v2 
   print "doing bench dns!"

   


   startTime <- getCPUTime
   v_func1 `seq` v_func2 `seq` return ()
   let !ans_vec = to_vector $ gemv 1.0 1.0 m v_func1 v_func2

   startTime `seq` ans_vec `deepseq` return () 
   endTime   <- getCPUTime
   let diff = endTime - startTime  

   endTime `seq` diff `seq` return ()
   print ("total time: " ++ show diff ++ "\n")


