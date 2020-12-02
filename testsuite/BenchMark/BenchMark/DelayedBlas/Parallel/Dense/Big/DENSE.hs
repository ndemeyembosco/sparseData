{-# LANGUAGE DataKinds, RankNTypes, BangPatterns, ScopedTypeVariables, FlexibleContexts, KindSignatures #-}

module BenchMark.DelayedBlas.Parallel.Dense.Big.DENSE where 

import Criterion.Main 
import Criterion.Types 
import Criterion.Measurement (initializeTime, getTime, getCPUTime )
import BenchMark.DelayedBlas.Parallel.PKernels
-- import qualified Kernels as K 
import Util.DataLoader
import qualified Data.Vector as U 
import qualified Data.Vector.Unboxed as UNB 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic 
import DelayedBlas.Data.Matrix.Parallel.Sparse.COO 
import DelayedBlas.Data.Matrix.Parallel.Dense.DENSE 
import DelayedBlas.Data.Matrix.Parallel.Sparse.CSR 
import DelayedBlas.Data.Matrix.Parallel.Sparse.ELL 
import DelayedBlas.Data.Matrix.Parallel.Sparse.CSC 
-- import qualified DelayedBlas.Data.Matrix.Dense.DENSE as D 
-- import qualified DelayedBlas.Data.Matrix.Generic.Generic as G 
import Data.Maybe
-- import System.Random 
import System.Random.PCG 
import Control.DeepSeq (deepseq, force)
import Control.Parallel (pseq)
import Control.Exception 
import Control.Monad.ST 
import System.Environment (getArgs)
import GHC.TypeLits 
import Data.Proxy 


genRandMatrixPCG ::  Int -> Int -> Double -> Double -> (forall n1 n2. (KnownNat n1, KnownNat n2) => MatrixData DNS U n1 n2 Double)  
genRandMatrixPCG width height start end = runST $ do 
    let 
        widthT  = fromJust $ someNatVal $ toInteger width 
        heightT = fromJust $ someNatVal $ toInteger height 
    gen    <- create 
    to_ret <- UNB.mapM (\(t :: Int) -> uniformR (start :: Double, end :: Double) gen >>= \a -> return (a :: Double)) $ UNB.enumFromN 0 (width * height)
    return $ (DNS to_ret :: MatrixData DNS U widthT heightT Double) 


-- genRandMatricesPCG :: Variate a => [Int] -> [MatrixData DNS U a] 
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
       m  = (genRandMatrixPCG dimension dimension (1.0 :: Double) (fromIntegral $ max_rand) :: MatrixData DNS U 10000 10000 Double)
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


