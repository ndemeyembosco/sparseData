{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark.SparseBlas.Parallel.PBenchMark where 

import Criterion.Main 
import Criterion.Types 
import BenchMark.SparseBlas.Parallel.PKernels
-- import Util.DataLoaderSmall
import qualified Data.Vector as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser ( MMExchange, mm_to_s_data_p ) 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic
    ( Sparse(SparseData), RepIndex(U) ) 
import SparseBlas.Data.Matrix.Parallel.Sparse.COO ( COO ) 
import BenchMark.SparseBlas.Parallel.Sparse.Big.COO
    ( bench_coo_big ) 
import BenchMark.SparseBlas.Parallel.Sparse.Big.CSR
    ( bench_csr_big ) 
import BenchMark.SparseBlas.Parallel.Sparse.Big.ELL
    ( bench_ell_big ) 
import BenchMark.SparseBlas.Parallel.Sparse.Big.CSC
    ( bench_csc_big )
import BenchMark.SparseBlas.Parallel.Dense.Big.DENSE 
    (bench_dns_big)
import Data.Maybe
import System.Random.PCG
import Control.DeepSeq (deepseq)
import Control.Monad.ST 


-- genRandMatrixPCG :: Variate a => Int -> Int -> SparseData DNS U a 
-- genRandMatrixPCG width height = runST $ do 
--     gen    <- create 
--     to_ret <- mapM (\_ -> uniform gen >>= \a -> return a) $ U.enumFromN 0 (width * height)
--     return $ DNS to_ret width height 


-- genRandMatricesPCG :: Variate a => [(Int, Int)] -> [SparseData DNS U a] 
-- genRandMatricesPCG = Prelude.map (\(w, h) -> genRandMatrixPCG w h) 


sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data_p dict  

config :: Config
config = defaultConfig 




hs :: IO ()
hs = main 

main :: IO ()
main = do
      print "starting dense benchmark"
      bench_dns_big  
    --   bench_csr_big  
    --   bench_ell_big 
    --   bench_csc_big  
    --   let (v1, v2, v3, v4, v5) = deepseq ans ans 
    --   print $ U.length $ v1
    --   print $ U.length $ v2
    --   print $ U.length $ v3
    --   print $ U.length $ v4
    --   print $ U.length $ v5  
      return ()
