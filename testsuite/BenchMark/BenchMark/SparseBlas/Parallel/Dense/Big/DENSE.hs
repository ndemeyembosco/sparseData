{-# LANGUAGE DataKinds, BangPatterns, ScopedTypeVariables #-}

module BenchMark.SparseBlas.Parallel.Dense.Big.DENSE where 

import Criterion.Main 
import Criterion.Types 
import BenchMark.SparseBlas.Parallel.PKernels
import Util.DataLoader
import qualified Data.Vector as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic 
import SparseBlas.Data.Matrix.Parallel.Sparse.COO 
import SparseBlas.Data.Matrix.Parallel.Dense.DENSE
import SparseBlas.Data.Matrix.Parallel.Sparse.CSR 
import SparseBlas.Data.Matrix.Parallel.Sparse.ELL 
import SparseBlas.Data.Matrix.Parallel.Sparse.CSC 
import Data.Maybe
import System.Random 
import Control.DeepSeq (deepseq)



genRandMatrix :: Random a => Int -> Int -> IO (SparseData DNS U a)
genRandMatrix width height = do 
    stdGen <- getStdGen 
    let rs = (U.fromList $ take (width * height) $ randoms stdGen)
    return $ DNS rs width height  

genRandMatrices :: Random a => [(Int, Int)] -> IO [(SparseData DNS U a)]
genRandMatrices = mapM (\(x, y) -> genRandMatrix x y) 





bench_dns_big :: IO (U.Vector Double)
bench_dns_big = do
    (mdata :: [SparseData DNS U Double]) <- genRandMatrices [(1000, 1000), (10000, 10000), (100000, 100000), (1000000, 1000000)]
    let 
        test1 = head mdata 
        ans   = to_vector $ axpy 1.0 (from_vector $ U.replicate 100000000 1.0) 1.0 (from_vector $ U.replicate 100000000 1.0)
    return $ deepseq ans ans  
