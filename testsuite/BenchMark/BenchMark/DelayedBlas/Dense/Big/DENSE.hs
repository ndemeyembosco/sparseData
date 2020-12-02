{-# LANGUAGE DataKinds, BangPatterns, ScopedTypeVariables #-}

module BenchMark.SparseBlas.Dense.Big.DENSE where 

import Criterion.Main 
import Criterion.Types 
import Kernels 
import qualified Data.Vector.Unboxed as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import SparseBlas.Data.Matrix.Generic.Generic 
import SparseBlas.Data.Matrix.Dense.DENSE 
import Data.Maybe
import Control.Monad.IO.Class 
import Control.DeepSeq (deepseq)
import System.Random 

genRandMatrix :: (U.Unbox a, Random a) => Int -> Int -> IO (SparseData DNS U a)
genRandMatrix width height = do 
    stdGen <- getStdGen 
    let rs = (U.fromList $ take (width * height) $ randoms stdGen)
    return $ DNS rs width height  

genRandMatrices :: (U.Unbox a, Random a) => [(Int, Int)] -> IO [(SparseData DNS U a)]
genRandMatrices = mapM (\(x, y) -> genRandMatrix x y) 





bench_dns_big :: IO (U.Vector Double)
bench_dns_big = do
    (mdata :: [SparseData DNS U Double]) <- genRandMatrices [(1000, 1000), (10000, 10000), (100000, 100000), (1000000, 1000000)]
    let 
        test1 = head mdata 
        ans   = to_vector $ axpy 1.0 (from_vector $ U.replicate 100000000 1.0) 1.0 (from_vector $ U.replicate 100000000 1.0)
    return $ deepseq ans ans  
