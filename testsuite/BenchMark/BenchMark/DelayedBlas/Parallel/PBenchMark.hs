{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark.DelayedBlas.Parallel.PBenchMark where 

import Criterion.Main 
import Criterion.Types 
import BenchMark.DelayedBlas.Parallel.PKernels
-- import Util.DataLoaderSmall
import qualified Data.Vector as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser ( MMExchange, mm_to_s_data_p ) 
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Matrix(MatrixData), RepIndex(U) ) 
-- import DelayedBlas.Data.Matrix.Parallel.Sparse.COO ( COO ) 
-- import BenchMark.DelayedBlas.Parallel.Sparse.Big.COO
--     ( bench_coo_big ) 
-- import BenchMark.DelayedBlas.Parallel.Sparse.Big.CSR
--     ( bench_csr_big ) 
-- import BenchMark.DelayedBlas.Parallel.Sparse.Big.ELL
--     ( bench_ell_big ) 
-- import BenchMark.DelayedBlas.Parallel.Sparse.Big.CSC
--     ( bench_csc_big )
import BenchMark.DelayedBlas.Parallel.Dense.Big.DENSE 
    (bench_dns_big)
import Data.Maybe
import System.Random.PCG
import Control.DeepSeq (deepseq)
import Control.Monad.ST  

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
      return ()



