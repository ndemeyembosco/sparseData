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
import Control.DeepSeq (deepseq)

-- hs = undefined 


sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data_p dict  

config :: Config
config = defaultConfig 




hs :: IO ()
hs = main 

main :: IO ()
main = do
      print "I am here"
      !ans <- bench_dns_big  
    --   bench_csr_big  
    --   bench_ell_big 
    --   bench_csc_big  
      let v = deepseq ans ans 
      print $ U.length v 
      return ()
