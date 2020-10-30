{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark where 

import Criterion.Main 
import Criterion.Types 
import Kernels
import Util.DataLoader
import qualified Data.Vector.Unboxed as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import SparseBlas.Data.Matrix.Generic.Generic 
import SparseBlas.Data.Matrix.Sparse.COO 
import BenchMark.SparseBlas.Sparse.Big.COO 
import BenchMark.SparseBlas.Sparse.Big.CSR 
import BenchMark.SparseBlas.Sparse.Big.ELL 
import BenchMark.SparseBlas.Sparse.Big.CSC
import BenchMark.SparseBlas.Dense.Big.DENSE
import Data.Maybe
import Control.DeepSeq (deepseq)



sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data dict  

config = defaultConfig 

hs = main  

main = do
      print "about to benchmark serial dns"
      vec <- bench_dns_big  
      -- bench_csr_big  
      
      -- bench_ell_big 
      -- bench_csc_big  
      let v = deepseq vec vec  
      print $ U.length v
      return () 
