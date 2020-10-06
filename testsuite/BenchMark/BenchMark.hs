{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark where 

import Criterion.Main 
import Criterion.Types 
import Kernels
import Util.DataLoaderSmall
import qualified Data.Vector.Unboxed as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import SparseBlas.Data.Matrix.Generic.Generic 
import SparseBlas.Data.Matrix.Sparse.COO 
import BenchMark.SparseBlas.Sparse.Big.COO 
import BenchMark.SparseBlas.Sparse.Big.CSR 
import BenchMark.SparseBlas.Sparse.Big.ELL 
import BenchMark.SparseBlas.Sparse.Big.CSC
import Data.Maybe

-- hs = undefined 


sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data dict  

config = defaultConfig 

hs = undefined 

main = do
      bench_coo_big  
      bench_csr_big  
      bench_ell_big 
      bench_csc_big  
      return ()
