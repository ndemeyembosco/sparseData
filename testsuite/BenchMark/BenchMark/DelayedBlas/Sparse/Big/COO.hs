{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark.SparseBlas.Sparse.Big.COO where 

import Criterion.Main 
import Criterion.Types 
import Kernels
import Util.DataLoader
import qualified Data.Vector.Unboxed as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import SparseBlas.Data.Matrix.Generic.Generic 
import SparseBlas.Data.Matrix.Sparse.COO 
import SparseBlas.Data.Matrix.Sparse.CSR 
import SparseBlas.Data.Matrix.Sparse.ELL 
import SparseBlas.Data.Matrix.Sparse.CSC 
import Data.Maybe
import Control.DeepSeq (deepseq)



sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data dict  

config = defaultConfig 


bench_coo_big :: IO (U.Vector Double)
bench_coo_big = do
   print "entering bench_coo"
   big_data_m <- matrix_data M.empty M.empty 
   let 
      sp_data_bigm_coo = sparse_matrify big_data_m
      !ans =  to_vector $ axpy 1.0 (from_vector (fromJust $ M.lookup "asic_680k" vec_data_u)) 1.0 (from_vector (fromJust $ M.lookup "asic_680k" vec_data_u))
      dud  = deepseq ans 0 
   print "should have printed length"
   return ans 
