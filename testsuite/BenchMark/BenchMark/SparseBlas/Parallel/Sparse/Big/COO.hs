{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark.SparseBlas.Parallel.Sparse.Big.COO where 

import Criterion.Main 
import Criterion.Types 
import BenchMark.SparseBlas.Parallel.PKernels
import Util.DataLoader
import qualified Data.Vector as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic 
import SparseBlas.Data.Matrix.Parallel.Sparse.COO 
import SparseBlas.Data.Matrix.Parallel.Sparse.CSR 
import SparseBlas.Data.Matrix.Parallel.Sparse.ELL 
import SparseBlas.Data.Matrix.Parallel.Sparse.CSC 
import Data.Maybe
import Control.DeepSeq (deepseq)



sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data_p dict  

config = defaultConfig 


bench_coo_big :: IO (U.Vector Double)
bench_coo_big = do
   print "entering bench_coo"
   big_data_m <- matrix_data M.empty M.empty 
   let 
      sp_data_bigm_coo = sparse_matrify big_data_m
      !ans =  to_vector $ axpy 1.0 (from_vector (fromJust $ M.lookup "asic_680k" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "asic_680k" vec_data))
      dud  = deepseq ans 0 
   print "should have printed length"
   return ans 
