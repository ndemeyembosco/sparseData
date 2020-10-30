{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark.SparseBlas.Parallel.Sparse.Big.CSR where 

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
import Data.Maybe
 


sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data_p dict  

config = defaultConfig 


bench_csr_big = do
   big_data_m <- matrix_data M.empty M.empty 
   let 
      sp_data_bigm_coo = sparse_matrify big_data_m
      sp_data_bigm_csr    = M.map (\m -> manifest_convert m :: SparseData CSR U Double ) sp_data_bigm_coo  
   let big_vec_data = M.fromList [
                    ("bcsstk09", gen_vec_boxed 1083)
                   , ("bcsstk11", gen_vec_boxed 1473)
                   , ("bcsstk14", gen_vec_boxed 1806)
                   ]



   defaultMainWith config [

        bgroup "mvec_big_csr" [
              bench "bcsstk09" $ nf to_vector 
                                $ mvec (fromJust $ M.lookup "bcsstk09" sp_data_bigm_csr)
                                    (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 
           ,  bench "bcsstk11" $ nf to_vector  
                                   $ mvec (fromJust $ M.lookup "bcsstk11" sp_data_bigm_csr) 
                                          (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 
           ,  bench "bcsstk14" $ nf to_vector 
                                $ mvec (fromJust $ M.lookup "bcsstk14" sp_data_bigm_csr)
                                       (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 
        ]


    ]
