{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark.SparseBlas.Sparse.Big.CSR where 

import Criterion.Main 
import Criterion.Types 
import Kernels
import Util.DataLoaderSmall
import qualified Data.Vector.Unboxed as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser 
import SparseBlas.Data.Matrix.Generic.Generic 
import SparseBlas.Data.Matrix.Sparse.COO 
import SparseBlas.Data.Matrix.Sparse.CSR 
import Data.Maybe
 


sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data dict  

config = defaultConfig 


bench_csr_big = do
   (_, big_data_m) <- matrix_data M.empty M.empty 
   let 
      sp_data_bigm_coo = sparse_matrify big_data_m
      sp_data_bigm_csr    = M.map (\m -> manifest_convert m :: SparseData CSR U Double ) sp_data_bigm_coo  
   let big_vec_data = M.fromList [
                    ("bcsstk09", gen_vec 1083)
                   , ("bcsstk11", gen_vec 1473)
                   , ("bcsstk14", gen_vec 1806)
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
