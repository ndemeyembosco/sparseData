{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark where 

import Criterion.Main 
import Criterion.Types 
import Kernels
import DataLoader
import qualified Data.Vector.Unboxed as U 
import qualified Data.Map.Strict as M
import MMParser 
import SGeneric
import UCOO 
import UCSR 
import UELL 
import UCSC 
import Data.Maybe



sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data dict  

config = defaultConfig 

main = do
   (small_data_m, big_data_m) <- matrix_data M.empty M.empty 
   let 
      sp_data_bigm_coo = sparse_matrify big_data_m
      sp_data_bigm_csr    = M.map (\m -> manifest_convert m :: SparseData CSR U Double ) sp_data_bigm_coo  
      sp_data_bigm_ell    = M.map (\m -> let !m' = (manifest_convert m :: SparseData ELL U Double) in m') sp_data_bigm_coo  
      sp_data_bigm_csc    = M.map (\m -> manifest_convert m :: SparseData CSC U Double ) sp_data_bigm_csc
   let big_vec_data = M.fromList [
                    ("bcsstk09", gen_vec 1083)
                   , ("bcsstk11", gen_vec 1473)
                   , ("bcsstk14", gen_vec 1806)
                   ]



   defaultMainWith config [
           --- axpy 
       --      bgroup "axpy_thousands_coo" [
       --        bench "bcsstk09" $ nf to_vector 
       --                       $ axpy 1.0
       --                              (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data))
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ axpy 1.0 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data))
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ axpy 1.0 
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 1.0  (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data))
       --     ]
       --      , bgroup "twice_axpy_thousands_no_force_coo" [
       --        bench "bcsstk09" $ nf to_vector 
       --                       $ twiceAxpyNoForce 1.0 
       --                              (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data))
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ twiceAxpyNoForce 1.0 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data))
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ twiceAxpyNoForce 1.0 
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 1.0  (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data))
       --     ]

       --  ,   bgroup "twice_axpy_thousands_force_coo" [
       --        bench "bcsstk09" $ nf to_vector 
       --                       $ twiceAxpyForce 1.0 
       --                              (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data))
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ twiceAxpyForce 1.0 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data))
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ twiceAxpyForce 1.0 
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 1.0  (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data))
       --     ]

       -- atax 

       -- , 
       --  bgroup "mvec_big_coo" [
       --        bench "bcsstk09" $ nf to_vector 
       --                          $ mvec (fromJust $ M.lookup "bcsstk09" sp_data_bigm_coo)
       --                              (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ mvec (fromJust $ M.lookup "bcsstk11" sp_data_bigm_coo) 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ mvec (fromJust $ M.lookup "bcsstk14" sp_data_bigm_coo)
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 
       --  ]

       --  ,  
       --  bgroup "atax_no_force_big_coo" [
       --        bench "bcsstk09" $ nf to_vector 
       --                          $ ataxNoForce (fromJust $ M.lookup "bcsstk09" sp_data_bigm_coo)
       --                              (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ ataxNoForce (fromJust $ M.lookup "bcsstk11" sp_data_bigm_coo) 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ ataxNoForce (fromJust $ M.lookup "bcsstk14" sp_data_bigm_coo)
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 
       --  ]

       --  ,  
       --  bgroup "atax_force_big_coo" [
       --        bench "bcsstk09" $ nf to_vector 
       --                          $ ataxForce (fromJust $ M.lookup "bcsstk09" sp_data_bigm_coo)
       --                              (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ ataxForce (fromJust $ M.lookup "bcsstk11" sp_data_bigm_coo) 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ ataxForce (fromJust $ M.lookup "bcsstk14" sp_data_bigm_coo)
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 
       --  ]

       --  ,  
       --  bgroup "mvec_big_csr" [
       --        bench "bcsstk09" $ nf to_vector 
       --                          $ mvec (fromJust $ M.lookup "bcsstk09" sp_data_bigm_csr)
       --                              (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ mvec (fromJust $ M.lookup "bcsstk11" sp_data_bigm_csr) 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ mvec (fromJust $ M.lookup "bcsstk14" sp_data_bigm_csr)
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 
       --  ]
       -- --  , 

        bgroup "mvec_big_ell" [
              bench "bcsstk09" $ nf to_vector 
                                $ mvec (fromJust $ M.lookup "bcsstk09" sp_data_bigm_ell)
                                    (from_vector (fromJust $ M.lookup "bcsstk09" big_vec_data)) 
           ,  bench "bcsstk11" $ nf to_vector  
                                   $ mvec (fromJust $ M.lookup "bcsstk11" sp_data_bigm_ell) 
                                          (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 
           ,  bench "bcsstk14" $ nf to_vector 
                                $ mvec (fromJust $ M.lookup "bcsstk14" sp_data_bigm_ell)
                                       (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 
        ]
       --  , bgroup "twice_atax_small_no_force_coo" [
       --        bench "bcsstk09" $ nf to_vector 
       --                       $ ataxTwiceNoForce (fromJust $ M.lookup "bcsstk09" sp_data_bigm_coo)
       --                              (from_vector (fromJust $ M.lookup "tb" big_vec_data)) 
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ ataxTwiceNoForce (fromJust $ M.lookup "bcsstk11" sp_data_bigm_coo) 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ ataxTwiceNoForce (fromJust $ M.lookup "bcsstk14" sp_data_bigm_coo)
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 
       --  ]
       --  ,   bgroup "twice_atax_small_force_coo" [
       --        bench "bcsstk09" $ nf to_vector 
       --                       $ ataxTwiceForce (fromJust $ M.lookup "bcsstk09" sp_data_bigm_coo)
       --                              (from_vector (fromJust $ M.lookup "tb" big_vec_data)) 
       --     ,  bench "bcsstk11" $ nf to_vector  
       --                             $ ataxTwiceForce (fromJust $ M.lookup "bcsstk11" sp_data_bigm_coo) 
       --                                    (from_vector (fromJust $ M.lookup "bcsstk11" big_vec_data)) 
       --     ,  bench "bcsstk14" $ nf to_vector 
       --                          $ ataxTwiceForce (fromJust $ M.lookup "bcsstk14" sp_data_bigm_coo)
       --                                 (from_vector (fromJust $ M.lookup "bcsstk14" big_vec_data)) 
       --  ]


    ]
