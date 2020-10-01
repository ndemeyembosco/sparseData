{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMarkSmall where 

import Criterion.Main 
import Criterion.Types 
import Kernels
import DataLoaderSmall
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
      sp_data_m_coo    = sparse_matrify small_data_m 
      sp_data_m_csr    = M.map (\m -> manifest_convert m :: SparseData CSR U Double ) sp_data_m_coo  
      
      sp_data_m_ell    = M.map (\m -> let !m' = (manifest_convert m :: SparseData ELL U Double)  in m' ) sp_data_m_coo  
      sp_data_m_csc    = M.map (\m -> manifest_convert m :: SparseData CSC U Double ) sp_data_m_csc 
   let vec_data  = M.fromList [
                               ("tb", U.replicate 100 1.0 :: U.Vector Double)
                              ,("pr1", U.replicate 30 1.0 :: U.Vector Double)
                              , ("lf", U.replicate 18 1.0 :: U.Vector Double)
                              , ("bcm", U.replicate 112 1.0 :: U.Vector Double)
                              , ("bck", U.replicate 112 1.0 :: U.Vector Double)

                   ]

   defaultMainWith config [

          --- axpy 
       --      bgroup "axpy_small_coo" [
       --        bench "tub100" $ nf to_vector 
       --                       $ axpy 1.0
       --                              (from_vector (fromJust $ M.lookup "tb" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "tb" vec_data))
       --     ,  bench "pores_1_data" $ nf to_vector  
       --                             $ axpy 1.0 
       --                                    (from_vector (fromJust $ M.lookup "pr1" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "pr1" vec_data))
       --     ,  bench "LF10_data" $ nf to_vector 
       --                          $ axpy 1.0 
       --                                 (from_vector (fromJust $ M.lookup "lf" vec_data)) 1.0  (from_vector (fromJust $ M.lookup "lf" vec_data))
       --     ,  bench "bcsstm03" $ nf to_vector 
       --                         $ axpy 1.0 
       --                                (from_vector (fromJust $ M.lookup "bcm" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcm" vec_data))
       --     ,  bench "bcsstk03" $ nf to_vector 
       --                         $ axpy 1.0 
       --                                (from_vector (fromJust $ M.lookup "bck" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bck" vec_data))
       --     ]
       --      , bgroup "twice_axpy_small_no_force_coo" [
       --        bench "tub100" $ nf to_vector 
       --                       $ twiceAxpyNoForce 1.0 
       --                              (from_vector (fromJust $ M.lookup "tb" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "tb" vec_data))
       --     ,  bench "pores_1_data" $ nf to_vector  
       --                             $ twiceAxpyNoForce 1.0 
       --                                    (from_vector (fromJust $ M.lookup "pr1" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "pr1" vec_data))
       --     ,  bench "LF10_data" $ nf to_vector 
       --                          $ twiceAxpyNoForce 1.0 
       --                                 (from_vector (fromJust $ M.lookup "lf" vec_data)) 1.0  (from_vector (fromJust $ M.lookup "lf" vec_data))
       --     ,  bench "bcsstm03" $ nf to_vector 
       --                         $ twiceAxpyNoForce 1.0 
       --                                (from_vector (fromJust $ M.lookup "bcm" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcm" vec_data))
       --     ,  bench "bcsstk03" $ nf to_vector 
       --                         $ twiceAxpyNoForce 1.0 
       --                                (from_vector (fromJust $ M.lookup "bck" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bck" vec_data))
       --     ]

       --  ,   bgroup "twice_axpy_small_force_coo" [
       --        bench "tub100" $ nf to_vector 
       --                       $ twiceAxpyForce 1.0 
       --                              (from_vector (fromJust $ M.lookup "tb" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "tb" vec_data))
       --     ,  bench "pores_1_data" $ nf to_vector  
       --                             $ twiceAxpyForce 1.0 
       --                                    (from_vector (fromJust $ M.lookup "pr1" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "pr1" vec_data))
       --     ,  bench "LF10_data" $ nf to_vector 
       --                          $ twiceAxpyForce 1.0 
       --                                 (from_vector (fromJust $ M.lookup "lf" vec_data)) 1.0  (from_vector (fromJust $ M.lookup "lf" vec_data))
       --     ,  bench "bcsstm03" $ nf to_vector 
       --                         $ twiceAxpyForce 1.0 
       --                                (from_vector (fromJust $ M.lookup "bcm" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bcm" vec_data))
       --     ,  bench "bcsstk03" $ nf to_vector 
       --                         $ twiceAxpyForce 1.0 
       --                                (from_vector (fromJust $ M.lookup "bck" vec_data)) 1.0 (from_vector (fromJust $ M.lookup "bck" vec_data))
       --     ]

       -- atax 

       -- , 
       -- bgroup "mvec_small_coo" [
       --        bench "tub100" $ nf to_vector 
       --                       $ mvec (fromJust $ M.lookup "tub100" sp_data_m_coo)
       --                              (from_vector (fromJust $ M.lookup "tb" vec_data)) 
       --     ,  bench "pores_1_data" $ nf to_vector  
       --                             $ mvec (fromJust $ M.lookup "pores_1_data" sp_data_m_coo) 
       --                                    (from_vector (fromJust $ M.lookup "pr1" vec_data)) 
       --     ,  bench "LF10_data" $ nf to_vector 
       --                          $ mvec (fromJust $ M.lookup "LF10_data" sp_data_m_coo)
       --                                 (from_vector (fromJust $ M.lookup "lf" vec_data)) 
       --     ,  bench "bcsstm03" $ nf to_vector 
       --                         $ mvec (fromJust $ M.lookup "bcsstm03" sp_data_m_coo)
       --                                (from_vector (fromJust $ M.lookup "bcm" vec_data)) 
       --     ,  bench "bcsstk03" $ nf to_vector 
       --                         $ mvec (fromJust $ M.lookup "bcsstk03" sp_data_m_coo)
       --                                (from_vector (fromJust $ M.lookup "bck" vec_data)) 
       --     ]
       --      , bgroup "atax_small_no_force_coo" [
       --        bench "tub100" $ nf to_vector 
       --                       $ ataxNoForce (fromJust $ M.lookup "tub100" sp_data_m_coo)
       --                              (from_vector (fromJust $ M.lookup "tb" vec_data)) 
       --     ,  bench "pores_1_data" $ nf to_vector  
       --                             $ ataxTwiceNoForce (fromJust $ M.lookup "pores_1_data" sp_data_m_coo) 
       --                                    (from_vector (fromJust $ M.lookup "pr1" vec_data)) 
       --     ,  bench "LF10_data" $ nf to_vector 
       --                          $ ataxNoForce (fromJust $ M.lookup "LF10_data" sp_data_m_coo)
       --                                 (from_vector (fromJust $ M.lookup "lf" vec_data)) 
       --     ,  bench "bcsstm03" $ nf to_vector 
       --                         $ ataxNoForce (fromJust $ M.lookup "bcsstm03" sp_data_m_coo)
       --                                (from_vector (fromJust $ M.lookup "bcm" vec_data)) 
       --     ,  bench "bcsstk03" $ nf to_vector 
       --                         $ ataxNoForce (fromJust $ M.lookup "bcsstk03" sp_data_m_coo)
       --                                (from_vector (fromJust $ M.lookup "bck" vec_data)) 
       --     ]



       --  ,   bgroup "atax_small_force_coo" [
       --        bench "tub100" $ nf to_vector 
       --                       $ ataxForce (fromJust $ M.lookup "tub100" sp_data_m_coo)
       --                              (from_vector (fromJust $ M.lookup "tb" vec_data)) 
       --     ,  bench "pores_1_data" $ nf to_vector  
       --                             $ ataxTwiceForce (fromJust $ M.lookup "pores_1_data" sp_data_m_coo) 
       --                                    (from_vector (fromJust $ M.lookup "pr1" vec_data)) 
       --     ,  bench "LF10_data" $ nf to_vector 
       --                          $ ataxForce (fromJust $ M.lookup "LF10_data" sp_data_m_coo)
       --                                 (from_vector (fromJust $ M.lookup "lf" vec_data)) 
       --     ,  bench "bcsstm03" $ nf to_vector 
       --                         $ ataxForce (fromJust $ M.lookup "bcsstm03" sp_data_m_coo)
       --                                (from_vector (fromJust $ M.lookup "bcm" vec_data)) 
       --     ,  bench "bcsstk03" $ nf to_vector 
       --                         $ ataxForce (fromJust $ M.lookup "bcsstk03" sp_data_m_coo)
       --                                (from_vector (fromJust $ M.lookup "bck" vec_data))
       --     ]

       --     ,   
           bgroup "twice_atax_small_force_coo" [
              bench "tub100" $ nf to_vector 
                             $ ataxTwiceAddForce (fromJust $ M.lookup "tub100" sp_data_m_coo)
                                    (from_vector (fromJust $ M.lookup "tb" vec_data)) 
           ,  bench "pores_1_data" $ nf to_vector  
                                   $ ataxTwiceAddForce (fromJust $ M.lookup "pores_1_data" sp_data_m_coo) 
                                          (from_vector (fromJust $ M.lookup "pr1" vec_data)) 
           ,  bench "LF10_data" $ nf to_vector 
                                $ ataxTwiceAddForce (fromJust $ M.lookup "LF10_data" sp_data_m_coo)
                                       (from_vector (fromJust $ M.lookup "lf" vec_data)) 
           ,  bench "bcsstm03" $ nf to_vector 
                               $ ataxTwiceAddForce (fromJust $ M.lookup "bcsstm03" sp_data_m_coo)
                                      (from_vector (fromJust $ M.lookup "bcm" vec_data)) 
           ,  bench "bcsstk03" $ nf to_vector 
                               $ ataxTwiceAddForce (fromJust $ M.lookup "bcsstk03" sp_data_m_coo)
                                      (from_vector (fromJust $ M.lookup "bck" vec_data))
           ]

           ,   bgroup "twice_atax_small_no_force_coo" [
              bench "tub100" $ nf to_vector 
                             $ ataxTwiceAddNoForce (fromJust $ M.lookup "tub100" sp_data_m_coo)
                                    (from_vector (fromJust $ M.lookup "tb" vec_data)) 
           ,  bench "pores_1_data" $ nf to_vector  
                                   $ ataxTwiceAddNoForce (fromJust $ M.lookup "pores_1_data" sp_data_m_coo) 
                                          (from_vector (fromJust $ M.lookup "pr1" vec_data)) 
           ,  bench "LF10_data" $ nf to_vector 
                                $ ataxTwiceAddNoForce (fromJust $ M.lookup "LF10_data" sp_data_m_coo)
                                       (from_vector (fromJust $ M.lookup "lf" vec_data)) 
           ,  bench "bcsstm03" $ nf to_vector 
                               $ ataxTwiceAddNoForce (fromJust $ M.lookup "bcsstm03" sp_data_m_coo)
                                      (from_vector (fromJust $ M.lookup "bcm" vec_data)) 
           ,  bench "bcsstk03" $ nf to_vector 
                               $ ataxTwiceAddNoForce (fromJust $ M.lookup "bcsstk03" sp_data_m_coo)
                                      (from_vector (fromJust $ M.lookup "bck" vec_data))
           ]
          
       ]
