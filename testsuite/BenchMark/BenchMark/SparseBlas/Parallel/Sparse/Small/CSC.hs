{-# LANGUAGE DataKinds #-}
module BenchMark.SparseBlas.Parallel.Sparse.Small.CSC where 

import Criterion.Main
    ( defaultMainWith, defaultConfig, bench, bgroup, nf ) 
import BenchMark.SparseBlas.Parallel.PKernels ( ataxTwiceAddNoForce, ataxTwiceAddForce )
import Util.DataLoaderSmall ( matrix_data )
import qualified Data.Vector as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser ( MMExchange, mm_to_s_data_p ) 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic
    ( Sparse(SparseData),
      RepIndex(U),
      to_vector,
      from_vector,
      manifest_convert ) 
import SparseBlas.Data.Matrix.Parallel.Sparse.COO ( COO ) 
import SparseBlas.Data.Matrix.Parallel.Sparse.CSC ( CSC ) 
import Data.Maybe ( fromJust )
import Criterion.Types (Config)



sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data_p dict  

config :: Config
config = defaultConfig 


bench_csc :: IO ()
bench_csc = do
   (small_data_m, big_data_m) <- matrix_data M.empty M.empty 
   let 
      sp_data_m_coo    = sparse_matrify small_data_m 
      sp_data_m_csc    = M.map (\m -> manifest_convert m :: SparseData CSC U Double ) sp_data_m_coo  
   let vec_data  = M.fromList [
                               ("tb", U.replicate 100 1.0 :: U.Vector Double)
                              ,("pr1", U.replicate 30 1.0 :: U.Vector Double)
                              , ("lf", U.replicate 18 1.0 :: U.Vector Double)
                              , ("bcm", U.replicate 112 1.0 :: U.Vector Double)
                              , ("bck", U.replicate 112 1.0 :: U.Vector Double)

                   ]

   defaultMainWith config [
           bgroup "twice_atax_small_force_csc" [
              bench "tub100" $ nf to_vector 
                             $ ataxTwiceAddForce (fromJust $ M.lookup "tub100" sp_data_m_csc)
                                    (from_vector (fromJust $ M.lookup "tb" vec_data)) 
           ,  bench "pores_1_data" $ nf to_vector  
                                   $ ataxTwiceAddForce (fromJust $ M.lookup "pores_1_data" sp_data_m_csc) 
                                          (from_vector (fromJust $ M.lookup "pr1" vec_data)) 
           ,  bench "LF10_data" $ nf to_vector 
                                $ ataxTwiceAddForce (fromJust $ M.lookup "LF10_data" sp_data_m_csc)
                                       (from_vector (fromJust $ M.lookup "lf" vec_data)) 
           ,  bench "bcsstm03" $ nf to_vector 
                               $ ataxTwiceAddForce (fromJust $ M.lookup "bcsstm03" sp_data_m_csc)
                                      (from_vector (fromJust $ M.lookup "bcm" vec_data)) 
           ,  bench "bcsstk03" $ nf to_vector 
                               $ ataxTwiceAddForce (fromJust $ M.lookup "bcsstk03" sp_data_m_csc)
                                      (from_vector (fromJust $ M.lookup "bck" vec_data))
           ]

           ,   bgroup "twice_atax_small_no_force_csc" [
              bench "tub100" $ nf to_vector 
                             $ ataxTwiceAddNoForce (fromJust $ M.lookup "tub100" sp_data_m_csc)
                                    (from_vector (fromJust $ M.lookup "tb" vec_data)) 
           ,  bench "pores_1_data" $ nf to_vector  
                                   $ ataxTwiceAddNoForce (fromJust $ M.lookup "pores_1_data" sp_data_m_csc) 
                                          (from_vector (fromJust $ M.lookup "pr1" vec_data)) 
           ,  bench "LF10_data" $ nf to_vector 
                                $ ataxTwiceAddNoForce (fromJust $ M.lookup "LF10_data" sp_data_m_csc)
                                       (from_vector (fromJust $ M.lookup "lf" vec_data)) 
           ,  bench "bcsstm03" $ nf to_vector 
                               $ ataxTwiceAddNoForce (fromJust $ M.lookup "bcsstm03" sp_data_m_csc)
                                      (from_vector (fromJust $ M.lookup "bcm" vec_data)) 
           ,  bench "bcsstk03" $ nf to_vector 
                               $ ataxTwiceAddNoForce (fromJust $ M.lookup "bcsstk03" sp_data_m_csc)
                                      (from_vector (fromJust $ M.lookup "bck" vec_data))
           ]
          
       ]