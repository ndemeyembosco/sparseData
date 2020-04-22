{-# LANGUAGE DataKinds #-}

module BenchMark where 

import Criterion.Main 
import Kernels
import DataLoader
import qualified Data.Vector.Unboxed as U 
import qualified Data.Map.Strict as M
import MMParser 
import SGeneric
import UCOO 
import Data.Maybe



sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data dict  





main = do
   data_m <- matrix_data M.empty
   let sp_data_m = sparse_matrify data_m 
   let vec_data  = M.fromList [("tb", U.replicate 100 1)
                              ,("pr1", U.replicate 30 1)
                              , ("lf", U.replicate 18 1)
                              , ("bcm", U.replicate 112 1)
                              , ("bck", U.replicate 112 1)
                   ] 
   defaultMain [
           bgroup "atax" [
              bench "tub100" $ nf to_vector 
                             $  atax (fromJust $ 
                                                 M.lookup "tub100" sp_data_m) 
                                     (fromJust $ M.lookup "tb" vec_data) 
           ,  bench "pores_1_data" $ nf to_vector  
                                   $ atax (fromJust $ 
                                           M.lookup "pores_1_data" sp_data_m)
                                          (fromJust $ M.lookup "pr1" vec_data) 
           ,  bench "LF10_data" $ nf to_vector 
                                $ atax (fromJust $ 
                                                  M.lookup "LF10_data" sp_data_m)
                                       (fromJust $ M.lookup "lf" vec_data)
           ,  bench "bcsstm03" $ nf to_vector 
                               $ atax (fromJust $ 
                                                  M.lookup "bcsstm03" sp_data_m)
                                      (fromJust $ M.lookup "bcm" vec_data)
           ,  bench "bcsstk03" $ nf to_vector 
                               $ atax (fromJust $ 
                                                  M.lookup "bcsstk03" sp_data_m)
                                      (fromJust $ M.lookup "bck" vec_data)
           ]
       ]
