{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMark.SparseBlas.Parallel.PBenchMarkSmall where 

import Criterion.Main
    ( defaultMainWith, defaultConfig, bench, bgroup, nf ) 
import BenchMark.SparseBlas.Parallel.PKernels ( ataxTwiceAddNoForce, ataxTwiceAddForce )
import Util.DataLoader ( matrix_data )
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
import BenchMark.SparseBlas.Parallel.Sparse.Small.COO 
import BenchMark.SparseBlas.Parallel.Sparse.Small.CSR 
import BenchMark.SparseBlas.Parallel.Sparse.Small.ELL 
import BenchMark.SparseBlas.Parallel.Sparse.Small.CSC 
import Data.Maybe ( fromJust )
import Criterion.Types (Config)



sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data_p dict  

config :: Config
config = defaultConfig 




hs = main 

main = do
      bench_coo 
      bench_csr 
      bench_ell 
      bench_csc 
      return ()
