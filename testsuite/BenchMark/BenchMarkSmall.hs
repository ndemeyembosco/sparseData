{-# LANGUAGE DataKinds, BangPatterns #-}

module BenchMarkSmall where 

import Criterion.Main
    ( defaultMainWith, defaultConfig, bench, bgroup, nf ) 
import Kernels ( ataxTwiceAddNoForce, ataxTwiceAddForce )
import Util.DataLoaderSmall ( matrix_data )
import qualified Data.Vector.Unboxed as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser ( MMExchange, mm_to_s_data ) 
import SparseBlas.Data.Matrix.Generic.Generic
    ( Sparse(SparseData),
      RepIndex(U),
      to_vector,
      from_vector,
      manifest_convert ) 
import SparseBlas.Data.Matrix.Sparse.COO ( COO ) 
import BenchMark.SparseBlas.Sparse.Small.COO 
import BenchMark.SparseBlas.Sparse.Small.CSR 
import BenchMark.SparseBlas.Sparse.Small.ELL 
import BenchMark.SparseBlas.Sparse.Small.CSC 
import Data.Maybe ( fromJust )
import Criterion.Types (Config)



sparse_matrify :: M.Map String MMExchange 
               -> M.Map String (SparseData COO U Double)
sparse_matrify dict = M.map mm_to_s_data dict  

config :: Config
config = defaultConfig 

hs = undefined 

main = do
      bench_coo 
      bench_csr 
      bench_ell 
      bench_csc 
      return ()
