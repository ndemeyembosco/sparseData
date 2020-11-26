{-# LANGUAGE RankNTypes, AllowAmbiguousTypes #-}
module Test.SparseBlas.Parallel.PCheck where

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Args(maxSuccess) ) 
import qualified Data.Vector.Unboxed as UVector
import qualified SparseBlas.Parallel.Generic.Generic as PG ( s_vec_test )  
import SparseBlas.Parallel.Sparse.COO ( test_coo )  
import SparseBlas.Parallel.Sparse.CSC ( test_csc ) 
import SparseBlas.Parallel.Sparse.CSR ( test_csr ) 
import SparseBlas.Parallel.Sparse.ELL ( test_ell ) 
import SparseBlas.Parallel.Dense.DENSE ( test_dense ) 
-- import SparseBlas.Generic.Generic -- hiding (s_vec_test, Arbitrary(..))
import GHC.TypeLits 
import Data.Proxy 



    
hs = main  

main :: IO () 
main = do 
--  quickCheckWith (stdArgs {maxSuccess=1000}) 
--                 (PG.s_vec_test :: forall n. (KnownNat n) => UVector.Vector Double -> Bool)
--  test_coo 
--  test_csc 
--  test_csr 
--  test_ell 
 test_dense 
 return () 





