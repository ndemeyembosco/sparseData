{-# LANGUAGE RankNTypes, AllowAmbiguousTypes #-}
module Test.DelayedBlas.Parallel.PCheck where

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Args(maxSuccess) ) 
import qualified Data.Vector.Unboxed as UVector
import qualified DelayedBlas.Parallel.Generic.Generic as PG ( s_vec_test )  
import DelayedBlas.Parallel.Sparse.COO ( test_coo )  
import DelayedBlas.Parallel.Sparse.CSC ( test_csc ) 
import DelayedBlas.Parallel.Sparse.CSR ( test_csr ) 
import DelayedBlas.Parallel.Sparse.ELL ( test_ell ) 
import DelayedBlas.Parallel.Dense.DENSE ( test_dense ) 
-- import DelayedBlas.Generic.Generic -- hiding (s_vec_test, Arbitrary(..))
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





