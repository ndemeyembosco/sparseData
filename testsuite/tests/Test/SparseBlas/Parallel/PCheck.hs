module Test.SparseBlas.Parallel.PCheck where

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Args(maxSuccess) ) 
import qualified Data.Vector as UVector
import SparseBlas.Parallel.Generic.Generic ( s_vec_test )  
import SparseBlas.Parallel.Sparse.COO ( test_coo )  
import SparseBlas.Parallel.Sparse.CSC ( test_csc ) 
import SparseBlas.Parallel.Sparse.CSR ( test_csr ) 
import SparseBlas.Parallel.Sparse.ELL ( test_ell ) 
import SparseBlas.Parallel.Dense.DENSE ( test_dense ) 



    
hs = main  

main :: IO () 
main = do 
 quickCheckWith (stdArgs {maxSuccess=1000}) 
                (s_vec_test :: UVector.Vector Double -> Bool)
 test_coo 
 test_csc 
 test_csr 
 test_ell 
 test_dense 
 return () 




