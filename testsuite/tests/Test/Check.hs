module Test.Check where

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Args(maxSuccess) ) 
import qualified Data.Vector.Unboxed as UVector
import SparseBlas.Generic.Generic ( s_vec_test )  
import SparseBlas.Sparse.COO ( test_coo )  
import SparseBlas.Sparse.CSC ( test_csc ) 
import SparseBlas.Sparse.CSR ( test_csr ) 
import SparseBlas.Sparse.ELL ( test_ell ) 
import SparseBlas.Dense.DENSE ( test_dense ) 



    


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





