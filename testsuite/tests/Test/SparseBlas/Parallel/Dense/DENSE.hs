{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds #-}

module SparseBlas.Parallel.Dense.DENSE where

import qualified SparseBlas.Data.Matrix.Sparse.COO as O  
import qualified SparseBlas.Data.Matrix.Dense.DENSE as D  
import SparseBlas.Sparse.COO 

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Arbitrary(arbitrary), Args(maxSuccess) )
import SparseBlas.Generic.Generic
    ( s_assoc_const_mul_prop,
      s_commut_add_prop,
      s_distr_const_mul_prop,
      s_assoc_add_prop,
      s_assoc_mult_vec_prop,
      s_distr_add_mult_vec_prop,
      s_scalar_vec_transform,
      s_convert_test ) 
import SparseBlas.Data.Matrix.Generic.Generic
    ( Undelay, Sparse(SparseData), RepIndex(D, U), manifest_convert )
import qualified Data.Vector.Unboxed as UVector

instance (Arbitrary (SparseData O.COO U e), Undelay D.DNS e) 
         => Arbitrary (SparseData D.DNS U e) where 
             arbitrary = do 
                 (arr :: SparseData O.COO U e) <- arbitrary 
                 let (to_return :: SparseData D.DNS U e) = (manifest_convert arr) 
                 return to_return

test_dense :: IO () 
test_dense = do 
    print "testing DNS ... \n\n"
    print "undelay . delay = id : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_convert_test :: SparseData D.DNS U Double -> Bool)

    print " r (s A) == (r s) A: \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_assoc_const_mul_prop :: Int 
                                    -> Int 
                                    -> SparseData D.DNS D Int -> Bool)

    print "(A + B) == (B + A): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_commut_add_prop :: SparseData D.DNS D Double 
                                        -> SparseData D.DNS D Double -> Bool)

    print "r (A + B) = r A + r B: \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_distr_const_mul_prop :: Int  
                                    -> SparseData D.DNS D Int  
                                    -> SparseData D.DNS D Int -> Bool)

    print "(A + B) + C == A + (B + C): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_assoc_add_prop :: SparseData D.DNS D Double 
                                    -> SparseData D.DNS D Double 
                                    -> SparseData D.DNS D Double -> Bool)

    print "A (w + v) = A w + A v : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                (s_assoc_mult_vec_prop :: UVector.Vector Double 
                                    -> UVector.Vector Double 
                                    -> SparseData D.DNS D Double -> Bool)

    print "(A + B) v = A v + B v : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_distr_add_mult_vec_prop :: UVector.Vector Double 
                                    -> SparseData D.DNS D Double 
                                    -> SparseData D.DNS D Double -> Bool)

    print "A (a  u) = a  (A . u): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_scalar_vec_transform :: Int
                                    -> UVector.Vector Int 
                                    -> SparseData D.DNS D Int -> Bool)
    return ()