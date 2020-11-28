{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds #-}

module SparseBlas.Parallel.Dense.DENSE where

import qualified SparseBlas.Data.Matrix.Parallel.Sparse.COO as O  
import qualified SparseBlas.Data.Matrix.Parallel.Dense.DENSE as D  
import SparseBlas.Parallel.Sparse.COO 

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Arbitrary(arbitrary), Args(maxSuccess) )
import SparseBlas.Parallel.Generic.Generic
import SparseBlas.Data.Matrix.Parallel.Generic.Generic
    ( Undelay, Sparse(SparseData), RepIndex(D, U), manifest_convert )
import qualified Data.Vector.Unboxed as UVector

instance (Arbitrary (SparseData O.COO U n1 n2 e), Undelay D.DNS n1 n2 e) 
         => Arbitrary (SparseData D.DNS U n1 n2 e) where 
             arbitrary = do 
                 (arr :: SparseData O.COO U n1 n2 e) <- arbitrary 
                 let (to_return :: SparseData D.DNS U n1 n2 e) = (manifest_convert arr) 
                 return to_return

test_dense :: IO () 
test_dense = do 
    print "testing DNS ... \n\n"
    -- print "undelay . delay = id : \n"
    quickCheckWith (stdArgs {maxSuccess=100}) -- more than one will cause nested data parallelism error 
                    (s_convert_test :: SparseData D.DNS U 10 10 Int -> Bool)

    print " r (s A) == (r s) A: \n"
    quickCheckWith (stdArgs {maxSuccess=100}) 
                    (s_assoc_const_mul_prop :: Int 
                                    -> Int 
                                    -> SparseData D.DNS D 100 100 Int -> Bool)

    print "(A + B) == (B + A): \n"
    quickCheckWith (stdArgs {maxSuccess=100}) 
                    (s_commut_add_prop :: SparseData D.DNS D 100 100 Int 
                                        -> SparseData D.DNS D 100 100 Int -> Bool)

    print "r (A + B) = r A + r B: \n"
    quickCheckWith (stdArgs {maxSuccess=100}) 
                    (s_distr_const_mul_prop :: Int  
                                    -> SparseData D.DNS D 100 100 Int  
                                    -> SparseData D.DNS D 100 100 Int -> Bool)

    print "(A + B) + C == A + (B + C): \n"
    quickCheckWith (stdArgs {maxSuccess=100}) 
                    (s_assoc_add_prop :: SparseData D.DNS D 100 100 Int 
                                    -> SparseData D.DNS D 100 100 Int
                                    -> SparseData D.DNS D 100 100 Int -> Bool)

    print "A (w + v) = A w + A v : \n"
    quickCheckWith (stdArgs {maxSuccess=10000}) 
                (s_assoc_mult_vec_prop :: UVector.Vector Int  
                                    -> UVector.Vector Int  
                                    -> SparseData D.DNS D 100 100 Int -> Bool)

    print "(A + B) v = A v + B v : \n"
    quickCheckWith (stdArgs {maxSuccess=10000}) 
                    (s_distr_add_mult_vec_prop :: UVector.Vector Int  
                                    -> SparseData D.DNS D 100 100 Int 
                                    -> SparseData D.DNS D 100 100 Int -> Bool)

    print "A (a  u) = a  (A . u): \n"
    quickCheckWith (stdArgs {maxSuccess=10000}) 
                    (s_scalar_vec_transform :: Int
                                    -> UVector.Vector Int 
                                    -> SparseData D.DNS D 100 100 Int -> Bool)

    print "A (B + C) = AB + AC: \n"
    quickCheckWith (stdArgs {maxSuccess=500})
                   (s_mult_mult_ldistr :: SparseData D.DNS D 10 10 Int 
                                       -> SparseData D.DNS D 10 10 Int
                                       -> SparseData D.DNS D 10 10 Int 
                                       -> Bool)

    print "(B + C) D = BD + CD: \n"
    quickCheckWith (stdArgs {maxSuccess=500})
                   (s_mult_mult_rdistr :: SparseData D.DNS D 10 10 Int 
                                       -> SparseData D.DNS D 10 10 Int
                                       -> SparseData D.DNS D 10 10 Int 
                                       -> Bool)
    print "c(AB) = (cA)B: \n" 
    quickCheckWith (stdArgs {maxSuccess=500}) 
                   (s_mult_mult_lscalar :: Int 
                                        -> SparseData D.DNS D 10 10 Int 
                                        -> SparseData D.DNS D 10 10 Int 
                                        -> Bool)
    print "(AB)c = A(Bc): \n"
    quickCheckWith (stdArgs {maxSuccess=500})
                   (s_mult_mult_rscalar :: Int 
                                        -> SparseData D.DNS D 10 10 Int 
                                        -> SparseData D.DNS D 10 10 Int 
                                        -> Bool)
    print "(AB)^t = B^t A^t: \n"
    quickCheckWith (stdArgs {maxSuccess=500})
                   (s_mult_mult_trans :: SparseData D.DNS D 10 10 Int 
                                      -> SparseData D.DNS D 10 10 Int 
                                      -> Bool)
    print "(AB)C = A(BC) : \n"
    quickCheckWith (stdArgs {maxSuccess=500}) 
                   (s_mult_mult_assoc :: SparseData D.DNS D 10 10 Int 
                                      -> SparseData D.DNS D 10 10 Int 
                                      -> SparseData D.DNS D 10 10 Int 
                                      -> Bool)
    return ()