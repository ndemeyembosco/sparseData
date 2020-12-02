{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds #-}

module DelayedBlas.Parallel.Sparse.ELL where

import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.ELL as E  
import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.COO as O 
import DelayedBlas.Parallel.Sparse.COO 

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Arbitrary(arbitrary), Args(maxSuccess) )
import DelayedBlas.Parallel.Generic.Generic
    ( s_assoc_const_mul_prop,
      s_commut_add_prop,
      s_distr_const_mul_prop,
      s_assoc_add_prop,
      s_assoc_mult_vec_prop,
      s_distr_add_mult_vec_prop,
      s_scalar_vec_transform,
      s_convert_test )
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Undelay, Matrix(MatrixData), RepIndex(D, U), manifest_convert ) 
import qualified Data.Vector.Unboxed as UVector



instance (Arbitrary (MatrixData O.COO U n1 n2 e), Undelay E.ELL n1 n2 e) 
         => Arbitrary (MatrixData E.ELL U n1 n2 e) where 
             arbitrary = do 
                 (arr :: MatrixData O.COO U n1 n2 e) <- arbitrary 
                 let (to_return :: MatrixData E.ELL U n1 n2 e) = (manifest_convert arr) 
                 return to_return


test_ell :: IO () 
test_ell = do 
    print "testing ELL ... \n \n"
    print "undelay . delay = id : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_convert_test :: MatrixData E.ELL U 100 100 Double -> Bool)

    print " r (s A) == (r s) A: \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_assoc_const_mul_prop :: Double 
                                    -> Double 
                                    -> MatrixData E.ELL D 100 100 Double -> Bool)

    print "(A + B) == (B + A): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_commut_add_prop :: MatrixData E.ELL D 100 100 Double 
                                        -> MatrixData E.ELL D 100 100 Double -> Bool)

    print "r (A + B) = r A + r B: \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_distr_const_mul_prop :: Double 
                                    -> MatrixData E.ELL D 100 100 Double 
                                    -> MatrixData E.ELL D 100 100 Double -> Bool)

    print "(A + B) + C == A + (B + C): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_assoc_add_prop :: MatrixData E.ELL D 100 100 Double 
                                    -> MatrixData E.ELL D 100 100 Double 
                                    -> MatrixData E.ELL D 100 100 Double -> Bool)

    print "A (w + v) = A w + A v : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                (s_assoc_mult_vec_prop :: UVector.Vector Double 
                                    -> UVector.Vector Double 
                                    -> MatrixData E.ELL D 100 100 Double -> Bool)

    print "(A + B) v = A v + B v : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_distr_add_mult_vec_prop :: UVector.Vector Double 
                                    -> MatrixData E.ELL D 100 100 Double 
                                    -> MatrixData E.ELL D 100 100 Double -> Bool)

    print "A (a  u) = a  (A . u): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_scalar_vec_transform :: Int
                                    -> UVector.Vector Int 
                                    -> MatrixData E.ELL D 100 100 Int -> Bool)
    return ()
