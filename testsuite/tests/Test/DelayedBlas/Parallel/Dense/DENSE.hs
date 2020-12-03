{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds #-}

module DelayedBlas.Parallel.Dense.DENSE where

import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.COO as O  
import qualified DelayedBlas.Data.Matrix.Parallel.Dense.DENSE as D  
import DelayedBlas.Parallel.Sparse.COO () 

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Arbitrary(arbitrary), Args(maxSuccess) )
import DelayedBlas.Parallel.Generic.Generic
    ( convertTest, 
      assocConstMulProp, 
      commutAddProp, 
      assocAddProp, 
      assocMultVecProp, 
      distrConstMulProp, 
      distrAddMultVecProp, 
      scalarVecTransform, 
      multMultLDistr, 
      multMultRDistr, 
      multMultLScalar, 
      multMultRScalar, 
      multMultTrans, 
      multMultAssoc)
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Undelay, Matrix(MatrixData), RepIndex(D, U), manifestConvert )
import qualified Data.Vector.Unboxed as UVector

instance (Arbitrary (MatrixData O.COO U n1 n2 e), Undelay D.DNS n1 n2 e) 
         => Arbitrary (MatrixData D.DNS U n1 n2 e) where 
             arbitrary = do 
                 (arr :: MatrixData O.COO U n1 n2 e) <- arbitrary 
                 let (to_return :: MatrixData D.DNS U n1 n2 e) = manifestConvert arr
                 return to_return

test_dense :: IO () 
test_dense = do 
    print "testing DNS ... \n\n"
    print "undelay . delay = id : \n"
    quickCheckWith (stdArgs {maxSuccess=100}) -- more than one will cause nested data parallelism error (test serial versions first)
                    (convertTest :: MatrixData D.DNS U 10 10 Int -> Bool)

    print " r (s A) == (r s) A: \n"
    quickCheckWith (stdArgs {maxSuccess=100}) 
                    (assocConstMulProp :: Int 
                                    -> Int 
                                    -> MatrixData D.DNS D 100 100 Int -> Bool)

    print "(A + B) == (B + A): \n"
    quickCheckWith (stdArgs {maxSuccess=100}) 
                    (commutAddProp :: MatrixData D.DNS D 100 100 Int 
                                        -> MatrixData D.DNS D 100 100 Int -> Bool)

    print "r (A + B) = r A + r B: \n"
    quickCheckWith (stdArgs {maxSuccess=100}) 
                    (distrConstMulProp :: Int  
                                    -> MatrixData D.DNS D 100 100 Int  
                                    -> MatrixData D.DNS D 100 100 Int -> Bool)

    print "(A + B) + C == A + (B + C): \n"
    quickCheckWith (stdArgs {maxSuccess=100}) 
                    (assocAddProp :: MatrixData D.DNS D 100 100 Int 
                                    -> MatrixData D.DNS D 100 100 Int
                                    -> MatrixData D.DNS D 100 100 Int -> Bool)

    print "A (w + v) = A w + A v : \n"
    quickCheckWith (stdArgs {maxSuccess=10000}) 
                (assocMultVecProp :: UVector.Vector Int  
                                    -> UVector.Vector Int  
                                    -> MatrixData D.DNS D 100 100 Int -> Bool)

    print "(A + B) v = A v + B v : \n"
    quickCheckWith (stdArgs {maxSuccess=10000}) 
                    (distrAddMultVecProp :: UVector.Vector Int  
                                    -> MatrixData D.DNS D 100 100 Int 
                                    -> MatrixData D.DNS D 100 100 Int -> Bool)

    print "A (a  u) = a  (A . u): \n"
    quickCheckWith (stdArgs {maxSuccess=10000}) 
                    (scalarVecTransform :: Int
                                    -> UVector.Vector Int 
                                    -> MatrixData D.DNS D 100 100 Int -> Bool)

    print "A (B + C) = AB + AC: \n"
    quickCheckWith (stdArgs {maxSuccess=500})
                   (multMultLDistr :: MatrixData D.DNS D 10 10 Int 
                                       -> MatrixData D.DNS D 10 10 Int
                                       -> MatrixData D.DNS D 10 10 Int 
                                       -> Bool)

    print "(B + C) D = BD + CD: \n"
    quickCheckWith (stdArgs {maxSuccess=500})
                   (multMultRDistr :: MatrixData D.DNS D 10 10 Int 
                                       -> MatrixData D.DNS D 10 10 Int
                                       -> MatrixData D.DNS D 10 10 Int 
                                       -> Bool)
    print "c(AB) = (cA)B: \n" 
    quickCheckWith (stdArgs {maxSuccess=500}) 
                   (multMultLScalar :: Int 
                                        -> MatrixData D.DNS D 10 10 Int 
                                        -> MatrixData D.DNS D 10 10 Int 
                                        -> Bool)
    print "(AB)c = A(Bc): \n"
    quickCheckWith (stdArgs {maxSuccess=500})
                   (multMultRScalar :: Int 
                                        -> MatrixData D.DNS D 10 10 Int 
                                        -> MatrixData D.DNS D 10 10 Int 
                                        -> Bool)
    print "(AB)^t = B^t A^t: \n"
    quickCheckWith (stdArgs {maxSuccess=500})
                   (multMultTrans :: MatrixData D.DNS D 10 10 Int 
                                      -> MatrixData D.DNS D 10 10 Int 
                                      -> Bool)
    print "(AB)C = A(BC) : \n"
    quickCheckWith (stdArgs {maxSuccess=500}) 
                   (multMultAssoc :: MatrixData D.DNS D 10 10 Int 
                                      -> MatrixData D.DNS D 10 10 Int 
                                      -> MatrixData D.DNS D 10 10 Int 
                                      -> Bool)
    return ()