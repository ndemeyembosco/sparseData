{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds #-}


module DelayedBlas.Parallel.Sparse.CSR where

import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.CSR as R  
import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.COO as O 
import DelayedBlas.Parallel.Sparse.COO 

import Test.QuickCheck
    ( quickCheckWith, stdArgs, Arbitrary(arbitrary), Args(maxSuccess) )
import DelayedBlas.Parallel.Generic.Generic
    ( assocConstMulProp,
      commutAddProp,
      distrConstMulProp,
      assocAddProp,
      assocMultVecProp,
      distrAddMultVecProp,
      scalarVecTransform,
      convertTest )
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Undelay, Matrix(MatrixData), RepIndex(D, U), manifestConvert )
import qualified Data.Vector.Unboxed as UVector



instance (Arbitrary (MatrixData O.COO U n1 n2 e), Undelay R.CSR n1 n2 e) 
         => Arbitrary (MatrixData R.CSR U n1 n2 e) where 
             arbitrary = do 
                 (arr :: MatrixData O.COO U n1 n2 e) <- arbitrary 
                 let (to_return :: MatrixData R.CSR U n1 n2 e) = manifestConvert arr 
                 return to_return




test_csr :: IO () 
test_csr = do 
 print "testing CSR ... \n\n"
 print "undelay . delay = id : \n"
 quickCheckWith (stdArgs {maxSuccess=1000}) 
                (convertTest :: MatrixData R.CSR U 100 100 Double -> Bool)

 print " r (s A) == (r s) A: \n"
 quickCheckWith (stdArgs {maxSuccess=1000}) 
                (assocConstMulProp :: Double 
                                -> Double 
                                -> MatrixData R.CSR D 100 100 Double -> Bool)

 print "(A + B) == (B + A): \n"
 quickCheckWith (stdArgs {maxSuccess=1000}) 
                (commutAddProp :: MatrixData R.CSR D 100 100 Double 
                                    -> MatrixData R.CSR D 100 100 Double -> Bool)

 print "r (A + B) = r A + r B: \n"
 quickCheckWith (stdArgs {maxSuccess=1000}) 
                (distrConstMulProp :: Double 
                                -> MatrixData R.CSR D 100 100 Double 
                                -> MatrixData R.CSR D 100 100 Double -> Bool)

 print "(A + B) + C == A + (B + C): \n"
 quickCheckWith (stdArgs {maxSuccess=1000}) 
                (assocAddProp :: MatrixData R.CSR D 100 100 Double 
                                  -> MatrixData R.CSR D 100 100 Double 
                                  -> MatrixData R.CSR D 100 100 Double -> Bool)

 print "A (w + v) = A w + A v : \n"
 quickCheckWith (stdArgs {maxSuccess=1000}) 
               (assocMultVecProp :: UVector.Vector Double 
                                  -> UVector.Vector Double 
                                  -> MatrixData R.CSR D 100 100 Double -> Bool)

 print "(A + B) v = A v + B v : \n"
 quickCheckWith (stdArgs {maxSuccess=1000}) 
                (distrAddMultVecProp :: UVector.Vector Double 
                                  -> MatrixData R.CSR D 100 100 Double 
                                  -> MatrixData R.CSR D 100 100 Double -> Bool)

 print "A (a  u) = a  (A . u): \n"
 quickCheckWith (stdArgs {maxSuccess=1000}) 
                (scalarVecTransform :: Int
                                  -> UVector.Vector Int 
                                  -> MatrixData R.CSR D 100 100 Int -> Bool)
 return ()