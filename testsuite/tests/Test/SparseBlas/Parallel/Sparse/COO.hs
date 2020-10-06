{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module SparseBlas.Parallel.Sparse.COO where

import qualified SparseBlas.Data.Matrix.Parallel.Sparse.COO as O  

import Test.QuickCheck
    ( choose,
      suchThat,
      quickCheckWith,
      stdArgs,
      Arbitrary(arbitrary),
      Args(maxSuccess) )
import SparseBlas.Parallel.Generic.Generic
    ( arbitraryVector,
      s_assoc_const_mul_prop,
      s_commut_add_prop,
      s_distr_const_mul_prop,
      s_assoc_add_prop,
      s_assoc_mult_vec_prop,
      s_distr_add_mult_vec_prop,
      s_scalar_vec_transform,
      s_convert_test ) 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic
    ( Sparse(SparseData), RepIndex(D, U) ) 
import qualified Data.Vector as UVector
import Control.Parallel.Strategies 
import Data.Vector.Strategies 
import qualified Data.Set as S
import Control.Monad ( replicateM ) 
import Data.List (sort)

instance (Arbitrary a, NFData a, Num a, Eq a, Ord a) 
         => Arbitrary (SparseData O.COO U a) where 
    arbitrary = do  
        (len :: Int) <- arbitrary
        (n   :: Int) <- arbitrary `suchThat` (< 1000)
        let 
            !def_height = n 
            !def_width  = n 
        !(heights' :: [Int])  <- replicateM len (choose (0, def_height - 1)) 
        !(widths'  :: [Int])  <- replicateM len (choose (0, def_width - 1))
        let (!heights, !widths) = UVector.unzip 
                                     $ UVector.fromList $ sort 
                                     $ S.toList $ S.fromList 
                                     $ zip heights' widths' 
        (uvec :: UVector.Vector a) <- arbitraryVector `suchThat` 
                                         (\v -> 
                                            (UVector.length $ UVector.filter (/= 0) v) 
                                            == UVector.length widths)
        !to_return <- UVector.imapM (\i x -> do 
                      let 
                       y1 = heights UVector.!? i 
                       z1 = widths UVector.!? i 
                      case (y1, z1) of 
                       (Just y, Just z)  -> return (x, y, z) 
                       (Just _, Nothing) -> error "out of bounds in widths"
                       (Nothing, Just _) -> error "out of bounds in heights"
                       (Nothing, Nothing)-> error "width & height out of bound" ) uvec 
        return $ (O.COO to_return n n)

test_coo :: IO () 
test_coo = do 
    print "testing COO ... \n\n"

    print "undelay . delay = id : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_convert_test :: SparseData O.COO U Double -> Bool)

    print " r (s A) == (r s) A: \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_assoc_const_mul_prop :: Double 
                                    -> Double 
                                    -> SparseData O.COO D Double -> Bool)

    print "(A + B) == (B + A): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_commut_add_prop :: SparseData O.COO D Double 
                                        -> SparseData O.COO D Double -> Bool)

    print "r (A + B) = r A + r B: \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_distr_const_mul_prop :: Double 
                                    -> SparseData O.COO D Double 
                                    -> SparseData O.COO D Double -> Bool)

    print "(A + B) + C == A + (B + C): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_assoc_add_prop :: SparseData O.COO D Double 
                                    -> SparseData O.COO D Double 
                                    -> SparseData O.COO D Double -> Bool)

    print "A (w + v) = A w + A v : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                (s_assoc_mult_vec_prop :: UVector.Vector Double 
                                    -> UVector.Vector Double 
                                    -> SparseData O.COO D Double -> Bool)

    print "(A + B) v = A v + B v : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_distr_add_mult_vec_prop :: UVector.Vector Double 
                                    -> SparseData O.COO D Double 
                                    -> SparseData O.COO D Double -> Bool)

    print "A (a  u) = a  (A . u): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (s_scalar_vec_transform :: Int
                                    -> UVector.Vector Int 
                                    -> SparseData O.COO D Int -> Bool)
    return ()