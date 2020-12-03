{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module DelayedBlas.Parallel.Sparse.COO where

import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.COO as O  

import Test.QuickCheck
    ( choose,
      suchThat,
      quickCheckWith,
      stdArgs,
      Arbitrary(arbitrary),
      Args(maxSuccess))
import DelayedBlas.Parallel.Generic.Generic 
    ( arbitraryVector, 
      convertTest,
      assocConstMulProp, 
      commutAddProp, 
      distrConstMulProp, 
      assocAddProp, 
      assocMultVecProp, 
      distrAddMultVecProp, 
      scalarVecTransform)
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Matrix(MatrixData), RepIndex(D, U) ) 
import qualified Data.Vector.Unboxed as UVector
import Control.Parallel.Strategies ( NFData ) 
import qualified Data.Set as S
import Control.Monad ( replicateM ) 
import Data.List (sort)
import GHC.TypeLits ( KnownNat, natVal, someNatVal ) 
import Data.Proxy ( Proxy(..) ) 

instance (KnownNat n, Arbitrary a, NFData a, Num a, Eq a, Ord a, UVector.Unbox a) 
         => Arbitrary (MatrixData O.COO U n n a) where 
    arbitrary = do  
        let (len :: Int) = fromIntegral $ natVal (Proxy :: Proxy n)
        (n1   :: Int) <- arbitrary `suchThat` (< 1000)
        let 
            !def_height = n1 
            !def_width  = n1
        !(heights' :: [Int])  <- replicateM len (choose (0, def_height - 1)) 
        !(widths'  :: [Int])  <- replicateM len (choose (0, def_width - 1))
        let (!heights, !widths) = UVector.unzip 
                                     $ UVector.fromList $ sort 
                                     $ S.toList $ S.fromList 
                                     $ zip heights' widths' 
        (uvec :: UVector.Vector a) <- arbitraryVector `suchThat` 
                                         (\v -> 
                                            UVector.length (UVector.filter (/= 0) v) 
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
        case someNatVal (toInteger len) of 
            Nothing -> error "quickcheck size mismatch freak out!"
            Just h  -> return $ (O.COO to_return :: MatrixData O.COO U h h a)
        

test_coo :: IO () 
test_coo = do 
    print "testing COO ... \n\n"

    print "undelay . delay = id : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (convertTest :: MatrixData O.COO U 100 100 Double -> Bool)

    print " r (s A) == (r s) A: \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (assocConstMulProp :: Double 
                                    -> Double 
                                    -> MatrixData O.COO D 100 100 Double -> Bool)

    print "(A + B) == (B + A): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (commutAddProp :: MatrixData O.COO D 100 100 Double 
                                        -> MatrixData O.COO D 100 100 Double -> Bool)

    print "r (A + B) = r A + r B: \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (distrConstMulProp :: Double 
                                    -> MatrixData O.COO D 100 100 Double 
                                    -> MatrixData O.COO D 100 100 Double -> Bool)

    print "(A + B) + C == A + (B + C): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (assocAddProp :: MatrixData O.COO D 100 100 Double 
                                    -> MatrixData O.COO D 100 100 Double 
                                    -> MatrixData O.COO D 100 100 Double -> Bool)

    print "A (w + v) = A w + A v : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                (assocMultVecProp :: UVector.Vector Double 
                                    -> UVector.Vector Double 
                                    -> MatrixData O.COO D 100 100 Double -> Bool)

    print "(A + B) v = A v + B v : \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (distrAddMultVecProp :: UVector.Vector Double 
                                    -> MatrixData O.COO D 100 100 Double 
                                    -> MatrixData O.COO D 100 100 Double -> Bool)

    print "A (a  u) = a  (A . u): \n"
    quickCheckWith (stdArgs {maxSuccess=1000}) 
                    (scalarVecTransform :: Int
                                    -> UVector.Vector Int 
                                    -> MatrixData O.COO D 100 100 Int -> Bool)
    return ()