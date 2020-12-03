{-# LANGUAGE  FlexibleInstances, 
              FlexibleContexts, 
              ScopedTypeVariables, 
              GADTs, 
              RankNTypes,
              DataKinds, 
              AllowAmbiguousTypes, 
              UndecidableInstances #-}

module DelayedBlas.Parallel.Generic.Generic where 


import Test.QuickCheck ( Arbitrary(..), Gen ) 
import Test.QuickCheck.Property () 
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Generic as GVector
import Control.Parallel.Strategies ( NFData ) 
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Undelay(s_undelay),
      Matrix((#.), (#*), MatrixData),
      SVector,
      RepIndex(D, U),
      vmap,
      vzipWith,
      (!+!),
      veq,
      delay,
      transpose,
      (#+),
      scale,
      fromVector,
      toVector )
import Data.List (sort)
import GHC.TypeLits ( KnownNat ) 
import Data.Proxy () 


instance (NFData a, Arbitrary a, Ord a, Num a, UVector.Unbox a) 
         => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink    = shrinkVector


arbitraryVector :: (GVector.Vector v a, Arbitrary a, Ord a, Num a) 
                => Gen (v a)
arbitraryVector = fmap (GVector.fromList . sort . filter (>0)) arbitrary


                      
shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList



-- ---------------------------------------------------- Delayed --------------------------------------------------------

instance (Arbitrary (MatrixData ty U n1 n2 e), Matrix ty U n1 n2 e)
         => Arbitrary (MatrixData ty D n1 n2 e) where 
    arbitrary = do 
        (arr :: MatrixData ty U n1 n2 e) <- arbitrary
        let (interm :: MatrixData ty D n1 n2 e) = delay arr 
        return interm  

       
-- -- ---------------------------------------------------------------------------------------------------------------
-- -- --------------------------------- Properties ------------------------------------------------------------------
-- -- ---------------------------------------------------------------------------------------------------------------





-- -- generate random Matrix matrix of given size 
-- -- generate 2 random scalars (integers) r and s
-- -- check whether r (s A) = (r s) A 
assocConstMulProp :: (Matrix rep D n1 n2 a
                         , Eq (MatrixData rep D n1 n2 a)) 
                       => a -> a -> MatrixData rep D n1 n2 a -> Bool  
assocConstMulProp r s a_mat = (r `scale` (s `scale` a_mat)) == ((r * s) `scale` a_mat)  -- removed empty case



-- -- generate random Matrix matrix of given size
-- -- generate second Matrix matrix of same size
-- -- check if (A + B) == (B + A)
commutAddProp :: (Matrix rep D n1 n2 a, Eq (MatrixData rep D n1 n2 a )) 
                  => MatrixData rep D n1 n2 a  -> MatrixData rep D n1 n2 a  -> Bool  
commutAddProp a_mat b_mat = (a_mat #+ b_mat) == (b_mat #+ a_mat)


-- -- generate random scalar r 
-- -- generate random Matrix matrices A and B 
-- -- check whether r (A + B) = r A + r B 
distrConstMulProp :: (Matrix rep D n1 n2 a
                         , Eq (MatrixData rep D n1 n2 a)) 
                        => a -> MatrixData rep D n1 n2 a  
                             -> MatrixData rep D n1 n2 a  -> Bool  
distrConstMulProp r a_mat b_mat = r `scale` (a_mat #+ b_mat) == (r `scale` a_mat) #+ (r `scale` b_mat)

-- -- generate a random Matrix matrix of given size
-- -- generate another Matrix matrix with same length as above
-- -- generate a third
-- -- check if (A + B) + C == A + (B + C) 
assocAddProp ::  (Matrix rep D n1 n2 a
                    , Eq (MatrixData rep D n1 n2 a)) 
                 => MatrixData rep D n1 n2 a  
                 -> MatrixData rep D n1 n2 a  -> MatrixData rep D n1 n2 a  -> Bool 
assocAddProp a_mat b_mat c_mat = (a_mat #+ b_mat) #+ c_mat == a_mat #+ (b_mat #+ c_mat) 
 


-------- Linear transformation properties 
-- 1. Transform vectors on left : only hold for square matrices, otherwise type error?
-- -- generate random Matrix matrix A 
-- -- generate random vectors w and v 
-- -- check whether A (w + v) = A w + A v 
assocMultVecProp :: (Matrix rep D n n a 
                        , Eq (MatrixData rep D n n a)) 
                      => UVector.Vector a 
                      -> UVector.Vector a -> MatrixData rep D n n a -> Bool  
assocMultVecProp  w_vec' v_vec' a_mat = case (fromVector $ UVector.map (const 1) w_vec'
                                                  , fromVector $ UVector.map (const 1) v_vec') of 
                                              (w_vec, v_vec) -> (a_mat #. (w_vec ^+^ v_vec)) `veq` ((a_mat #. w_vec) ^+^ (a_mat #. v_vec))
    where   
        (^+^)  = vzipWith (+)

-- 2. Transform vectors on right (sum of linear tranformations is a linear transformatioin)
-- -- generate random Matrix matrices A and B 
-- -- generate random vector v 
-- -- check whether (A + B) v = A v + B v 
distrAddMultVecProp :: (Matrix rep D n1 n2 a
                            , Eq (MatrixData rep D n1 n2 a)) 
                          => UVector.Vector a 
                          -> MatrixData rep D n1 n2 a  
                          -> MatrixData rep D n1 n2 a  -> Bool  
distrAddMultVecProp v_vec' a_mat b_mat = case fromVector v_vec' of 
                                                v_vec -> ((a_mat #+ b_mat) #. v_vec) 
                                                                    `veq` ((a_mat #. v_vec) 
                                                                            !+! (b_mat #. v_vec)) 

    
-- 3. transform scalar vector multiplication
scalarVecTransform :: (Matrix rep D n1 n2 a
                         , Eq (MatrixData rep D n1 n2 a)) 
                       => a -> UVector.Vector a -> MatrixData rep D n1 n2 a -> Bool
scalarVecTransform alpha u t_mat =  case (fromVector u, fromVector $ UVector.map (*alpha) u) of 
                                          (vec, s_vec) -> (t_mat #. s_vec) `veq` (alpha `s_scale` (t_mat #. vec))
    where
        s_scale alpha  = vmap (*alpha)   
        
        

-- 4. matrix-matrix left distributivity 
-- A (B + C) = AB + AC 
multMultLDistr :: (Matrix rep D n1 n2 a, Matrix rep D n2 n3 a 
                    , Eq (MatrixData rep D n1 n3 a)) 
                    => MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> MatrixData rep D n2 n3 a -> Bool 
multMultLDistr a_mat b_mat c_mat =  a_mat #* (b_mat #+ c_mat) == ((a_mat #* b_mat) #+ (a_mat #* c_mat)) 


-- 5. matrix-matrix right distributivity 
-- (B + C) D = BD + CD 
multMultRDistr :: (Matrix rep D n1 n2 a, Matrix rep D n2 n3 a 
                        , Eq (MatrixData rep D n1 n3 a)) 
                    => MatrixData rep D n1 n2 a -> MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> Bool 
multMultRDistr b_mat c_mat d_mat =  (b_mat #+ c_mat) #* d_mat == (b_mat #* d_mat) #+ (c_mat #* d_mat)


-- 6. matrix-matrix left scalar product 
-- c(AB) = (cA)B 
multMultLScalar :: (Matrix rep D n1 n2 a, Matrix rep D n2 n3 a 
                        , Eq (MatrixData rep D n1 n3 a)) 
                    => a -> MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> Bool 
multMultLScalar c a_mat b_mat = c `scale` (a_mat #* b_mat) == ((c `scale` a_mat) #* b_mat)



-- 7. matrix-matrix right scalar product 
-- (AB)c = A(Bc) 
multMultRScalar :: (Matrix rep D n1 n2 a, Matrix rep D n2 n3 a 
                        , Eq (MatrixData rep D n1 n3 a)) 
                    => a -> MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> Bool 
multMultRScalar c a_mat b_mat = (a_mat #* b_mat) `fscale` c == a_mat #* (b_mat `fscale` c) 
     where 
         fscale :: (Matrix r ty n3 n4 a, Num a) => MatrixData r ty n3 n4 a -> a -> MatrixData r D n3 n4 a 
         fscale = flip scale 
        

-- 8. transpose of matrix-matrix  
-- (AB)^t = B^t A^t  
multMultTrans :: (Matrix rep D n1 n2 a, 
                      Matrix rep D n2 n3 a, 
                      Eq (MatrixData rep D n3 n1 a)) 
                  => MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> Bool 
multMultTrans a_mat b_mat = transpose (a_mat #* b_mat) == ((transpose b_mat) #* (transpose a_mat))



-- 9. associativity of matrix-matrix  
-- (AB)C = A(BC)  
multMultAssoc :: (Matrix rep D n1 n2 a
                      , Matrix rep D n2 n3 a 
                      , Matrix rep D n3 n4 a 
                    , Eq (MatrixData rep D n1 n4 a)) 
                 => MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> MatrixData rep D n3 n4 a -> Bool 
multMultAssoc a_mat b_mat c_mat = (a_mat #* b_mat) #* c_mat == (a_mat #* (b_mat #* c_mat))




-- testing conversions
convertTest :: (Eq (MatrixData rep U n1 n2 a), Undelay rep n1 n2 a) => MatrixData rep U n1 n2 a  -> Bool
convertTest arr = arr' == arr
    where 
        arr'    = s_undelay $ delay arr 


vecTest :: forall a n. (KnownNat n, Eq a, Num a, NFData a, UVector.Unbox a) => UVector.Vector a  -> Bool
vecTest vec = ans 
    where 
        ans = case fromVector vec of 
                    v  -> toVector (v :: SVector n a) == vec