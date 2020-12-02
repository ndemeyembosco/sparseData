{-# LANGUAGE  TypeOperators, 
              FlexibleInstances, 
              BangPatterns, 
              FlexibleContexts, 
              ScopedTypeVariables, 
              GADTs, 
              RankNTypes,
              DataKinds, 
              AllowAmbiguousTypes, 
              UndecidableInstances #-}

module DelayedBlas.Parallel.Generic.Generic where 


import Test.QuickCheck ( Arbitrary(..), Gen ) 
import Test.QuickCheck.Property 
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Generic as GVector
import qualified Data.Set as S 
import Control.Parallel.Strategies 
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    -- ( Undelay(s_undelay),
    --   Matrix((#.), SparseData, s_dims),
    --   RepIndex(D, U),
    --   to_vector,
    --   from_vector,
    --   vnull,
    --   vmap,
    --   vzipWith,
    --   veq,
    --   delay,
    --   empty,
    --   (#+),
    --   scale ) 
import Data.List (sort)
import GHC.TypeLits 
import Data.Proxy 
import Data.Maybe (fromJust)


instance (NFData a, Arbitrary a, Ord a, Num a, UVector.Unbox a) 
         => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink    = shrinkVector


arbitraryVector :: (GVector.Vector v a, Arbitrary a, Ord a, Num a) 
                => Gen (v a)
arbitraryVector = fmap (\l -> GVector.fromList $ sort (filter (>0) l)) arbitrary


                      
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
s_assoc_const_mul_prop :: (Matrix rep D n1 n2 a
                         , Eq (MatrixData rep D n1 n2 a)) 
                       => a -> a -> MatrixData rep D n1 n2 a -> Bool  
s_assoc_const_mul_prop r s a_mat = (r `scale` (s `scale` a_mat)) == ((r * s) `scale` a_mat)  -- removed empty case



-- -- generate random Matrix matrix of given size
-- -- generate second Matrix matrix of same size
-- -- check if (A + B) == (B + A)
s_commut_add_prop :: (Matrix rep D n1 n2 a, Eq (MatrixData rep D n1 n2 a )) 
                  => MatrixData rep D n1 n2 a  -> MatrixData rep D n1 n2 a  -> Bool  
s_commut_add_prop a_mat b_mat = (a_mat #+ b_mat) == (b_mat #+ a_mat)


-- -- generate random scalar r 
-- -- generate random Matrix matrices A and B 
-- -- check whether r (A + B) = r A + r B 
s_distr_const_mul_prop :: (Matrix rep D n1 n2 a
                         , Eq (MatrixData rep D n1 n2 a)) 
                        => a -> MatrixData rep D n1 n2 a  
                             -> MatrixData rep D n1 n2 a  -> Bool  
s_distr_const_mul_prop r a_mat b_mat = r `scale` (a_mat #+ b_mat) == (r `scale` a_mat) #+ (r `scale` b_mat)

-- -- generate a random Matrix matrix of given size
-- -- generate another Matrix matrix with same length as above
-- -- generate a third
-- -- check if (A + B) + C == A + (B + C) 
s_assoc_add_prop ::  (Matrix rep D n1 n2 a
                    , Eq (MatrixData rep D n1 n2 a)) 
                 => MatrixData rep D n1 n2 a  
                 -> MatrixData rep D n1 n2 a  -> MatrixData rep D n1 n2 a  -> Bool 
s_assoc_add_prop a_mat b_mat c_mat = (a_mat #+ b_mat) #+ c_mat == a_mat #+ (b_mat #+ c_mat) 
 


-------- Linear transformation properties 
-- 1. Transform vectors on left : only hold for square matrices, otherwise type error?
-- -- generate random Matrix matrix A 
-- -- generate random vectors w and v 
-- -- check whether A (w + v) = A w + A v 
s_assoc_mult_vec_prop :: (Matrix rep D n n a 
                        , Eq (MatrixData rep D n n a)) 
                      => UVector.Vector a 
                      -> UVector.Vector a -> MatrixData rep D n n a -> Bool  
s_assoc_mult_vec_prop  w_vec' v_vec' a_mat = case (from_vector $ UVector.map (\_ -> fromInteger 1) w_vec'
                                                  , from_vector $ UVector.map (\_ -> fromInteger 1) v_vec') of 
                                              (w_vec, v_vec) -> (a_mat #. (w_vec ^+^ v_vec)) `veq` ((a_mat #. w_vec) ^+^ (a_mat #. v_vec))
    where   
        (^+^)  = vzipWith (+)

-- 2. Transform vectors on right (sum of linear tranformations is a linear transformatioin)
-- -- generate random Matrix matrices A and B 
-- -- generate random vector v 
-- -- check whether (A + B) v = A v + B v 
s_distr_add_mult_vec_prop :: (Matrix rep D n1 n2 a
                            , Eq (MatrixData rep D n1 n2 a)) 
                          => UVector.Vector a 
                          -> MatrixData rep D n1 n2 a  
                          -> MatrixData rep D n1 n2 a  -> Bool  
s_distr_add_mult_vec_prop v_vec' a_mat b_mat = case from_vector v_vec' of 
                                                v_vec -> ((a_mat #+ b_mat) #. v_vec) 
                                                                    `veq` ((a_mat #. v_vec) 
                                                                            !+! (b_mat #. v_vec)) 

    
-- 3. transform scalar vector multiplication
s_scalar_vec_transform :: (Matrix rep D n1 n2 a
                         , Eq (MatrixData rep D n1 n2 a)) 
                       => a -> UVector.Vector a -> MatrixData rep D n1 n2 a -> Bool
s_scalar_vec_transform alpha u t_mat =  case (from_vector u, from_vector $ UVector.map (*alpha) u) of 
                                          (vec, s_vec) -> (t_mat #. s_vec) `veq` (alpha `s_scale` (t_mat #. vec))
    where
        s_scale alpha  = vmap (*alpha)   
        
        

-- 4. matrix-matrix left distributivity 
-- A (B + C) = AB + AC 
s_mult_mult_ldistr :: (Matrix rep D n1 n2 a, Matrix rep D n2 n3 a 
                    , Eq (MatrixData rep D n1 n3 a)) 
                    => MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> MatrixData rep D n2 n3 a -> Bool 
s_mult_mult_ldistr a_mat b_mat c_mat =  a_mat #* (b_mat #+ c_mat) == ((a_mat #* b_mat) #+ (a_mat #* c_mat)) 


-- 5. matrix-matrix right distributivity 
-- (B + C) D = BD + CD 
s_mult_mult_rdistr :: (Matrix rep D n1 n2 a, Matrix rep D n2 n3 a 
                        , Eq (MatrixData rep D n1 n3 a)) 
                    => MatrixData rep D n1 n2 a -> MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> Bool 
s_mult_mult_rdistr b_mat c_mat d_mat =  (b_mat #+ c_mat) #* d_mat == (b_mat #* d_mat) #+ (c_mat #* d_mat)


-- 6. matrix-matrix left scalar product 
-- c(AB) = (cA)B 
s_mult_mult_lscalar :: (Matrix rep D n1 n2 a, Matrix rep D n2 n3 a 
                        , Eq (MatrixData rep D n1 n3 a)) 
                    => a -> MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> Bool 
s_mult_mult_lscalar c a_mat b_mat = c `scale` (a_mat #* b_mat) == ((c `scale` a_mat) #* b_mat)



-- 7. matrix-matrix right scalar product 
-- (AB)c = A(Bc) 
s_mult_mult_rscalar :: (Matrix rep D n1 n2 a, Matrix rep D n2 n3 a 
                        , Eq (MatrixData rep D n1 n3 a)) 
                    => a -> MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> Bool 
s_mult_mult_rscalar c a_mat b_mat = (a_mat #* b_mat) `fscale` c == a_mat #* (b_mat `fscale` c) 
     where 
         fscale :: (Matrix r ty n3 n4 a, Num a) => MatrixData r ty n3 n4 a -> a -> MatrixData r D n3 n4 a 
         fscale = flip scale 
        

-- 8. transpose of matrix-matrix  
-- (AB)^t = B^t A^t  
s_mult_mult_trans :: (Matrix rep D n1 n2 a, 
                      Matrix rep D n2 n3 a, 
                      Eq (MatrixData rep D n3 n1 a)) 
                  => MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> Bool 
s_mult_mult_trans a_mat b_mat = transpose (a_mat #* b_mat) == ((transpose b_mat) #* (transpose a_mat))



-- 9. associativity of matrix-matrix  
-- (AB)C = A(BC)  
s_mult_mult_assoc :: (Matrix rep D n1 n2 a
                      , Matrix rep D n2 n3 a 
                      , Matrix rep D n3 n4 a 
                    , Eq (MatrixData rep D n1 n4 a)) 
                 => MatrixData rep D n1 n2 a -> MatrixData rep D n2 n3 a -> MatrixData rep D n3 n4 a -> Bool 
s_mult_mult_assoc a_mat b_mat c_mat = (a_mat #* b_mat) #* c_mat == (a_mat #* (b_mat #* c_mat))




-- testing conversions
s_convert_test :: (Eq (MatrixData rep U n1 n2 a), Undelay rep n1 n2 a) => MatrixData rep U n1 n2 a  -> Bool
s_convert_test arr = arr' == arr
    where 
        arr'    = s_undelay $ delay arr 


s_vec_test :: forall a n. (KnownNat n, Eq a, Num a, NFData a, UVector.Unbox a) => UVector.Vector a  -> Bool
s_vec_test vec = ans 
    where 
        ans = case from_vector vec of 
                    v  -> (to_vector (v :: SVector n a)) == vec