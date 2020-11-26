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

module SparseBlas.Parallel.Generic.Generic where 


import Test.QuickCheck ( Arbitrary(..), Gen ) 
import Test.QuickCheck.Property 
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Generic as GVector
import qualified Data.Set as S 
import Control.Parallel.Strategies 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic
    -- ( Undelay(s_undelay),
    --   Sparse((#.), SparseData, s_dims),
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

instance (Arbitrary (SparseData ty U n1 n2 e), Sparse ty U n1 n2 e)
         => Arbitrary (SparseData ty D n1 n2 e) where 
    arbitrary = do 
        (arr :: SparseData ty U n1 n2 e) <- arbitrary
        let (interm :: SparseData ty D n1 n2 e) = delay arr 
        return interm  

       
-- -- ---------------------------------------------------------------------------------------------------------------
-- -- --------------------------------- Properties ------------------------------------------------------------------
-- -- ---------------------------------------------------------------------------------------------------------------





-- -- generate random sparse matrix of given size 
-- -- generate 2 random scalars (integers) r and s
-- -- check whether r (s A) = (r s) A 
s_assoc_const_mul_prop :: (Sparse rep D n1 n2 a
                         , Eq (SparseData rep D n1 n2 a)) 
                       => a -> a -> SparseData rep D n1 n2 a -> Bool  
s_assoc_const_mul_prop r s a_mat = (r `scale` (s `scale` a_mat)) == ((r * s) `scale` a_mat)  -- removed empty case



-- -- generate random sparse matrix of given size
-- -- generate second sparse matrix of same size
-- -- check if (A + B) == (B + A)
s_commut_add_prop :: (Sparse rep D n1 n2 a, Eq (SparseData rep D n1 n2 a )) 
                  => SparseData rep D n1 n2 a  -> SparseData rep D n1 n2 a  -> Bool  
s_commut_add_prop a_mat b_mat = (a_mat #+ b_mat) == (b_mat #+ a_mat)


-- -- generate random scalar r 
-- -- generate random sparse matrices A and B 
-- -- check whether r (A + B) = r A + r B 
s_distr_const_mul_prop :: (Sparse rep D n1 n2 a
                         , Eq (SparseData rep D n1 n2 a)) 
                        => a -> SparseData rep D n1 n2 a  
                             -> SparseData rep D n1 n2 a  -> Bool  
s_distr_const_mul_prop r a_mat b_mat = r `scale` (a_mat #+ b_mat) == (r `scale` a_mat) #+ (r `scale` b_mat)

-- -- generate a random sparse matrix of given size
-- -- generate another sparse matrix with same length as above
-- -- generate a third
-- -- check if (A + B) + C == A + (B + C) 
s_assoc_add_prop ::  (Sparse rep D n1 n2 a
                    , Eq (SparseData rep D n1 n2 a)) 
                 => SparseData rep D n1 n2 a  
                 -> SparseData rep D n1 n2 a  -> SparseData rep D n1 n2 a  -> Bool 
s_assoc_add_prop a_mat b_mat c_mat = (a_mat #+ b_mat) #+ c_mat == a_mat #+ (b_mat #+ c_mat) 
 


-------- Linear transformation properties 
-- 1. Transform vectors on left : only hold for square matrices, otherwise type error?
-- -- generate random sparse matrix A 
-- -- generate random vectors w and v 
-- -- check whether A (w + v) = A w + A v 
s_assoc_mult_vec_prop :: (Sparse rep D n n a 
                        , Eq (SparseData rep D n n a)) 
                      => UVector.Vector a 
                      -> UVector.Vector a -> SparseData rep D n n a -> Bool  
s_assoc_mult_vec_prop  w_vec' v_vec' a_mat = case (from_vector $ UVector.map (\_ -> fromInteger 1) w_vec'
                                                  , from_vector $ UVector.map (\_ -> fromInteger 1) v_vec') of 
                                              (Just w_vec, Just v_vec) -> (a_mat #. (w_vec ^+^ v_vec)) `veq` ((a_mat #. w_vec) ^+^ (a_mat #. v_vec))
                                              _                        -> True 
    where   
        (^+^)  = vzipWith (+)

-- 2. Transform vectors on right (sum of linear tranformations is a linear transformatioin)
-- -- generate random sparse matrices A and B 
-- -- generate random vector v 
-- -- check whether (A + B) v = A v + B v 
s_distr_add_mult_vec_prop :: (Sparse rep D n1 n2 a
                            , Eq (SparseData rep D n1 n2 a)) 
                          => UVector.Vector a 
                          -> SparseData rep D n1 n2 a  
                          -> SparseData rep D n1 n2 a  -> Bool  
s_distr_add_mult_vec_prop v_vec' a_mat b_mat = case from_vector v_vec' of 
                                                Just v_vec -> ((a_mat #+ b_mat) #. v_vec) 
                                                                    `veq` ((a_mat #. v_vec) 
                                                                            !+! (b_mat #. v_vec)) 
                                                Nothing    -> True 

    
-- 3. transform scalar vector multiplication
s_scalar_vec_transform :: (Sparse rep D n1 n2 a
                         , Eq (SparseData rep D n1 n2 a)) 
                       => a -> UVector.Vector a -> SparseData rep D n1 n2 a -> Bool
s_scalar_vec_transform alpha u t_mat =  case (from_vector u, from_vector $ UVector.map (*alpha) u) of 
                                          (Just vec, Just s_vec) -> (t_mat #. s_vec) `veq` (alpha `s_scale` (t_mat #. vec))
                                          _                      -> True 
    where
        s_scale alpha  = vmap (*alpha)   
        
        

-- 4. linear transformation composition
-- s_mult_mult_vec :: (Eq a
--                  , Ord a
--                  , NFData a
--                  , Sparse rep D a
--                  , Num a
--                  , Eq (SparseData rep D a)) 
--                => UVector.Vector a 
--                -> SparseData rep D a -> SparseData rep D a -> Bool 
-- s_mult_mult_vec vec a_mat b_mat = 
--    let 
--        (a_w, a_h)  = s_dims a_mat
--        (b_w, b_h)  = s_dims b_mat 
--        s_vec@(func, len) = from_vector vec 
--    in 
--        if or [a_h /= b_w
--             , vnull a_mat
--             , vnull b_mat
--             , b_w /= len] then True 
--        else ((a_mat #* b_mat) #. s_vec) 
--             `equals_i` (a_mat #. (b_mat #. s_vec))




-- testing conversions
s_convert_test :: (Eq (SparseData rep U n1 n2 a), Undelay rep n1 n2 a) => SparseData rep U n1 n2 a  -> Bool
s_convert_test arr = arr' == arr
    where 
        arr'    = s_undelay $ delay arr 


s_vec_test :: forall a n. (KnownNat n, Eq a, Num a, NFData a, UVector.Unbox a) => UVector.Vector a  -> Bool
s_vec_test vec = ans 
    where 
        ans = case from_vector vec of 
                    Just v  -> (to_vector (v :: SVector n a)) == vec
                    Nothing -> True 