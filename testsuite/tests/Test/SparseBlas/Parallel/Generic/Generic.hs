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
    ( Undelay(s_undelay),
      Sparse((#.), SparseData, s_dims),
      RepIndex(D, U),
      to_vector,
      from_vector,
      vnull,
      vmap,
      vzipWith,
      veq,
      delay,
      empty,
      (#+),
      scale ) 
import Data.List (sort)


instance (NFData a, Arbitrary a, Ord a, Num a, UVector.Unbox a) 
         => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink    = shrinkVector


arbitraryVector :: (GVector.Vector v a, Arbitrary a, Ord a, Num a) 
                => Gen (v a)
arbitraryVector = fmap (\l -> GVector.fromList $ sort (filter (>0) l)) arbitrary


                      
shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList



---------------------------------------------------- Delayed --------------------------------------------------------

instance (Arbitrary (SparseData ty U e), Sparse ty U e)
         => Arbitrary (SparseData ty D e) where 
    arbitrary = do 
        (arr :: SparseData ty U e) <- arbitrary
        let (interm :: SparseData ty D e) = delay arr 
        return interm  

       
-- ---------------------------------------------------------------------------------------------------------------
-- --------------------------------- Properties ------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------





-- -- generate random sparse matrix of given size 
-- -- generate 2 random scalars (integers) r and s
-- -- check whether r (s A) = (r s) A 
s_assoc_const_mul_prop :: (Eq a
                         , Ord a
                         , NFData a
                         , Sparse rep D a
                         , Num a, Undelay rep a 
                         , Eq (SparseData rep D a)) 
                       => a -> a -> SparseData rep D a -> Bool  
s_assoc_const_mul_prop r s a_mat = 
    if empty a_mat then True 
    else 
      if and [(r `scale` (s `scale` a_mat))
               == ((r * s) `scale` a_mat)
              , not $ empty a_mat]
        then True 
        else False 


-- -- generate random sparse matrix of given size
-- -- generate second sparse matrix of same size
-- -- check if (A + B) == (B + A)
s_commut_add_prop :: (Eq a
                    , Ord a
                    , NFData a
                    , Sparse rep D a
                    , Num a, Undelay rep a 
                    , Eq (SparseData rep D a)) 
                  => SparseData rep D a  -> SparseData rep D a  -> Bool  
s_commut_add_prop a_mat b_mat = 
    let 
        (a_w, a_h) = s_dims a_mat 
        (b_w, b_h) = s_dims b_mat 
    in 
     if or [a_w /= b_w
          , a_h /= b_h
          , empty a_mat
          , empty b_mat] 
     then True 
     else  
        if (a_mat #+ b_mat) == (b_mat #+ a_mat)
            then True 
            else False 

-- -- generate random scalar r 
-- -- generate random sparse matrices A and B 
-- -- check whether r (A + B) = r A + r B 
s_distr_const_mul_prop :: (Eq a
                         , Ord a
                         , NFData a
                         , Sparse rep D a
                         , Num a
                         , Eq (SparseData rep D a)) 
                        => a -> SparseData rep D a  
                             -> SparseData rep D a  -> Bool  
s_distr_const_mul_prop r a_mat b_mat = 
    let 
        (a_w, a_h) = s_dims a_mat
        (b_w, b_h) = s_dims b_mat 
    in if a_w /= b_w || a_h /= b_h then True 
    else 
        if r `scale` (a_mat #+ b_mat) == (r `scale` a_mat) #+ (r `scale` b_mat)
            then True 
            else False

-- -- generate a random sparse matrix of given size
-- -- generate another sparse matrix with same length as above
-- -- generate a third
-- -- check if (A + B) + C == A + (B + C) 
s_assoc_add_prop ::  (Eq a
                    , Ord a
                    , NFData a
                    , Sparse rep D a
                    , Num a
                    , Eq (SparseData rep D a)) 
                 => SparseData rep D a  
                 -> SparseData rep D a  -> SparseData rep D a  -> Bool 
s_assoc_add_prop a_mat b_mat c_mat = 
    let 
        (a_w, a_h) = s_dims a_mat
        (b_w, b_h) = s_dims b_mat 
        (c_w, c_h) = s_dims c_mat 
    in if not $ and [a_w == b_w, a_w == c_w, a_h == b_h, a_h == c_h] then True
    else 
        if (a_mat #+ b_mat) #+ c_mat == a_mat #+ (b_mat #+ c_mat) 
                then True 
                else False 
 


-------- Linear transformation properties
-- 1. Transform vectors on left
-- -- generate random sparse matrix A 
-- -- generate random vectors w and v 
-- -- check whether A (w + v) = A w + A v 
s_assoc_mult_vec_prop :: (Eq a
                        , Ord a
                        , NFData a
                        , Sparse rep D a
                        , Num a, Undelay rep a 
                        , Eq (SparseData rep D a)) 
                      => UVector.Vector a 
                      -> UVector.Vector a -> SparseData rep D a -> Bool  
s_assoc_mult_vec_prop  w_vec' v_vec' a_mat = 
        if or [len_w /= len_v
             , len_w /= w
             , vnull w_vec || vnull v_vec || all_zeros w_vec' || all_zeros v_vec' 
             , empty a_mat] then True 
        else (a_mat #. (w_vec ^+^ v_vec)) 
             `veq` ((a_mat #. w_vec) ^+^ (a_mat #. v_vec))
    where   
        w_vec  = from_vector $ UVector.map (\_ -> fromInteger 1) w_vec'
        v_vec  = from_vector $ UVector.map (\_ -> fromInteger 1) v_vec'
        all_zeros v = UVector.foldr (\a b -> (a == 0) && b) True v 
        (^+^)  = vzipWith (+)
        (w, h) = s_dims a_mat
        len_w  = snd w_vec 
        len_v  = snd v_vec 


-- 2. Transform vectors on right (sum of linear tranformations is a linear transformatioin)
-- -- generate random sparse matrices A and B 
-- -- generate random vector v 
-- -- check whether (A + B) v = A v + B v 
s_distr_add_mult_vec_prop :: (Eq a
                            , Ord a
                            , NFData a
                            , Sparse rep D a
                            , Num a, Undelay rep a 
                            , Eq (SparseData rep D a)) 
                          => UVector.Vector a 
                          -> SparseData rep D a  
                          -> SparseData rep D a  -> Bool  
s_distr_add_mult_vec_prop v_vec' a_mat b_mat = 
    let 
        (a_w, a_h) = s_dims a_mat 
        (b_w, b_h) = s_dims b_mat 
        len        = snd v_vec
    in if or [a_w /= b_w
            , a_h /= b_h
            , a_w /= len
            , empty a_mat
            , empty b_mat] then True
       else 
        if ((a_mat #+ b_mat) #. v_vec) 
           `veq` ((a_mat #. v_vec) 
           `addVecs` (b_mat #. v_vec)) 
        then True 
        else False 
    where 
        addVecs !v1 !v2 = vzipWith (+) v1 v2 
        v_vec = from_vector v_vec'

    
-- 3. transform scalar vector multiplication
s_scalar_vec_transform :: (Eq a
                         , Ord a
                         , NFData a
                         , Sparse rep D a
                         , Num a, Undelay rep a 
                         , Eq (SparseData rep D a)) 
                       => a -> UVector.Vector a -> SparseData rep D a -> Bool
s_scalar_vec_transform alpha u t_mat = 
    let 
        vec@(_, len)         = from_vector u 
        s_vec                = from_vector $ UVector.map (*alpha) u
        (t_w, t_h)           = s_dims t_mat 
    in if or [empty t_mat, t_w /= len] then True 
       else (t_mat #. s_vec) `veq` (alpha `s_scale` (t_mat #. vec))
    where
        s_scale alpha = vmap (*alpha)   
        
        

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
s_convert_test :: (Eq (SparseData rep U a), Undelay rep a) => SparseData rep U a  -> Bool
s_convert_test arr = 
    let 
        arr'    = s_undelay $ delay arr 
    in arr' == arr 


s_vec_test :: (Eq a, Num a, NFData a, UVector.Unbox a) => UVector.Vector a  -> Bool
s_vec_test vec = 
    let 
        un_vec = from_vector vec 
    in (to_vector un_vec) == vec 