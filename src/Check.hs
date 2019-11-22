{-# LANGUAGE  TypeOperators, FlexibleInstances, BangPatterns, FlexibleContexts 
, ScopedTypeVariables, GADTs, RankNTypes, AllowAmbiguousTypes, RecursiveDo 
, UndecidableInstances #-}



module Check where

import SparseData 
import Test.QuickCheck hiding (scale) 
import Test.QuickCheck.Property 
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Generic as GVector
import qualified Data.Set as S 
import Data.List 
import Control.Monad 


instance (UVector.Unbox a, Arbitrary a, Ord a, Num a) => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector


arbitraryVector :: (GVector.Vector v a, Arbitrary a, Ord a, Num a) => Gen (v a)
arbitraryVector = fmap (\l -> GVector.fromList $ sort (filter (>0) l)) arbitrary


                      
shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList


-------------------------------------------------- Unboxed ------------------------------------------------

instance (Arbitrary a, UVector.Unbox a, Num a, Eq a, Ord a) => Arbitrary (SparseData COO U a) where 
    arbitrary = do  
        (len :: Int) <- arbitrary
        (n :: Int) <- arbitrary `suchThat` (< 100)
        let 
            !def_height = n 
            !def_width  = n 
        !(heights' :: [Int]) <- replicateM len (choose (0, def_height - 1)) 
        !(widths' :: [Int])  <- replicateM len (choose (0, def_width - 1))
        let (!heights, !widths) = UVector.unzip $ UVector.fromList $ sort $ S.toList $ S.fromList $ zip heights' widths' 
        (uvec' :: UVector.Vector a) <- arbitraryVector `suchThat` (\v -> UVector.length v == UVector.length widths)
        let 
            !uvec       = UVector.filter (/= 0) uvec'
            -- !len        = UVector.length uvec
        !to_return <- UVector.imapM (\i x -> do 
                                 let 
                                    y1 = heights UVector.!? i 
                                    z1 = widths UVector.!? i 
                                 case (y1, z1) of 
                                    (Just y, Just z)    -> return (x, y, z) 
                                    (Just _, Nothing)   -> error "out of bounds in widths"
                                    (Nothing, Just _)   -> error "out of bounds in heights"
                                    (Nothing, Nothing)  -> error "out of bounds in both widths, heights") uvec 
        return $ (COO to_return n n)
---------------------------------------------------- Delayed --------------------------------------------------------

instance (Arbitrary a, UVector.Unbox a, Sparse r D a, Eq a, Ord a) => Arbitrary (SparseData r D a) where 
    arbitrary = do 
        (arr :: SparseData COO U a) <- arbitrary
        let (interm :: SparseData r D a) = coo_to_sd arr 
        return interm  

       
-- ---------------------------------------------------------------------------------------------------------------
-- --------------------------------- Properties ------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------





-- -- generate random sparse matrix of given size 
-- -- generate 2 random scalars (integers) r and s
-- -- check whether r (s A) = (r s) A 
s_assoc_const_mul_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep D a, Num a, Eq (SparseData rep D a)) => a -> a -> SparseData rep D a -> Bool  
s_assoc_const_mul_prop r s a_mat = 
    if is_null a_mat then True 
    else 
    if and [(r `scale` (s `scale` a_mat)) == ((r * s) `scale` a_mat), not $ is_null a_mat]
        then True 
        else False 


-- -- generate random sparse matrix of given size
-- -- generate second sparse matrix of same size
-- -- check if (A + B) == (B + A)
s_commut_add_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep D a, Num a, Eq (SparseData rep D a)) => SparseData rep D a  -> SparseData rep D a  -> Bool  
s_commut_add_prop a_mat b_mat = 
    let 
        (a_w, a_h) = (s_width a_mat, s_height a_mat)
        (b_w, b_h) = (s_width b_mat, s_height b_mat)
    in if or [a_w /= b_w, a_h /= b_h, is_null a_mat, is_null b_mat] then True 
    else  
        if (a_mat #+ b_mat) == (b_mat #+ a_mat)
            then True 
            else False 

-- -- generate random scalar r 
-- -- generate random sparse matrices A and B 
-- -- check whether r (A + B) = r A + r B 
s_distr_const_mul_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep D a, Num a, Eq (SparseData rep D a)) => a -> SparseData rep D a  -> SparseData rep D a  -> Bool  
s_distr_const_mul_prop r a_mat b_mat = 
    let 
        (a_w, a_h) = (s_width a_mat, s_height a_mat)
        (b_w, b_h) = (s_width b_mat, s_height b_mat)
    in if a_w /= b_w || a_h /= b_h then True 
    else 
        if r `scale` (a_mat #+ b_mat) == (r `scale` a_mat) #+ (r `scale` b_mat)
            then True 
            else False

-- -- generate a random sparse matrix of given size
-- -- generate another sparse matrix with same length as above
-- -- generate a third
-- -- check if (A + B) + C == A + (B + C) 
s_assoc_add_prop ::  (Eq a, Ord a, UVector.Unbox a, Sparse rep D a, Num a, Eq (SparseData rep D a)) => SparseData rep D a  -> SparseData rep D a  -> SparseData rep D a  -> Bool 
s_assoc_add_prop a_mat b_mat c_mat = 
    let 
        (a_w, a_h) = (s_width a_mat, s_height a_mat)
        (b_w, b_h) = (s_width b_mat, s_height b_mat)
        (c_w, c_h) = (s_width c_mat, s_height c_mat) 
    in if not $ and [a_w == b_w, a_w == c_w, a_h == b_h, a_h == c_h] then True
    else 
        if (a_mat #+ b_mat) #+ c_mat == a_mat #+ (b_mat #+ c_mat) 
                then True 
                else False 
 

-- -- generate random sparse matrix A 
-- -- generate random vectors w and v 
-- -- check whether A (w + v) = A w + A v 
s_assoc_mult_vec_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep D a, Num a, Eq (SparseData rep D a)) => UVector.Vector a -> UVector.Vector a -> SparseData rep D a -> Bool  
s_assoc_mult_vec_prop  w_vec' v_vec' a_mat = 
        if or [len_w /= len_v, len_w /= w, null_i w_vec || null_i v_vec, is_null a_mat] then True 
        else (a_mat #. (w_vec ^+^ v_vec)) `equals_i` ((a_mat #. w_vec) ^+^ (a_mat #. v_vec))
    where   
        w_vec  = from_vector w_vec'
        v_vec  = from_vector v_vec'
        (^+^)  = szipWith_i (+)
        (w, h) = (s_width a_mat, s_height a_mat)
        len_w  = snd w_vec 
        len_v  = snd v_vec 


-- -- generate random sparse matrices A and B 
-- -- generate random vector v 
-- -- check whether (A + B) v = A v + B v 
s_distr_add_mult_vec_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep D a, Num a, Eq (SparseData rep D a)) => UVector.Vector a -> SparseData rep D a  -> SparseData rep D a  -> Bool  
s_distr_add_mult_vec_prop v_vec' a_mat b_mat = 
    let 
        (a_w, a_h) = (s_width a_mat, s_height a_mat)
        (b_w, b_h) = (s_width b_mat, s_height b_mat)
        len        = snd v_vec
    in if or [a_w /= b_w, a_h /= b_h, a_w /= len] then True
    else 
        if ((a_mat #+ b_mat) #. v_vec) `equals_i` ((a_mat #. v_vec) `addVecs` (b_mat #. v_vec)) 
            then True 
            else False 
    where 
        addVecs !v1 !v2 = szipWith_i (+) v1 v2 
        v_vec = from_vector v_vec'




-- testing conversions
s_convert_test :: (Eq (SparseData rep D a), Undelayable r a, Sparse r2 U a, r ~ r2, rep ~ r) => a -> SparseData rep D a  -> Bool
s_convert_test zero arr = 
    let 
        un_arr = undelay zero arr 
        arr'   = delay un_arr 
    in arr' == arr 








