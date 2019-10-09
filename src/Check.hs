{-# LANGUAGE  TypeOperators, FlexibleInstances, BangPatterns, FlexibleContexts 
, ScopedTypeVariables, GADTs, RankNTypes, AllowAmbiguousTypes #-}



module Check where

import SparseData 
import Test.QuickCheck hiding (scale) 
import Test.QuickCheck.Property 
import Test.QuickCheck.Function ((:->))
import Control.Monad.Par 
import Data.Monoid 

import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Generic as GVector
import qualified Data.Set as S 

-- This code is taken directly from quickcheck-instances which failed to install --------------

instance (UVector.Unbox a, CoArbitrary a) => CoArbitrary (UVector.Vector a) where
    coarbitrary = coarbitraryVector

instance (UVector.Unbox a, Function a) => Function (UVector.Vector a) where
    function = functionVector

instance (UVector.Unbox a, Arbitrary a) => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector


arbitraryVector :: (GVector.Vector v a, Arbitrary a) => Gen (v a)
arbitraryVector = GVector.fromList `fmap` arbitrary

shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList

coarbitraryVector :: (GVector.Vector v a, CoArbitrary a) => v a -> Gen b -> Gen b
coarbitraryVector = coarbitrary . GVector.toList

functionVector :: (GVector.Vector v a, Function a) => (v a -> c) -> v a :-> c
functionVector = functionMap GVector.toList GVector.fromList


----------------------------------------------------------------------------------------------------

----------------------------------------------- COO ------------------------------------------------

instance (Arbitrary a, UVector.Unbox a, Num a, Eq a) => Arbitrary (SparseData COO a) where 
    arbitrary = sized $ \n -> do 
        (uvec' :: UVector.Vector a) <- arbitraryVector
        let 
            !uvec       = UVector.filter (/= 0) uvec'
            !len        = UVector.length uvec 
            !def_height = n 
            !def_width  = n 
        !(heights :: UVector.Vector Int) <- UVector.replicateM len (choose (0, def_height)) 
        !(widths :: UVector.Vector Int)  <- UVector.replicateM len (choose (0, def_width))
        !to_return <- UVector.imapM (\i x -> do 
                                 let 
                                    !y = heights UVector.! i 
                                    !z = widths UVector.! i 
                                 return (x, y, z)) uvec 
        return $ (COO to_return n n)

instance (Show a, UVector.Unbox a) => Show (SparseData COO a) where 
    show vec@COO{coo_vals = my_vec, height=h, width=w} = unwords [show my_vec, "(", show h, ",", show w, ")"]


instance CoArbitrary a => CoArbitrary (SparseData COO a) where 
    coarbitrary = coarbitrarySparseData

instance Function a => Function (SparseData COO a) where 
    function = functionSparseData 

   
---------------------------------------------------- CSR --------------------------------------------------------

instance (Arbitrary a, UVector.Unbox a, Num a, Eq a) => Arbitrary (SparseData CSR a) where 
    arbitrary = sized $ \n -> do 
        (val_vec :: UVector.Vector a)   <- arbitraryVector
        let 
            !height = n 
            !width  = n 
            !len    = UVector.length val_vec
         
        (col_vec :: UVector.Vector Int)       <- UVector.replicateM len (choose (0, n))
        (r_off_set_vec :: UVector.Vector Int) <- fmap (UVector.fromList) $ resize width (sublistOf [0..width*height])
        return $ CSR r_off_set_vec col_vec val_vec width height

instance (Show a, UVector.Unbox a) => Show (SparseData CSR a) where 
    show v@CSR{row_offsets=rofs, columns=cols, csr_vals=vals, max_col=width, max_rows=height}= unlines [show rofs
                                                                                                  , show cols
                                                                                                  , show vals 
                                                                                                  , "(" ++ show width ++ "," ++ show height ++ ")"] 

 
instance CoArbitrary a => CoArbitrary (SparseData CSR a) where 
    coarbitrary = coarbitrarySparseData

instance Function a => Function (SparseData CSR a) where 
    function = functionSparseData


---------------------------------------------------------------------------------------------------------------------


coarbitrarySparseData :: (Sparse rep a, CoArbitrary a) => SparseData rep a  -> Gen b -> Gen b 
coarbitrarySparseData = undefined 


functionSparseData :: (Sparse rep a, Function a) => (SparseData rep a  -> c) -> SparseData rep a  :-> c 
functionSparseData = undefined


-- seeA :: (Show a, Arbitrary a, UVector.Unbox a) => Gen (SparseData COO a) 
-- seeA = arbitrary 


-- ---------------------------------------------------------------------------------------------------------------
-- --------------------------------- Properties ------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------


-- -- data MatVecProps = AssocAdd | CommutAdd | AssocConstMul | DistrConstMul 
-- --                 --  | AssocMultVec 
-- --                  | DistrAddMultVec 
-- --         deriving Show 


-- instance Testable MatVecProps where 
--     property p = case p of 
--         AssocAdd        -> MkProperty s_assoc_add_prop 
--         CommutAdd       -> MkProperty s_commut_add_prop 
--         AssocConstMul   -> MkProperty s_assoc_const_mul_prop 
--         DistrConstMul   -> MkProperty s_distr_const_mul_prop 
--         -- AssocMultVec    -> MkProperty s_assoc_mult_vec_prop 
--         DistrAddMultVec -> MkProperty s_distr_add_mult_vec_prop


instance (Eq a, Ord a, UVector.Unbox a) => Eq (SparseData COO a) where 
    (==) !v1 !v2 = let
                        sv1 = S.fromList $ UVector.toList $ coo_vals v1 
                        sv2 = S.fromList $ UVector.toList $ coo_vals v2
                   in (sv1 == sv2) && (height v1 == height v2) && (width v1 == width v2)

         
instance (Eq a, Ord a, UVector.Unbox a) => Eq (SparseData CSR a) where 
    (==) !v1 !v2 = let
                        sv1   = S.fromList $ UVector.toList $ csr_vals v1 
                        sv2   = S.fromList $ UVector.toList $ csr_vals v2
                        cols1 = S.fromList $ UVector.toList $ columns v1 
                        cols2 = S.fromList $ UVector.toList $ columns v2
                        off1  = S.fromList $ UVector.toList $ row_offsets v1
                        off2  = S.fromList $ UVector.toList $ row_offsets v2 
                   in and [(sv1 == sv2)
                           , (max_col v1 == max_col v2) 
                           , (max_rows v1 == max_rows v2)
                           , cols1 == cols2 
                           , off1 == off2 ]
 



-- -- generate a random sparse matrix of given size
-- -- generate another sparse matrix with same length as above
-- -- generate a third
-- -- check if (A + B) + C == A + (B + C) 
s_assoc_add_prop ::  (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a)) => SparseData rep a  -> SparseData rep a  -> SparseData rep a  -> Bool 
s_assoc_add_prop a_mat b_mat c_mat = 
    if (a_mat #+ b_mat) #+ c_mat == a_mat #+ (b_mat #+ c_mat) 
            then True 
            else False 



-- -- generate random sparse matrix of given size
-- -- generate second sparse matrix of same size
-- -- check if (A + B) == (B + A)
s_commut_add_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a)) => SparseData rep a  -> SparseData rep a  -> Bool  
s_commut_add_prop a_mat b_mat =  
    if (a_mat #+ b_mat) == (b_mat #+ a_mat)
        then True 
        else False  

-- -- generate random sparse matrix of given size 
-- -- generate 2 random scalars (integers) r and s
-- -- check whether r (s A) = (r s) A 
s_assoc_const_mul_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a), NFData a) => a -> a -> SparseData rep a -> Bool  
s_assoc_const_mul_prop r s a_mat = 
    if (r `scale` (s `scale` a_mat)) == ((r * s) `scale` a_mat)
        then True 
        else False 


-- -- generate random scalar r 
-- -- generate random sparse matrices A and B 
-- -- check whether r (A + B) = r A + r B 
s_distr_const_mul_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a), NFData a) => a -> SparseData rep a  -> SparseData rep a  -> Bool  
s_distr_const_mul_prop r a_mat b_mat = 
    if r `scale` (a_mat #+ b_mat) == (r `scale` a_mat) #+ (r `scale` b_mat)
        then True 
        else False

-- -- generate random sparse matrix A 
-- -- generate random vectors w and v 
-- -- check whether (A w) . v = A . (w . v) 
-- s_assoc_mult_vec_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a)) => UVector.Vector a -> UVector.Vector a -> SparseData rep a -> Bool  
-- s_assoc_mult_vec_prop  w_vec v_vec a_mat = 
--     if (a_mat #. w_vec) `dot` v_vec == a_mat #. (w_vec `dot` v_vec)
--         then True 
--         else False 
--     where 
--         dot !v1 !v2 = UVector.sum $ UVector.zipWith (*) v1 v2 


-- -- generate random sparse matrices A and B 
-- -- generate random vector v 
-- -- check whether (A + B) v = A v + B v 
s_distr_add_mult_vec_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a)) => UVector.Vector a -> SparseData rep a  -> SparseData rep a  -> Bool  
s_distr_add_mult_vec_prop v_vec a_mat b_mat = 
    if (a_mat #+ b_mat) #. v_vec == (a_mat #. v_vec) `addVecs` (b_mat #. v_vec)
        then True 
        else False 
    where 
        addVecs !v1 !v2 = UVector.zipWith (+) v1 v2 





