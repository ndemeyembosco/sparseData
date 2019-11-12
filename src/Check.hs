{-# LANGUAGE  TypeOperators, FlexibleInstances, BangPatterns, FlexibleContexts 
, ScopedTypeVariables, GADTs, RankNTypes, AllowAmbiguousTypes, RecursiveDo #-}



module Check where

import SparseData 
import Test.QuickCheck hiding (scale) 
import Test.QuickCheck.Property 
import Test.QuickCheck.Function ((:->))
import Control.Monad.Par 
import Data.Monoid 

import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector as BVector 
import qualified Data.Vector.Generic as GVector
import qualified Data.Set as S 
import Data.List 
import Control.Monad 


instance (UVector.Unbox a, Arbitrary a, Ord a) => Arbitrary (UVector.Vector a) where
    arbitrary = arbitraryVector
    shrink = shrinkVector


arbitraryVector :: (GVector.Vector v a, Arbitrary a, Ord a) => Gen (v a)
arbitraryVector = fmap (\l -> GVector.fromList $ sort l) arbitrary


                        

shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList

-- coarbitraryVector :: (GVector.Vector v a, CoArbitrary a) => v a -> Gen b -> Gen b
-- coarbitraryVector = coarbitrary . GVector.toList

-- functionVector :: (GVector.Vector v a, Function a) => (v a -> c) -> v a :-> c
-- functionVector = functionMap GVector.toList GVector.fromList


----------------------------------------------------------------------------------------------------

----------------------------------------------- COO ------------------------------------------------

instance (Arbitrary a, UVector.Unbox a, Num a, Eq a, Ord a) => Arbitrary (SparseData COO a) where 
    arbitrary = sized $ \n -> do 
        (uvec' :: UVector.Vector a) <- arbitraryVector
        let 
            !uvec       = UVector.filter (/= 0) uvec'
            !len        = UVector.length uvec 
            !def_height = n 
            !def_width  = n 
        !(heights' :: [Int]) <- replicateM len (choose (0, def_height - 1)) 
        !(widths' :: [Int])  <- replicateM len (choose (0, def_width - 1))
        let (!heights, !widths) = UVector.unzip $ UVector.fromList $ sort $ S.toList $ S.fromList $ zip heights' widths' 
        !to_return <- UVector.imapM (\i x -> do 
                                 let 
                                    !y = heights UVector.! i 
                                    !z = widths UVector.! i 
                                 return (x, y, z)) uvec 
        return $ (COO to_return n n)

instance (Show a, UVector.Unbox a) => Show (SparseData COO a) where 
    show vec@COO{coo_vals = my_vec, height=h, width=w} = unwords [show my_vec, "(", show h, ",", show w, ")"]


-- instance CoArbitrary a => CoArbitrary (SparseData COO a) where 
--     coarbitrary = coarbitrarySparseData

-- instance Function a => Function (SparseData COO a) where 
--     function = functionSparseData 


instance (Eq a, Ord a, UVector.Unbox a) => Eq (SparseData COO a) where 
    (==) !v1 !v2 = let
                        sv1 = S.fromList $ UVector.toList $ coo_vals v1 
                        sv2 = S.fromList $ UVector.toList $ coo_vals v2
                   in (sv1 == sv2) && (height v1 == height v2) && (width v1 == width v2)

  
---------------------------------------------------- CSR --------------------------------------------------------

instance (Arbitrary a, UVector.Unbox a, Num a, Eq a, Ord a) => Arbitrary (SparseData CSR a) where 
    arbitrary = sized $ \n1 -> do 
        (coo_source :: SparseData COO a) <- arbitrary
        return $ coo_to_csr coo_source 

                

instance (Show a, UVector.Unbox a) => Show (SparseData CSR a) where 
    show !v1@CSR{row_offsets=row_off1, 
                        col_index_csr=col1 
                        , csr_vals=vals1 
                        , csr_height=h1
                        , csr_width=w1}  = unlines [ 
                                                "(" ++ show h1 ++ "," ++ show w1 ++ ")"
                                               , "values: " ++ show vals1 
                                               , "column index: " ++ show col1
                                               , "row offsets: " ++ show row_off1
                                            ]
                                                                                                --   , show cols
                                                                                                --   , show vals 
                                                                                                --   , "(" ++ show width ++ "," ++ show height ++ ")"] 

 
-- instance CoArbitrary a => CoArbitrary (SparseData CSR a) where 
--     coarbitrary = coarbitrarySparseData

-- instance Function a => Function (SparseData CSR a) where 
--     function = functionSparseData

instance (Eq a, Ord a, UVector.Unbox a) => Eq (SparseData CSR a) where 
    (==) !v1@CSR{row_offsets=row_off1, 
                 col_index_csr=col1 
                 , csr_vals=vals1 
                 , csr_height=h1
                 , csr_width=w1} 
                 
                 !v2@CSR{row_offsets=row_off2 
                        , col_index_csr=col2 
                        , csr_vals=vals2 
                        , csr_height=h2 
                        , csr_width=w2} = and [ row_off1 == row_off2
                                           , col1 == col2 
                                           , vals1 == vals2 
                                           , h1 == h2 
                                           , w1 == w2]


--------------------------------------------------- ELL -------------------------------------------------------------

instance (Arbitrary a, UVector.Unbox a, Num a, Eq a, Ord a) => Arbitrary (SparseData ELL a) where 
    arbitrary = do 
        (height :: Int) <- suchThat arbitrary (>2)  
        mr <- choose (2, height - 1)
        let vec_size = height * mr 
        col_index <- UVector.replicateM (vec_size) (choose (0, height - 1))  -- no two similar indices on the same row
        vals      <- UVector.replicateM vec_size (suchThat arbitrary (>0))
        return $ ELL mr col_index vals height height

instance (Show a, UVector.Unbox a) => Show (SparseData ELL a) where
    show m@ELL{
                max_elem_row = mr 
              , ell_vals     = vals 
              , ell_height   = height 
              , col_index_ell = col_index 
              } = unlines [
                  "(" ++ show height ++ "," ++ show height ++ ")"
                , "max elem per row: " ++ show mr 
                , "values : " ++ show vals 
                , "col indices : " ++ show col_index
              ]

-- instance CoArbitrary a => CoArbitrary (SparseData ELL a) where 
--     coarbitrary = coarbitrarySparseData

-- instance Function a => Function (SparseData ELL a) where 
--     function = functionSparseData

instance (Eq a, Ord a, UVector.Unbox a) => Eq (SparseData ELL a) where
    (==) (ELL mr1 ind1 vals1 h1 w1) 
         (ELL mr2 ind2 vals2 h2 w2) 
                                   = and 
                                     [mr1 == mr2 
                                     , ind1 == ind2 
                                     , vals1 == vals2 
                                     , h1 == h2 
                                     , w1 == w2]


---------------------------------------------------------------------------------------------------------------------


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

-- -- generate a random sparse matrix of given size
-- -- generate another sparse matrix with same length as above
-- -- generate a third
-- -- check if (A + B) + C == A + (B + C) 
s_assoc_add_prop ::  (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a)) => SparseData rep a  -> SparseData rep a  -> SparseData rep a  -> Bool 
s_assoc_add_prop a_mat b_mat c_mat = 
    let 
        (a_w, a_h) = dims a_mat
        (b_w, b_h) = dims b_mat
        (c_w, c_h) = dims c_mat 
    in if not $ and [a_w == b_w, a_w == c_w, a_h == b_h, a_h == c_h] then True
    else 
        if (a_mat #+ b_mat) #+ c_mat == a_mat #+ (b_mat #+ c_mat) 
                then True 
                else False 



-- -- generate random sparse matrix of given size
-- -- generate second sparse matrix of same size
-- -- check if (A + B) == (B + A)
s_commut_add_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a)) => SparseData rep a  -> SparseData rep a  -> Bool  
s_commut_add_prop a_mat b_mat = 
    let 
        (a_w, a_h) = dims a_mat
        (b_w, b_h) = dims b_mat
    in if or [a_w /= b_w, a_h /= b_h, is_null a_mat, is_null b_mat] then True 
    else  
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
    let 
        (a_w, a_h) = dims a_mat
        (b_w, b_h) = dims b_mat
    in if a_w /= b_w || a_h /= b_h then True 
    else 
        if r `scale` (a_mat #+ b_mat) == (r `scale` a_mat) #+ (r `scale` b_mat)
            then True 
            else False



-- -- generate random sparse matrix A 
-- -- generate random vectors w and v 
-- -- check whether A (w + v) = A w + A v 
s_assoc_mult_vec_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a)) => UVector.Vector a -> UVector.Vector a -> SparseData rep a -> Bool  
s_assoc_mult_vec_prop  w_vec v_vec a_mat = 
        if or [len_w /= len_v, len_w /= w, UVector.null w_vec || UVector.null v_vec, is_null a_mat] then True 
        else a_mat #. (w_vec ^+^ v_vec) == (a_mat #. w_vec) ^+^ (a_mat #. v_vec)
    where   
        (^+^)  = UVector.zipWith (+)
        (w, h) = dims a_mat
        len_w  = UVector.length w_vec 
        len_v  = UVector.length v_vec 


-- -- generate random sparse matrices A and B 
-- -- generate random vector v 
-- -- check whether (A + B) v = A v + B v 
s_distr_add_mult_vec_prop :: (Eq a, Ord a, UVector.Unbox a, Sparse rep a, Num a, Eq (SparseData rep a)) => UVector.Vector a -> SparseData rep a  -> SparseData rep a  -> Bool  
s_distr_add_mult_vec_prop v_vec a_mat b_mat = 
    let 
        (a_w, a_h) = dims a_mat
        (b_w, b_h) = dims b_mat
        len        = UVector.length v_vec
    in if or [a_w /= b_w, a_h /= b_h, a_w /= len] then True
    else 
        if (a_mat #+ b_mat) #. v_vec == (a_mat #. v_vec) `addVecs` (b_mat #. v_vec)
            then True 
            else False 
    where 
        addVecs !v1 !v2 = UVector.zipWith (+) v1 v2 



-- testing conversions!
coo_csr_id_prop :: (UVector.Unbox a, Eq a, Ord a, Num a) => SparseData COO a -> SparseData CSR a -> Bool 
coo_csr_id_prop coo_mat csr_mat = 
    let 
        (coo_w, coo_h) = dims coo_mat 
        (csr_w, csr_h) = dims csr_mat 
    in if or [coo_w /= csr_w, coo_h /= csr_h, (csr_vals csr_mat == UVector.empty) || (coo_vals coo_mat == UVector.empty)] then True 
    else and [((csr_to_coo $ coo_to_csr coo_mat) == coo_mat), ((coo_to_csr $ csr_to_coo csr_mat) == csr_mat)] 





