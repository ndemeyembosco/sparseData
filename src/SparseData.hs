{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts #-}

module SparseData where 
import qualified Data.Vector as VU 
import qualified Data.Vector.Unboxed as U  
import Control.Monad 
import Data.Maybe (maybe)



type SVector a = (Int -> a, Int) -- indexing function, length of vector 


-- linear time 
to_vector ::U.Unbox a => SVector a -> U.Vector a 
to_vector (f, len) = U.generate len f 


-- constant time 
from_vector :: U.Unbox a => U.Vector a -> SVector a 
from_vector vec =  let len = U.length vec in ((U.!) vec, len)

null_i :: SVector a -> Bool 
null_i  = (== 0) . snd 

smap_i :: U.Unbox a => (a -> b) -> SVector a -> SVector b
smap_i f (g, len) = (f . g, len)


szipWith_i :: (U.Unbox a, U.Unbox a, U.Unbox c) => (a -> b -> c) -> SVector a -> SVector b -> SVector c 
szipWith_i f (g, len) (h, len1) = if len /= len1 then error "length mismatch!" else (\i -> f (g i) (h i), len)  

sum_i :: (U.Unbox a, Num a) => SVector a -> a 
sum_i (f, len) = VU.foldr (\i n ->  n + (f i)) 0 $ VU.enumFromN 0 (len - 1) 

equals_i :: (U.Unbox a, Num a, Eq a) => SVector a -> SVector a -> Bool 
equals_i vec1 vec2 = (to_vector vec1) == (to_vector vec2)


class (U.Unbox e, Num e) => Sparse r ty e where 
    data SparseData r ty e :: * 
    s_index   :: SparseData r ty e -> (Int, Int) -> Maybe e 
    s_height  :: SparseData r ty e -> Int 
    s_width   :: SparseData r ty e -> Int 
    -- By default matVec 
    (#.)      :: SparseData r ty e -> SVector e -> SVector e 
    (#.) !mat !vec  = let delayed_mat = delay mat in delayed_mat #. vec
    -- Default sparse to coo transformation to ease testing
    s_to_coo  :: SparseData r ty e -> SparseData COO U e 
    s_to_coo  = s_undelay . delay  



instance (Eq e, Sparse r ty e) => Eq (SparseData r ty e) where 
    arr1 == arr2 = and_v (vals_vec mat)    
           where 
            and_v  l     = let parts = U.foldr (+) 1 l in U.length l == parts 
            darr1        = delay arr1 
            darr2        = delay arr2
            mat          = s_undelay $ zipWith_s (\x y -> if x == y then 1 else 0) darr1 darr2
            vals_vec m   = U.map (\(a, _, _) -> a) (coo_vals m)
 

 
data D 
----------------- Delayed --------------------------------------

instance (U.Unbox e, Num e) => Sparse r D e where 
    data SparseData r D e = SDelayed (Int, Int) ((Int, Int) -> Maybe e) -- (height, width), indexing function 
    s_index (SDelayed _ f) (r, c) = f (r, c) 
    s_height (SDelayed (h, _) _)  = h 
    s_width (SDelayed (_, w) _)   = w 
    (#.) (SDelayed (h, w) func) v@(f, len) = ((VU.!) part_sums, len)
                                where 
                                 row_func r1 c1   = maybe 0 id $ func (r1, c1)  -- turn Nothings into zeros 
                                 r_funcs          = VU.map (\ri -> (row_func ri, w)) $ VU.enumFromN 0 (h - 1)  
                                 part_sums        = VU.map (\(g, w) -> sum_i $ szipWith_i (*) (g, w) v) r_funcs
    s_to_coo = s_undelay 


delay :: (Sparse r ty e, U.Unbox e) => SparseData r ty e -> SparseData r D e 
delay arr = SDelayed (s_height arr, s_width arr) (s_index arr)


-- will there actually be any zeros in this?
-- As in if the Nothing is there just to catch 
-- indexing errors then, I don't actually think 
-- any zeros will be produced in this.
s_undelay :: (Num e, U.Unbox e) => SparseData r D e -> SparseData COO U e 
s_undelay (SDelayed (h, w) func) = COO vals w h 
        where 
            vals = U.generate (h * w) (\i -> let (r1, c1) = i `divMod` h in (maybe 0 id $ func (r1, c1), r1, c1))

coo_to_sd :: Sparse r D e => SparseData COO U e -> SparseData r D e 
coo_to_sd arr = let f = s_index arr in SDelayed (s_height arr, s_width arr) f 


is_null :: Sparse r ty a => SparseData r ty a -> Bool 
is_null mat = U.null vals 
    where 
        vals = coo_vals $ s_to_coo mat 

-------------- Polymorphic -----------------------------------------------

map_s :: Sparse r ty e => (e -> b) -> SparseData r ty e -> SparseData r D b 
map_s f arr = case delay arr of 
    SDelayed s g -> SDelayed s (\ix -> f <$> g ix)  



zipWith_s :: (Sparse r ty a, Sparse r1 ty1 b, r ~ r1) => (a -> b -> c) -> SparseData r ty a -> SparseData r1 ty1 b -> SparseData r D c  -- can only zip two things of the same rep
zipWith_s f arr1 arr2 = SDelayed (w, h) get 
                    where 
                        SDelayed (w1, h1) f1 = delay arr1 
                        SDelayed (w2, h2) f2 = delay arr2
                        get val = f <$> (f1 val) <*> (f2 val)
                        (w, h)  = if and [w1 == w2, h1 == h2] then (w1, h1) else error "zipWith dimension mismatch!"


(#+) :: (Sparse r ty a, Num a) => SparseData r ty a -> SparseData r ty a -> SparseData r D a
(#+) = zipWith_s (+)


(#-) :: (Sparse r ty a, Num a) => SparseData r ty a -> SparseData r ty a -> SparseData r D a 
(#-) = zipWith_s (-)

scale :: (Sparse r ty a, Num a) => a -> SparseData r ty a -> SparseData r D a 
scale n = map_s (* n)


data U
--------------- Unboxed --------------------

data COO 
--------------- COO ------------------------
instance (U.Unbox e, Num e) => Sparse COO U e where 
    data instance SparseData COO U e   = COO { coo_vals :: U.Vector (e, Int, Int), width :: Int, height :: Int}
    -- indexing is big o length of non zeros
    s_index (COO vals w h) (r, c) = if U.null els then Nothing else let (a1, _, _) = U.head els in Just a1 
                      where 
                        els = U.filter (\(a, x, y) -> and [x == r, y == c]) vals
    s_height (COO vals _ h) = h 
    s_width (COO vals w _)  = w 
    s_to_coo                = id 

instance (Show a, U.Unbox a) => Show (SparseData COO U a) where 
    show vec@COO{coo_vals = my_vec, height=h, width=w} = unwords [show my_vec, "(", show h, ",", show w, ")"]


data CSR   
--------------- CSR -------------------------
instance (U.Unbox e, Num e) => Sparse CSR U e where 
    data instance SparseData CSR U e = CSR { row_offsets     :: U.Vector Int
                                       ,  col_index_csr :: U.Vector Int
                                       ,  csr_vals      :: U.Vector e
                                       ,  csr_height    :: !Int
                                       ,  csr_width     :: !Int
                                       } 
    -- indexing is big o of maximum number of elements per row
    s_index (CSR row_offs col_index vals h w) (r, c) = if U.null els then Nothing else let (_, a1) = U.head els in Just a1
                                 where
                                   to_slice = row_offs U.! r 
                                   to_start = row_offs U.! (r - 1)
                                   vec      = U.slice to_start (to_slice - to_start) $ U.zip col_index vals 
                                   els      = U.filter (\(x, _) -> x == c) vec
    s_height (CSR _ _ _ h _)  = h 
    s_width  (CSR _ _ _ _ w)  = w 
    
instance (Show a, U.Unbox a) => Show (SparseData CSR U a) where 
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


data ELL 
----------------- ELL -------------------
instance (U.Unbox e, Num e) => Sparse ELL U e where 
    data instance SparseData ELL U e = ELL { max_elem_row    :: !Int
                                        , col_index_ell :: U.Vector Int
                                        , ell_vals      :: U.Vector e
                                        , ell_height    :: !Int
                                        , ell_width     :: !Int
                                        }
    -- indexing is big o of maximum number of elements per row.
    s_index (ELL max_e_r col_ind vals h w) (r, c) = if U.null els then Nothing else let (_, a1) = U.head els in Just a1
                                        where 
                                          to_start = r * max_e_r
                                          vec      = U.slice to_start max_e_r $ U.zip col_ind vals 
                                          els      = U.filter (\(x,_) -> x == c) vec  
    s_height (ELL _ _ _ h _) = h 
    s_width  (ELL _ _ _ _ w) = w 



instance (Show a, U.Unbox a) => Show (SparseData ELL U a) where
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


--------------------------- Generic Iterative linear solvers --------------------------------------------------------------------------

cg :: (Num a, Sparse rep ty a, U.Unbox a, Eq a, Floating a) => Int -> SVector a -> SparseData rep ty a ->  SVector a -> (a, SVector a)
{-# INLINE cg #-}
cg !iters !z !a !x = 
    let 
        !r     = x 
        !p     = r  
        !rho   = r <.> r 
    in go a z x rho p r 0 
    where
        {-# INLINE go #-}
        go !a !z !x !d !p !r !i | i == iters     =  
                                    let 
                                        !residual = (a #. z) ^-^ x 
                                        !to_ret = sqrt $ residual <.> residual 
                                    in (to_ret, z) 
                                | otherwise =  
                                    let 
                                        !q         = a #. p 
                                        !alpha     = d / (p <.> q) 
                                        !new_z     = z ^+^ (alpha .* p) 
                                        !new_r     = r ^-^ (alpha .* q)  
                                        !new_d     = new_r <.> new_r 
                                        !beta      = new_d / d  
                                        !new_p     = new_r ^+^ (beta .* p) 
                                    in go a new_z x new_d new_p new_r (i + 1)
        (<.>) v1 v2   = sum_i $ szipWith_i (*) v1 v2 
        (^+^)         = szipWith_i (+)
        (^-^)         = szipWith_i (-)  
        (.*) c        = smap_i (*c)  


---------------------------------------------------------------------------------------------------------------------------------------







