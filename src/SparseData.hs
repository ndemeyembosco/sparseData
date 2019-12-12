{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes 
           , UndecidableInstances #-}

module SparseData where 
import qualified Data.Vector as VU 
import qualified Data.Vector.Unboxed as U  
import Control.Monad 
import Data.Maybe (maybe, isJust)
import qualified Data.Map as M
 



type SVector a = (Int -> Maybe a, Int) -- indexing function, length of vector 


-- linear time 
to_vector ::(U.Unbox a, Num a) => SVector a -> U.Vector a 
to_vector (f, len) = U.generate len (\i -> maybe 0 id $ f i) 


-- constant time 
from_vector :: (U.Unbox a, Num a, Eq a) => U.Vector a -> SVector a 
from_vector vec =  let len = U.length vec in (\i -> vec U.!? i >>= \n -> if n == 0 then Nothing else Just n, len)

null_i :: SVector a -> Bool 
null_i  = (== 0) . snd 

smap_i :: U.Unbox a => (a -> b) -> SVector a -> SVector b
smap_i f (g, len) = (\i -> f <$> g i, len)


szipWith_i :: (U.Unbox a, U.Unbox a, U.Unbox c) => (a -> b -> c) -> SVector a -> SVector b -> SVector c 
szipWith_i f (g, len) (h, len1) = if len /= len1 then error "length mismatch!" else (\i -> f <$> (g i) <*> (h i), len)  

sum_i :: (U.Unbox a, Num a) => SVector a -> a 
sum_i (f, len) = VU.foldr (\i n ->  maybe 0 (+n) $ f i) 0 $ VU.enumFromN 0 (len - 1) 

equals_i :: (U.Unbox a, Num a, Eq a) => SVector a -> SVector a -> Bool 
equals_i vec1 vec2 = U.foldr (&&) True $! U.zipWith (==) (to_vector vec1) (to_vector vec2)


class (U.Unbox e, Num e, Eq e) => Sparse r ty e where 
    data SparseData r ty e :: * 
    s_index   :: SparseData r ty e -> (Int, Int) -> Maybe e 
    s_height  :: SparseData r ty e -> Int 
    s_width   :: SparseData r ty e -> Int 
    -- By default matVec 
    (#.)      :: SparseData r ty e -> SVector e -> SVector e 
    (#.) !mat !vec  = let delayed_mat = delay mat in delayed_mat #. vec
    -- Default sparse to coo transformation to ease testing
    s_to_coo  :: Num e => SparseData r ty e -> SparseData COO U e 
    s_to_coo  = (s_undelay 0) . delay  



class Sparse r D e => Undelayable r e where 
    -- expensive conversion operations! first argument, is a zero! 
    -- also might have to perform this in parallel!
    undelay :: e -> SparseData r D e -> SparseData r U e 
   




instance (Eq e, Sparse r ty e) => Eq (SparseData r ty e) where 
    arr1 == arr2 = and_v (vals_vec mat)    
           where 
            and_v  l     = U.foldr (&&) True l  
            darr1        = delay arr1 
            darr2        = delay arr2
            mat          = s_undelay True $ zipWith_s (==) darr1 darr2
            vals_vec m   = U.map (\(a, _, _) -> a) (coo_vals m)
 

 
data D 
----------------- Delayed --------------------------------------

instance (U.Unbox e, Num e, Eq e) => Sparse r D e where 
    data SparseData r D e = SDelayed (Int, Int) ((Int, Int) -> Maybe e) -- (height, width), indexing function 
    s_index (SDelayed _ f) (r, c) = f (r, c) 
    s_height (SDelayed (h, _) _)  = h 
    s_width (SDelayed (_, w) _)   = w 
    (#.) (SDelayed (h, w) func) v@(f, len) = ((VU.!?) part_sums, len)
                                where 
                                 r_funcs          = VU.map (\ri -> ((curry func) ri, w)) $ VU.enumFromN 0 (h - 1)  
                                 part_sums        = VU.map (\(g, w) -> sum_i $ szipWith_i (*) (g, w) v) r_funcs
    s_to_coo = s_undelay 0

instance (U.Unbox e, Show e, Sparse r D e, Num e, Eq e) => Show (SparseData r D e) where 
    show arr@(SDelayed (w, h) f) = unlines ["<delayed function> : ", show $ s_undelay 0 arr] 


delay :: (Sparse r ty e, U.Unbox e) => SparseData r ty e -> SparseData r D e 
delay arr = SDelayed (s_height arr, s_width arr) (s_index arr)


-- will there actually be any zeros in this?
-- As in if the Nothing is there just to catch 
-- indexing errors then, I don't actually think 
-- any zeros will be produced in this.
s_undelay :: (U.Unbox e, Eq e) => e -> SparseData r D e -> SparseData COO U e 
s_undelay e (SDelayed (h, w) func) = COO vals w h 
        where 
            vals = U.filter (\(el, _, _) -> el /= e) $ U.generate (h * w) (\i -> let (r1, c1) = i `divMod` h in (maybe e id $ func (r1, c1), r1, c1))

coo_to_sd :: Sparse r D e => SparseData COO U e -> SparseData r D e 
coo_to_sd arr = let f = s_index arr in SDelayed (s_height arr, s_width arr) f 

convert_sd :: (Sparse r1 D e, Sparse r2 D e) => SparseData r1 D e -> SparseData r2 D e 
convert_sd (SDelayed (w, h) func) = (SDelayed (w, h) func)  





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

(#*) :: (Sparse r ty a, Num a) => SparseData r ty a -> SparseData r ty a -> SparseData r D a
(#*) a_mat b_mat = SDelayed (w, h) get 
             where 
                mat1@(SDelayed (a_w, a_h) f1) = delay a_mat 
                mat2@(SDelayed (b_w, b_h) f2) = delay b_mat 
                get (i, j) = VU.foldr (\e prev -> e >>= \n -> prev >>= \m -> Just (n+m) ) (Just 0) 
                                  $ VU.map (\k -> (*) <$> f1 (i, k) <*> f2 (k, j)) (VU.enumFromN 0 (a_w - 1)) 
                (w, h) = if a_h == b_w then (a_w, b_h) else error "matrix matrix multiplication dimension mismatch!"


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
instance (U.Unbox e, Num e, Eq e) => Sparse COO U e where 
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
instance (U.Unbox e, Num e, Eq e) => Sparse CSR U e where 
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
                                   to_start = case row_offs U.!? (r - 1) of 
                                                  Nothing -> 0 -- error ("access out of bounds here" ++ show row_offs)
                                                  Just n  -> n 
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
instance (U.Unbox e, Num e, Eq e) => Sparse ELL U e where 
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



---------------------------- undelaying ----------------------------------------------------------------------

instance (U.Unbox e, Num e, Eq e) => Undelayable COO e where
    undelay  = s_undelay  
    
    
instance (U.Unbox e, Num e, Eq e) => Undelayable CSR e where 
    undelay zero f@(SDelayed (w, h) func) = CSR offsets col_inds vals h w 
                 where 
                    coo_rep                = coo_vals $ s_undelay zero f 
                    (vals, _, col_inds)    = U.unzip3 coo_rep
                    offsets                = (compress h) `U.snoc` (U.length vals) 
                    -- write compress with continuations!
                    compress 0 = U.singleton 0
                    compress n = let prev = compress (n - 1) in prev `U.snoc` (elem_row (n - 1) + U.last prev) 
                    elem_row r = U.length $ U.filter (/= zero) $ U.generate w (\i -> maybe zero id $ func (r, i))


instance (U.Unbox e, Num e, Eq e) => Undelayable ELL e where 
    undelay zero f@(SDelayed (w, h) func) = ELL max_e col_inds vals h w 
                   where 
                    (vals, col_inds) = U.unzip $  U.generate (max_e * w) (\i -> let (r, c) = i `divMod` h in (maybe zero id $ func (r, c), c)) 
                    max_e            = find_max 0 (h - 1) func  
                    find_max m 0 f = m 
                    find_max m r f = let 
                                       res = VU.map (\i -> f (r, i)) (VU.enumFromN 0 (w - 1))
                                       len = VU.length $ VU.filter isJust res 
                                     in if m < len then find_max len (r-1) f else find_max m (r-1) f   
                                                             


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







