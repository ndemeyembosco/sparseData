{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds #-}




module SGeneric where 

import qualified Data.Vector as VU 
import qualified Data.Vector.Unboxed as U  
import qualified Data.Vector.Unboxed.Sized as SU 
import Control.Monad 
import Data.Maybe (maybe, isJust)
import Data.Finite.Internal 
import Data.Functor.Identity 
import qualified Data.Map as M
import GHC.TypeNats
import Prelude hiding (map, zipWith)


data RepIndex = U | D 



type SVector a = (Int -> a, Int) 


to_vector ::U.Unbox a => SVector a -> U.Vector a 
to_vector ((f, len)) = U.generate len f 


from_vector :: U.Unbox a => U.Vector a -> SVector a 
from_vector !vec =  let len = U.length vec in ((U.!) vec, len)


null_i :: U.Unbox a => SVector a -> Bool 
null_i  = (== 0) . snd  

smap_i :: U.Unbox a => (a -> b) -> SVector a -> SVector b
smap_i f ((g, len)) = (f . g, len)


szipWith_i :: (U.Unbox a, U.Unbox a, U.Unbox c) => (a -> b -> c) -> SVector a -> SVector b -> SVector c 
szipWith_i f ((g, len1)) ((h, len2)) = (\i -> f (g i) (h i), len1)  

sum_i :: (U.Unbox a, Num a) => SVector a -> a 
sum_i ((f, len)) = VU.foldr (\i n -> n + (f i)) 0 $ VU.enumFromN 0 (len - 1) 

equals_i :: (U.Unbox a, Num a, Eq a) => SVector a -> SVector a -> Bool 
equals_i vec1 vec2 = U.foldr (&&) True $! U.zipWith (==) (to_vector vec1) (to_vector vec2)



class (U.Unbox e, Num e, Eq e) => Sparse r (ty :: RepIndex) e where 
    -- type r ?
    data SparseData r (ty :: RepIndex) e :: * 
    s_index   :: SparseData r ty e -> (Int, Int) -> e 
    s_dims  :: SparseData r ty e -> (Int, Int) 
    -- By default matVec 
    (#.)      :: SparseData r ty e -> SVector e -> SVector e 
    (#.) !mat !vec  = let delayed_mat = delay mat in delayed_mat #. vec
    s_undelay :: e -> SparseData r D e -> SparseData r U e 



instance (U.Unbox e, Num e, Eq e) => Sparse r D e where 
    data SparseData r D e = SDelayed (Int, Int) ((Int, Int) -> e) -- (height, width), indexing function 
    s_index (SDelayed _ f) (r, c) = f (r, c) 
    s_dims (SDelayed (w, h) _)  = (w, h) 
    (#.) (SDelayed (w, h) func) v@(f, len) = ((VU.!) part_sums, len)
                                where 
                                 r_funcs          = VU.map (\ri -> ((curry func) ri, w)) $ VU.enumFromN 0 (h - 1)  
                                 part_sums        = VU.map (\(g, w) -> sum_i $ szipWith_i (*) (g, w) v) r_funcs



-- instance (Eq e, Sparse r ty e, U.Unbox e) => Eq (SparseData r ty e) where 
--     arr1 == arr2 = and_v (vals_vec mat)    
--            where 
--             and_v  l     = U.foldr (&&) True l  
--             darr1        = delay arr1 
--             darr2        = delay arr2
--             mat          = s_undelay True $ zipWith_s (==) darr1 darr2
--             vals_vec m   = U.map (\(a, _, _) -> a) (coo_vals m)


delay :: (Sparse r ty e, U.Unbox e) => SparseData r ty e -> SparseData r D e 
delay arr = SDelayed (s_dims arr) (s_index arr)


transpose :: (Sparse r ty e, U.Unbox e) => SparseData r ty e -> SparseData r D e
transpose mat = let 
                    (w, h) = s_dims mat 
                    new_index_func m (r, c) = s_index m (c, r)
                in SDelayed (h, w) (new_index_func mat) 


convert :: (Sparse r1 D e, Sparse r2 D e) => SparseData r1 D e -> SparseData r2 D e 
convert (SDelayed (w, h) func) = (SDelayed (w, h) func)  





empty :: Sparse r ty a => SparseData r ty a -> Bool 
empty mat = (0, 0) == (s_dims mat)

-------------- Polymorphic -----------------------------------------------

map :: Sparse r ty e => (e -> b) -> SparseData r ty e -> SparseData r D b 
map f !arr = case delay arr of 
    SDelayed (w, h) g -> SDelayed (w, h) (f . g) 



zipWith :: (Sparse r ty a, Sparse r1 ty1 b, r ~ r1) => (a -> b -> c) -> SparseData r ty a -> SparseData r1 ty1 b -> SparseData r D c  -- can only zip two things of the same rep
zipWith f !arr1 !arr2 = SDelayed (w1, h1) get 
                    where 
                        SDelayed (w1, h1) f1 = delay arr1 
                        SDelayed _ f2 = delay arr2
                        get val = f (f1 val) (f2 val)

(#*) :: (Sparse r ty a, Num a) => SparseData r ty a -> SparseData r ty a -> SparseData r D a
(#*) !a_mat !b_mat = SDelayed (a_w, b_h) get 
             where 
                mat1@(SDelayed  (a_w, a_h) f1) = delay a_mat 
                mat2@(SDelayed  (b_w, b_h) f2) = delay b_mat 
                get (i, j) = VU.foldr (\e prev -> e + prev) (0) 
                                  $ VU.map (\k -> (*) (f1 (i, k)) (f2 (k, j))) (VU.enumFromN 0 (a_w - 1)) 



(#+) :: (Sparse r ty a, Num a) => SparseData r ty a -> SparseData r ty a -> SparseData r D a
(#+) = zipWith (+)


add :: (Sparse r ty a, Num a) => SparseData r ty a -> SparseData r ty a -> SparseData r D a
add = (#+)


(#-) :: (Sparse r ty a, Num a) => SparseData r ty a -> SparseData r ty a -> SparseData r D a 
(#-) = zipWith (-)


minus :: (Sparse r ty a, Num a) => SparseData r ty a -> SparseData r ty a -> SparseData r D a
minus = (#-)

scale :: (Sparse r ty a, Num a) => a -> SparseData r ty a -> SparseData r D a 
scale !n = map (* n)


