{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds #-}

module SparseBlas.Data.Matrix.Parallel.Generic.Generic where 

import qualified Data.Vector as V 
import Control.Parallel.Strategies ( NFData, using )
import Data.Vector.Strategies ( parVector )
import Prelude hiding (map, zipWith)


data RepIndex = U | D 

type SVector a = (Int -> a, Int) 


to_vector :: NFData a => SVector a -> V.Vector a 
to_vector (f, !len) = (V.generate len f) `using` (parVector 2)


from_vector :: NFData a => V.Vector a -> SVector a 
from_vector !vec =  let len = V.length vec in ((V.!) vec, len)


vnull ::  NFData a => SVector a -> Bool 
vnull  = (== 0) . snd  

vmap ::  NFData a => (a -> b) -> SVector a -> SVector b
vmap f (g, !len) = (f . g, len)




vzipWith :: (NFData a, NFData a, NFData c) 
         => (a -> b -> c) -> SVector a -> SVector b -> SVector c 
vzipWith f (g, !len1) (h, !len2) = (\i -> f (g i) (h i), len1) 

(!+!) :: (NFData a, Num a) 
      =>  SVector a -> SVector a -> SVector a  
(!+!)  = vzipWith (+)

(!-!) :: (NFData a, Num a) 
      =>  SVector a -> SVector a -> SVector a 
(!-!)  = vzipWith (-)

(!*!) :: (NFData a, Num a) => a -> SVector a -> SVector a 
(!*!) x = vmap (* x) 

vsum :: (NFData a, Num a) => SVector a -> a 
vsum (f, !len) = V.foldr' (\i n -> n + (f i)) 0 $ V.enumFromN 0 (len - 1) -- add parallelism 

(!.!) :: (NFData a, Num a) => SVector a -> SVector a -> a 
(!.!) v1 = vsum . vzipWith (*) v1 


veq :: (NFData a, Num a, Eq a) => SVector a -> SVector a -> Bool 
veq !vec1 !vec2 = V.foldr (&&) True $! V.zipWith 
                                     (==) (to_vector vec1) (to_vector vec2)



class (NFData e, Num e, Eq e) => Sparse r (ty :: RepIndex) e where 
    data SparseData r (ty :: RepIndex) e :: * 
    s_index   :: SparseData r ty e -> (Int, Int) -> e 
    s_dims    :: SparseData r ty e -> (Int, Int) 
    -- By default matVec 
    (#.)      :: Num e => SparseData r ty e -> SVector e -> SVector e 
    (#.) !mat !vec  = let delayed_mat = delay mat in delayed_mat #. vec

class (Sparse r D e, Sparse r U e) => Undelay r e where 
    s_undelay :: SparseData r D e -> SparseData r U e 
    non_zeros :: SparseData r U e      -> V.Vector e 
    



instance (NFData e, Num e, Eq e) => Sparse r D e where 
    data SparseData r D e         = SDelayed (Int, Int) ((Int, Int) -> e) 
    s_index (SDelayed _ f) (!r, !c) = f (r, c) 
    s_dims (SDelayed (!w, !h) _)    = (w, h) 
    (#.) (SDelayed (!w, !h) m_index_f) v@(_, !len) = ((V.!) part_sums, len)
       where 
         r_funcs   = V.map (\ri -> ((curry m_index_f) ri, w)) 
                           $ V.enumFromN 0 h   
         part_sums = V.map (\(g, w) -> (g, w) !.! v) r_funcs



--instance (Eq e, Num e, Sparse r U e
--              , Sparse r2 U e, r ~ r2, U.Unbox e) => Eq (SparseData COO U e) where 
--    arr1 == arr2 = (and_v v_vec) == (U.length v_vec)    
--           where 
--            v_vec        = vals_vec mat
--            and_v  l     = U.foldr (+) 0 l  
--            darr1   = delay arr1   
--            darr2   = delay arr2
--            mat          = s_undelay 0 $ zipWith (\x y -> if x == y then fromInteger 1 else 0) darr1 darr2  
--            vals_vec m   = U.map (\(a, _, _) -> a) (coo_vals m)


delay :: (Sparse r ty e) => SparseData r ty e -> SparseData r D e 
delay arr = SDelayed (s_dims arr) (s_index arr)


transpose :: (Sparse r ty e) => SparseData r ty e -> SparseData r D e
transpose mat = let 
                    (w, h) = s_dims mat 
                    new_index_func m (r, c) = s_index m (c, r)
                in SDelayed (h, w) (new_index_func mat) 


convert :: (Sparse r1 D e, Sparse r2 D e) 
        => SparseData r1 D e -> SparseData r2 D e 
convert (SDelayed (w, h) func) = (SDelayed (w, h) func)  


manifest_convert :: (Undelay r1 e, Undelay r2 e) 
                 => SparseData r1 U e -> SparseData r2 U e 
manifest_convert  = s_undelay . convert . delay   





empty :: (Sparse r ty a, Undelay r a) => SparseData r ty a -> Bool 
empty mat = (0, 0) == (s_dims mat) || (V.null $ non_zeros $ s_undelay dmat)
     where 
       dmat = delay mat 

-------------- Polymorphic -----------------------------------------------

map :: Sparse r ty e => (e -> b) -> SparseData r ty e -> SparseData r D b 
map f !arr = case delay arr of 
    SDelayed (w, h) g -> SDelayed (w, h) (f . g) 



zipWith :: (Sparse r ty a, Sparse r1 ty1 b, r ~ r1, ty ~ ty1) 
        => (a -> b -> c) -> SparseData r ty a 
        -> SparseData r1 ty1 b -> SparseData r D c  
zipWith f !arr1 !arr2 = SDelayed (w1, h1) get 
   where 
     SDelayed (w1, h1) f1 = delay arr1 
     SDelayed _ f2        = delay arr2
     get val              = f (f1 val) (f2 val)

-- (#*) :: (Sparse r ty a, Num a) 
--     => SparseData r ty a -> SparseData r ty a -> SparseData r D a
-- (#*) !a_mat !b_mat = SDelayed (a_w, b_h) get 
--   where 
--     mat1@(SDelayed  (a_w, a_h) f1) = delay a_mat 
--     mat2@(SDelayed  (b_w, b_h) f2) = delay b_mat 
--     get (i, j) = VU.foldr (\e prev -> e + prev) (0) 
--                              $ VU.map (\k -> 
--                                         (*) (f1 (i, k)) 
--                                             (f2 (k, j))) 
--                                              (VU.enumFromN 0 (a_w - 1)) 



(#+) :: (Sparse r ty a, Num a) 
     => SparseData r ty a -> SparseData r ty a -> SparseData r D a
(#+) = zipWith (+)


add :: (Sparse r ty a, Num a) 
    => SparseData r ty a -> SparseData r ty a -> SparseData r D a
add = (#+)


(#-) :: (Sparse r ty a, Num a) 
     => SparseData r ty a -> SparseData r ty a -> SparseData r D a 
(#-) = zipWith (-)


minus :: (Sparse r ty a, Num a) 
      => SparseData r ty a -> SparseData r ty a -> SparseData r D a
minus = (#-)

scale :: (Sparse r ty a, Num a) 
      => a -> SparseData r ty a -> SparseData r D a 
scale !n = map (* n)






