{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds #-}

module DelayedBlas.Data.Matrix.Generic.Generic where 
import Control.DeepSeq 
import qualified Data.Vector as VU 
import qualified Data.Vector.Unboxed as U  
import Prelude hiding (map, zipWith)


data RepIndex = U | D 

type SVector a = (Int -> a, Int) 


to_vector ::U.Unbox a => SVector a -> U.Vector a 
{-# INLINE to_vector #-}
to_vector (f, !len) = U.generate len f 


from_vector :: U.Unbox a => U.Vector a -> SVector a 
{-# INLINE from_vector #-}
from_vector !vec =  let len = U.length vec in ((U.!) vec, len)


vnull :: U.Unbox a => SVector a -> Bool 
vnull  = (== 0) . snd  

vmap :: U.Unbox a => (a -> b) -> SVector a -> SVector b
{-# INLINE vmap #-}
vmap f (g, !len) = (f . g, len)




vzipWith :: (U.Unbox a, U.Unbox a, U.Unbox c) 
         => (a -> b -> c) -> SVector a -> SVector b -> SVector c 
{-# INLINE vzipWith #-}
vzipWith f (g, !len1) (h, !len2) = (\i -> f (g i) (h i), len1) 

(!+!) :: (U.Unbox a, Num a) 
      =>  SVector a -> SVector a -> SVector a  
{-# INLINE (!+!) #-}
(!+!)  = vzipWith (+)

(!-!) :: (U.Unbox a, Num a) 
      =>  SVector a -> SVector a -> SVector a 
{-# INLINE (!-!) #-}
(!-!)  = vzipWith (-)

(!*!) :: (U.Unbox a, Num a) => a -> SVector a -> SVector a 
{-# INLINE (!*!) #-}
(!*!) x = vmap (* x) 

vsum :: (U.Unbox a, Num a) => SVector a -> a 
vsum (f, !len) = U.foldr' (\i n -> n + (f i)) 0 $ U.enumFromN 0 (len - 1) 

(!.!) :: (U.Unbox a, Num a) => SVector a -> SVector a -> a 
{-# INLINE (!.!) #-}
(!.!) v1 = vsum . vzipWith (*) v1 

vdot :: (U.Unbox a, Num a) => SVector a -> SVector a -> a
{-# INLINE vdot #-}
vdot v1 = U.sum . to_vector . vzipWith (*) v1 


veq :: (U.Unbox a, Num a, Eq a) => SVector a -> SVector a -> Bool 
{-# INLINE veq #-}
veq !vec1 !vec2 = U.foldr (&&) True $! U.zipWith 
                                     (==) (to_vector vec1) (to_vector vec2)



class (U.Unbox e, Num e, Eq e, NFData e) => Matrix r (ty :: RepIndex) e where 
    data MatrixData r (ty :: RepIndex) e :: * 
    s_index   :: MatrixData r ty e -> (Int, Int) -> e 
    s_dims    :: MatrixData r ty e -> (Int, Int) 
    -- By default matVec 
    (#.)      :: Num e => MatrixData r ty e -> SVector e -> SVector e 
    {-# INLINE (#.) #-}
    (#.) !mat !vec  = let delayed_mat = delay mat in delayed_mat #. vec

class (Matrix r D e, Matrix r U e) => Undelay r e where 
    s_undelay :: MatrixData r D e -> MatrixData r U e 
    non_zeros :: MatrixData r U e      -> U.Vector e 
    



instance (U.Unbox e, Num e, Eq e, NFData e) => Matrix r D e where 
    data MatrixData r D e         = SDelayed (Int, Int) ((Int, Int) -> e) 
    {-# INLINE s_index #-}
    s_index (SDelayed _ f) (!r, !c) = f (r, c) 
    {-# INLINE s_dims #-}
    s_dims (SDelayed (!w, !h) _)    = (w, h) 
    {-# INLINE (#.) #-}
    (#.) (SDelayed (!w, !h) m_index_f) v@(_, !len) = ((VU.!) part_sums, len)
       where 
         r_funcs   = (VU.map (\ri -> ((curry m_index_f) ri, w)) 
                           $ VU.enumFromN 0 h)  
         part_sums = (VU.map (\(g, w) -> (g, w) !.! v) r_funcs) 



--instance (Eq e, Num e, Matrix r U e
--              , Matrix r2 U e, r ~ r2, U.Unbox e) => Eq (MatrixData COO U e) where 
--    arr1 == arr2 = (and_v v_vec) == (U.length v_vec)    
--           where 
--            v_vec        = vals_vec mat
--            and_v  l     = U.foldr (+) 0 l  
--            darr1   = delay arr1   
--            darr2   = delay arr2
--            mat          = s_undelay 0 $ zipWith (\x y -> if x == y then fromInteger 1 else 0) darr1 darr2  
--            vals_vec m   = U.map (\(a, _, _) -> a) (coo_vals m)


delay :: (Matrix r ty e, U.Unbox e) => MatrixData r ty e -> MatrixData r D e 
{-# INLINE delay #-}
delay arr = SDelayed (s_dims arr) (s_index arr)


transpose :: (Matrix r ty e, U.Unbox e) => MatrixData r ty e -> MatrixData r D e
{-# INLINE transpose #-}
transpose mat = let 
                    (w, h) = s_dims mat 
                    new_index_func m (r, c) = s_index m (c, r)
                in SDelayed (h, w) (new_index_func mat) 


convert :: (Matrix r1 D e, Matrix r2 D e) 
        => MatrixData r1 D e -> MatrixData r2 D e 
{-# INLINE convert #-}
convert (SDelayed (w, h) func) = (SDelayed (w, h) func)  


manifest_convert :: (Undelay r1 e, Undelay r2 e) 
                 => MatrixData r1 U e -> MatrixData r2 U e 
{-# INLINE manifest_convert #-}
manifest_convert  = s_undelay . convert . delay   





empty :: (Matrix r ty a, Undelay r a) => MatrixData r ty a -> Bool 
{-# INLINE empty #-}
empty mat = (0, 0) == (s_dims mat) || (U.null $ non_zeros $ s_undelay dmat)
     where 
       dmat = delay mat 

-------------- Polymorphic -----------------------------------------------

map :: Matrix r ty e => (e -> b) -> MatrixData r ty e -> MatrixData r D b 
{-# INLINE map #-}
map f !arr = case delay arr of 
    SDelayed (w, h) g -> SDelayed (w, h) (f . g) 



zipWith :: (Matrix r ty a, Matrix r1 ty1 b, r ~ r1, ty ~ ty1) 
        => (a -> b -> c) -> MatrixData r ty a 
        -> MatrixData r1 ty1 b -> MatrixData r D c  
{-# INLINE zipWith #-}
zipWith f !arr1 !arr2 = SDelayed (w1, h1) get 
   where 
     SDelayed (w1, h1) f1 = delay arr1 
     SDelayed _ f2        = delay arr2
     {-# INLINE get #-}
     get val              = f (f1 val) (f2 val)

-- (#*) :: (Matrix r ty a, Num a) 
--     => MatrixData r ty a -> MatrixData r ty a -> MatrixData r D a
-- (#*) !a_mat !b_mat = SDelayed (a_w, b_h) get 
--   where 
--     mat1@(SDelayed  (a_w, a_h) f1) = delay a_mat 
--     mat2@(SDelayed  (b_w, b_h) f2) = delay b_mat 
--     get (i, j) = VU.foldr (\e prev -> e + prev) (0) 
--                              $ VU.map (\k -> 
--                                         (*) (f1 (i, k)) 
--                                             (f2 (k, j))) 
--                                              (VU.enumFromN 0 (a_w - 1)) 



(#+) :: (Matrix r ty a, Num a) 
     => MatrixData r ty a -> MatrixData r ty a -> MatrixData r D a
{-# INLINE (#+) #-}
(#+) = zipWith (+)


add :: (Matrix r ty a, Num a) 
    => MatrixData r ty a -> MatrixData r ty a -> MatrixData r D a 
{-# INLINE add #-}
add = (#+)


(#-) :: (Matrix r ty a, Num a) 
     => MatrixData r ty a -> MatrixData r ty a -> MatrixData r D a 
{-# INLINE (#-) #-}
(#-) = zipWith (-)


minus :: (Matrix r ty a, Num a) 
      => MatrixData r ty a -> MatrixData r ty a -> MatrixData r D a
{-# INLINE minus #-}
minus = (#-)

scale :: (Matrix r ty a, Num a) 
      => a -> MatrixData r ty a -> MatrixData r D a 
{-# INLINE scale #-}
scale !n = map (* n)





