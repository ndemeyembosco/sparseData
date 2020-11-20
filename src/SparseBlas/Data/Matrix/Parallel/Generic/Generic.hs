{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds, DeriveGeneric #-}

module SparseBlas.Data.Matrix.Parallel.Generic.Generic where 

import qualified Data.Vector.Unboxed as V 
import qualified Data.Vector.Unboxed.Mutable as VM 
import qualified Data.Vector as VB 
import Control.Parallel.Strategies ( NFData, using )
import Data.Vector.Strategies ( parVector )
import Prelude hiding (map, zipWith)
import Control.DeepSeq 
import GHC.Conc (numCapabilities)
import GHC.Base (quotInt)
import Data.Array.Repa.Eval (fillChunkedP)
import System.IO.Unsafe (unsafePerformIO)


data RepIndex = U | D 

type SVector a = (Int -> a, Int) 


to_vector :: (NFData a, V.Unbox a) => SVector a -> V.Vector a 
{-# INLINE to_vector #-}
to_vector (f, !len) = vals 
       where 
          vals = v `deepseq` v     
          v    = unsafePerformIO $ do 
                             (vec :: VM.IOVector e) <- VM.new len  
                             fillChunkedP len (VM.unsafeWrite vec) f 
                             v  <- V.unsafeFreeze vec 
                             return v 


from_vector :: (NFData a, V.Unbox a) => V.Vector a -> SVector a 
{-# INLINE from_vector #-}
from_vector !vec =  let len = V.length vec in ((V.!) vec, len)


vnull ::  (NFData a, V.Unbox a) => SVector a -> Bool 
{-# INLINE vnull #-}
vnull  = (== 0) . snd  

vmap ::  (NFData a, V.Unbox a) => (a -> b) -> SVector a -> SVector b
{-# INLINE vmap #-}
vmap f (g, !len) = (f . g, len)




vzipWith :: (NFData a, NFData a, NFData c, V.Unbox a, V.Unbox b, V.Unbox c) 
         => (a -> b -> c) -> SVector a -> SVector b -> SVector c 
{-# INLINE vzipWith #-}
vzipWith f (g, !len1) (h, !len2) = (\i -> f (g i) (h i), len1) 

(!+!) :: (NFData a, Num a, V.Unbox a) 
      =>  SVector a -> SVector a -> SVector a  
{-# INLINE (!+!) #-}
(!+!)  = vzipWith (+)

(!-!) :: (NFData a, Num a, V.Unbox a) 
      =>  SVector a -> SVector a -> SVector a 
{-# INLINE (!-!) #-}
(!-!)  = vzipWith (-)

(!*!) :: (NFData a, Num a, V.Unbox a) => a -> SVector a -> SVector a 
{-# INLINE (!*!) #-}
(!*!) x = vmap (* x) 

vsum :: (NFData a, Num a, V.Unbox a) => SVector a -> a 
{-# INLINE vsum #-}
vsum (f, !len) = V.foldl' (\n i -> let !m = f i in n + m) 0 $ V.enumFromN 0 (len - 1) -- add parallelism 

vsum' :: (NFData a, Num a, V.Unbox a) => SVector a -> a 
{-# INLINE vsum' #-}
vsum' v@(f, !len) = let !v1 = to_vector v in  v1 `deepseq` (V.sum v1) 

(!.!) :: (NFData a, Num a, V.Unbox a) => SVector a -> SVector a -> a 
{-# INLINE (!.!) #-}
(!.!) v1 = vsum . vzipWith (*) v1 

vdot :: (NFData a, Num a, V.Unbox a) => SVector a -> SVector a -> a 
{-# INLINE vdot #-}
vdot v1 = vsum' . vzipWith (*) v1 


veq :: (NFData a, Num a, Eq a, V.Unbox a) => SVector a -> SVector a -> Bool 
{-# INLINE veq #-}
veq !vec1 !vec2 = V.foldr (&&) True $! V.zipWith 
                                     (==) (to_vector vec1) (to_vector vec2)



class (NFData e, Num e, Eq e, V.Unbox e) => Sparse r (ty :: RepIndex) e where 
    data SparseData r (ty :: RepIndex) e :: * 
    s_index   :: SparseData r ty e -> (Int, Int) -> e 
    s_dims    :: SparseData r ty e -> (Int, Int) 
    -- By default matVec 
    (#.)      :: Num e => SparseData r ty e -> SVector e -> SVector e 
    {-# INLINE (#.) #-}
    (#.) !mat !vec  = let delayed_mat = delay mat in delayed_mat #. vec

class (Sparse r D e, Sparse r U e) => Undelay r e where 
    s_undelay :: SparseData r D e -> SparseData r U e 
    non_zeros :: SparseData r U e      -> V.Vector e 
    



instance (NFData e, Num e, Eq e, V.Unbox e) => Sparse r D e where 
    data SparseData r D e         = SDelayed (Int, Int) ((Int, Int) -> e) 
    {-# INLINE s_index #-}
    s_index (SDelayed _ f) (!r, !c) = f (r, c) 
    {-# INLINE s_dims #-}
    s_dims (SDelayed (!w, !h) _)    = (w, h) 
    {-# INLINE (#.) #-}
    (#.) (SDelayed (!w, !h) m_index_f) v@(_, !len) = (dots, len)
       where 
           {-# INLINE dots #-}
           dots !ri = let g = (curry m_index_f) ri in (g, w) !.! v 

instance (NFData (SparseData r D e)) where 
    {-# INLINE rnf #-}
    rnf (SDelayed (w, h) func) = let ( (), () ) = ( rnf w, rnf h ) in  (SDelayed (w, h) func) `seq` ()  


delay :: (Sparse r ty e) => SparseData r ty e -> SparseData r D e 
{-# INLINE delay #-}
delay arr = SDelayed (s_dims arr) (s_index arr)


transpose :: (Sparse r ty e) => SparseData r ty e -> SparseData r D e
{-# INLINE transpose #-}
transpose mat = SDelayed (h, w) (new_index_func mat) 
            where 
                (w, h) = s_dims mat 
                {-# INLINE new_index_func #-}
                new_index_func m (r, c) = s_index m (c, r)
 


convert :: (Sparse r1 D e, Sparse r2 D e) 
        => SparseData r1 D e -> SparseData r2 D e 
{-# INLINE convert #-}
convert (SDelayed (w, h) func) = (SDelayed (w, h) func)  


manifest_convert :: (Undelay r1 e, Undelay r2 e) 
                 => SparseData r1 U e -> SparseData r2 U e 
{-# INLINE manifest_convert #-}
manifest_convert  = s_undelay . convert . delay   





empty :: (Sparse r ty a, Undelay r a) => SparseData r ty a -> Bool 
{-# INLINE empty #-}
empty mat = (0, 0) == (s_dims mat) || (V.null $ non_zeros $ s_undelay dmat)
     where 
       dmat = delay mat 

-------------- Polymorphic -----------------------------------------------

map :: Sparse r ty e => (e -> b) -> SparseData r ty e -> SparseData r D b 
{-# INLINE map #-}
map f !arr = case delay arr of 
    SDelayed (w, h) g -> SDelayed (w, h) (f . g) 



zipWith :: (Sparse r ty a, Sparse r1 ty1 b, r ~ r1, ty ~ ty1) 
        => (a -> b -> c) -> SparseData r ty a 
        -> SparseData r1 ty1 b -> SparseData r D c  
{-# INLINE zipWith #-}
zipWith f !arr1 !arr2 = SDelayed (w1, h1) get 
   where 
     SDelayed (w1, h1) f1 = delay arr1 
     SDelayed _ f2        = delay arr2
     {-# INLINE get #-}
     get val              = f (f1 val) (f2 val)



(#+) :: (Sparse r ty a, Num a) 
     => SparseData r ty a -> SparseData r ty a -> SparseData r D a
{-# INLINE (#+) #-}
(#+) = zipWith (+)


add :: (Sparse r ty a, Num a) 
    => SparseData r ty a -> SparseData r ty a -> SparseData r D a
{-# INLINE add #-}
add = (#+)


(#-) :: (Sparse r ty a, Num a) 
     => SparseData r ty a -> SparseData r ty a -> SparseData r D a 
{-# INLINE (#-) #-}
(#-) = zipWith (-)


minus :: (Sparse r ty a, Num a) 
      => SparseData r ty a -> SparseData r ty a -> SparseData r D a
{-# INLINE minus #-}
minus = (#-)

scale :: (Sparse r ty a, Num a) 
      => a -> SparseData r ty a -> SparseData r D a 
{-# INLINE scale #-}
scale !n = map (* n)






