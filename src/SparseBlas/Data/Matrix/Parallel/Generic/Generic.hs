{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes, ScopedTypeVariables 
           , UndecidableInstances, DataKinds, DeriveGeneric, QuantifiedConstraints, GADTs #-}

module SparseBlas.Data.Matrix.Parallel.Generic.Generic where 

import qualified Data.Vector.Unboxed as V 
import qualified Data.Vector.Unboxed.Mutable as VM 
import qualified Data.Vector as VB 
import Prelude hiding (map, zipWith)
import Control.DeepSeq 
import GHC.Conc (numCapabilities)
import GHC.Base (quotInt)
import Data.Array.Repa.Eval (fillChunkedP)
import System.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits 
import Data.Proxy 
import Control.Exception 

data SparseException = InvalidVectorLength 
                     | NullVector 
                     | FromVecLengthMismatch 
                     deriving Show 

instance Exception SparseException 



data RepIndex = U | D 

newtype SVector (n :: Nat) a = F (Int -> a)


to_vector :: forall n a. (KnownNat n, NFData a, V.Unbox a) => SVector n a -> V.Vector a 
{-# INLINE to_vector #-}
to_vector (F f) = vals   
       where 
            vals = v `deepseq` v    
            !len  = fromIntegral $ natVal (Proxy :: Proxy n)
            -- v    = V.generate len func 
            v    = unsafePerformIO $ do 
                                (vec :: VM.IOVector e) <- VM.new len   
                                fillChunkedP len (VM.unsafeWrite vec) f 
                                v1  <- V.unsafeFreeze vec 
                                return v1 

instance (KnownNat n, NFData a, V.Unbox a, Show a) => Show (SVector n a) where 
    show v@(F f) = show $ to_vector v 


-- from_null :: (NFData a, V.Unbox a) => V.Vector a -> Maybe (SVector 0 a) 
-- from_null v = if V.null v then Just Z else Nothing   


from_vector :: forall a n. (NFData a, V.Unbox a) => V.Vector a -> (forall n1. (KnownNat n1, n ~ n1) => SVector n1 a) 
{-# INLINE from_vector #-}
from_vector !vec = let !len = V.length vec 
                   in case someNatVal (toInteger len) of 
                        Nothing -> throw InvalidVectorLength 
                        Just l  -> if l == (SomeNat (Proxy :: Proxy n)) 
                                   then if V.null vec 
                                        then throw NullVector 
                                        else (F ((V.!) vec) :: SVector l a) 
                                   else 
                                       throw FromVecLengthMismatch 


vmap ::  (KnownNat n, NFData a, V.Unbox a) => (a -> b) -> SVector n a -> SVector n b
{-# INLINE vmap #-}
vmap f (F g) = F (f . g)




vzipWith :: (KnownNat n, NFData a, NFData a, NFData c, V.Unbox a, V.Unbox b, V.Unbox c) 
         => (a -> b -> c) -> SVector n a -> SVector n b -> SVector n c 
{-# INLINE vzipWith #-}
vzipWith f (F g) (F h) = F (\i -> f (g i) (h i)) 

(!+!) :: (KnownNat n, NFData a, Num a, V.Unbox a) 
      =>  SVector n a -> SVector n a -> SVector n a  
{-# INLINE (!+!) #-}
(!+!)  = vzipWith (+)

(!-!) :: (KnownNat n, NFData a, Num a, V.Unbox a) 
      =>  SVector n a -> SVector n a -> SVector n a 
{-# INLINE (!-!) #-}
(!-!)  = vzipWith (-)

(!*!) :: (KnownNat n, NFData a, Num a, V.Unbox a) => a -> SVector n a -> SVector n a 
{-# INLINE (!*!) #-}
(!*!) x = vmap (* x) 

vsum :: forall n a. (KnownNat n, NFData a, Num a, V.Unbox a) => SVector n a -> a 
{-# INLINE vsum #-}
vsum (F f) = let !len = fromIntegral $ natVal (Proxy :: Proxy n) 
             in  V.foldl' (\n i -> let !m = f i in n + m) 0 $ V.enumFromN 0 (len - 1) -- add parallelism 

-- vsum' :: (NFData a, Num a, V.Unbox a) => SVector a -> a 
-- {-# INLINE vsum' #-}
-- vsum' v@(f, !len) = let !v1 = to_vector v in  v1 `deepseq` (V.sum v1) 

(!.!) :: (KnownNat n, NFData a, Num a, V.Unbox a) => SVector n a -> SVector n a -> a 
{-# INLINE (!.!) #-}
(!.!) v1 = vsum . vzipWith (*) v1 

-- vdot :: (NFData a, Num a, V.Unbox a) => SVector a -> SVector a -> a 
-- {-# INLINE vdot #-}
-- vdot v1 = vsum' . vzipWith (*) v1 


veq :: (KnownNat n, NFData a, Num a, Eq a, V.Unbox a) => SVector n a -> SVector n a -> Bool 
{-# INLINE veq #-}
veq !vec1 !vec2 = V.foldr (&&) True $! V.zipWith 
                                     (==) (to_vector vec1) (to_vector vec2)



class (NFData e, Num e, Eq e, V.Unbox e, KnownNat n1, KnownNat n2) => Sparse r (ty :: RepIndex) n1 n2 e where 
    data SparseData r (ty :: RepIndex) n1 n2 e :: * 
    s_index   :: SparseData r ty n1 n2 e -> (Int, Int) -> e 
    -- s_dims    :: SparseData r ty e -> (Int, Int) 
    -- By default matVec 
    (#.)      :: SparseData r ty n1 n2 e -> SVector n2 e -> SVector n1 e 
    {-# INLINE (#.) #-}
    (#.) !mat !vec  = let delayed_mat = delay mat in delayed_mat #. vec
    {-# INLINE (#*) #-}
    (#*)      :: (Sparse r ty n2 n3 e) => SparseData r ty n1 n2 e -> SparseData r ty n2 n3 e -> SparseData r D n1 n3 e 
    (#*) !mat1 !mat2  = let (!delayed_mat1, !delayed_mat2) = (delay mat1, delay mat2) in delayed_mat1 #* delayed_mat2  

class (Sparse r D n1 n2 e, Sparse r U n1 n2 e) => Undelay r n1 n2 e where 
    s_undelay :: SparseData r D n1 n2 e      -> SparseData r U n1 n2 e 
    non_zeros :: SparseData r U n1 n2 e      -> V.Vector e 
    



instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Sparse r D n1 n2 e where 
    data SparseData r D n1 n2 e   = SDelayed ((Int, Int) -> e) 
    {-# INLINE s_index #-}
    s_index (SDelayed f) (!r, !c) = f (r, c) 
    -- {-# INLINE s_dims #-}
    -- s_dims (SDelayed (!w, !h) _)    = (w, h) 
    {-# INLINE (#.) #-}
    (#.) (SDelayed m_index_f) v = F dots 
       where 
           {-# INLINE dots #-}
           dots !ri = let g = (curry m_index_f) ri in (F g) !.! v 
    {-# INLINE (#*) #-}
    (#*) (SDelayed m_index_f1) (SDelayed m_index_f2) = SDelayed ans_index 
       where 
           {-# INLINE ans_index #-} 
           ans_index (!ri, !colj) = let (g, h) = ((\(!c) -> m_index_f1 (ri, c), (\(!r) -> m_index_f2 (r, colj))))
                                    in  (F g :: SVector n2 e) !.! (F h :: SVector n2 e)   

instance (KnownNat n1, KnownNat n2) => (NFData (SparseData r D n1 n2 e)) where 
    {-# INLINE rnf #-}
    rnf (SDelayed func) = (SDelayed func) `seq` ()  


delay :: (Sparse r ty n1 n2 e) => SparseData r ty n1 n2 e -> SparseData r D n1 n2 e 
{-# INLINE delay #-}
delay arr = SDelayed (s_index arr)


transpose :: (Sparse r ty n1 n2 e) => SparseData r ty n1 n2 e -> SparseData r D n2 n1 e
{-# INLINE transpose #-}
transpose mat = SDelayed (new_index_func mat) 
            where 
                {-# INLINE new_index_func #-}
                new_index_func m (r, c) = s_index m (c, r)
 


convert :: (Sparse r1 D n1 n2 e, Sparse r2 D n1 n2 e) 
        => SparseData r1 D n1 n2 e -> SparseData r2 D n1 n2 e 
{-# INLINE convert #-}
convert (SDelayed func) = (SDelayed func)  


manifest_convert :: (Undelay r1 n1 n2 e, Undelay r2 n1 n2 e) 
                 => SparseData r1 U n1 n2 e -> SparseData r2 U n1 n2 e 
{-# INLINE manifest_convert #-}
manifest_convert  = s_undelay . convert . delay   




-- used mainly for tests
empty :: forall n1 n2 a r ty. (KnownNat n1, KnownNat n2, Sparse r ty n1 n2 a) => SparseData r ty n1 n2 a -> Bool 
{-# INLINE empty #-}
empty mat = let 
              z1 = fromIntegral $ natVal (Proxy :: Proxy n1)
              z2 = fromIntegral $ natVal (Proxy :: Proxy n2)
            in and [z1 == 0, z2 == 0]


-- -- -------------- Polymorphic -----------------------------------------------

map :: Sparse r ty n1 n2 e => (e -> b) -> SparseData r ty n1 n2 e -> SparseData r D n1 n2 b 
{-# INLINE map #-}
map f !arr = case delay arr of 
    SDelayed g -> SDelayed (f . g) 



zipWith :: (Sparse r ty n1 n2 a, Sparse r ty n1 n2 b, r ~ r1, ty ~ ty1) 
        => (a -> b -> c) -> SparseData r ty n1 n2 a 
        -> SparseData r ty n1 n2 b -> SparseData r D n1 n2 c  
{-# INLINE zipWith #-}
zipWith f !arr1 !arr2 = SDelayed get 
   where 
     SDelayed f1        = delay arr1 
     SDelayed f2        = delay arr2
     {-# INLINE get #-}
     get val            = f (f1 val) (f2 val)



(#+) :: (Sparse r ty n1 n2 a, Num a) 
     => SparseData r ty n1 n2 a -> SparseData r ty n1 n2 a -> SparseData r D n1 n2 a
{-# INLINE (#+) #-}
(#+) = zipWith (+)


add :: (Sparse r ty n1 n2 a, Num a) 
    => SparseData r ty n1 n2 a -> SparseData r ty n1 n2 a -> SparseData r D n1 n2 a
{-# INLINE add #-}
add = (#+)


(#-) :: (Sparse r ty n1 n2 a, Num a) 
     => SparseData r ty n1 n2 a -> SparseData r ty n1 n2 a -> SparseData r D n1 n2 a 
{-# INLINE (#-) #-}
(#-) = zipWith (-)


minus :: (Sparse r ty n1 n2 a, Num a) 
      => SparseData r ty n1 n2 a -> SparseData r ty n1 n2 a -> SparseData r D n1 n2 a
{-# INLINE minus #-}
minus = (#-)

scale :: (Sparse r ty n1 n2 a, Num a) 
      => a -> SparseData r ty n1 n2 a -> SparseData r D n1 n2 a 
{-# INLINE scale #-}
scale !n = map (* n)






