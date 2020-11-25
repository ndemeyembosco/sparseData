{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds 
           , ScopedTypeVariables #-}




module SparseBlas.Data.Matrix.Parallel.Dense.DENSE where 

import qualified Data.Vector.Unboxed as V  
import qualified Data.Vector.Unboxed.Mutable as VM 
import Control.Parallel.Strategies ( using, NFData )
import Control.DeepSeq 
import Data.Array.Repa.Eval (fillChunkedP, fillLinearS)
import System.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits 
import Data.Proxy


import SparseBlas.Data.Matrix.Parallel.Generic.Generic as SGeneric
    -- ( 
    --     Undelay(..),
    --   Sparse(s_dims, s_index, SparseData),
    --   SparseData(SDelayed),
    --   RepIndex(D, U),
    --   delay,
    --   transpose,
    --   convert,
    --   empty,
    --   map,
    --   zipWith,
    --   add,
    --   minus,
    --   scale )

data DNS 
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Sparse DNS U n1 n2 e where
    data SparseData DNS U n1 n2 e = DNS { vals :: V.Vector e }

    {-# INLINE s_index #-}
    s_index (DNS vals) (r, c) = vals V.! ((r * w + c) `mod` h) 
                           where 
                              w = fromIntegral $ natVal (Proxy :: Proxy n1)
                              h = fromIntegral $ natVal (Proxy :: Proxy n2)


instance (Sparse DNS D n1 n2 e, Sparse DNS U n1 n2 e) => Undelay DNS n1 n2 e where  
    {-# INLINE s_undelay #-}
    s_undelay (SDelayed func) = DNS vals
      where 
          vals = v `deepseq` v 
          w    = fromIntegral $ natVal (Proxy :: Proxy n1) 
          h    = fromIntegral $ natVal (Proxy :: Proxy n2)    
          v    = unsafePerformIO $ do 
                             (vec :: VM.IOVector e) <- VM.new (w*h) 
                             fillChunkedP (w*h) (VM.unsafeWrite vec) (\n -> let (!r, !c) = n `divMod` w in func (r, c))
                             v  <- V.unsafeFreeze vec 
                             return v 


        -- vals = (V.unfoldrN (w * h) (\n -> let (r, c) =  n `divMod` w 
        --                                  in if r < h then Just (func (r, c), n+1)
        --                                     else Nothing) 0) `using` (parVector 4) 
    non_zeros (DNS vals) = vals 



instance (Undelay DNS n1 n2 e) => Eq (SparseData DNS U n1 n2 e) where 
    {-# INLINE (==) #-}
    arr1@(DNS vals) == arr2@(DNS vals') = vals == vals'



instance (Eq (SparseData DNS U n1 n2 e), Undelay DNS n1 n2 e) => Eq (SparseData DNS D n1 n2 e) where 
    {-# INLINE (==) #-}
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)


instance NFData (SparseData DNS U n1 n2 e) where 
    {-# INLINE rnf #-}
    rnf (DNS vals) = (DNS vals) `seq` ()

instance (Show e, Undelay DNS n1 n2 e, Sparse DNS ty n1 n2 e) => Show (SparseData DNS ty n1 n2 e) where 
  show arr = let darr = SparseBlas.Data.Matrix.Parallel.Dense.DENSE.delay arr in 
              case s_undelay darr of 
                DNS vals ->  unlines ["DENSE", "\n"
                                        , "________"
                                        , "(height, width): "
                                        , show (h, w), "\n"
                                        , "non-zeros: "
                                        , "\n", show vals, "\n"
                                        ]
            where 
                w = fromIntegral $ natVal (Proxy :: Proxy n1)
                h = fromIntegral $ natVal (Proxy :: Proxy n2)


delay :: Sparse DNS ty n1 n2 e 
      => SparseData DNS ty n1 n2 e -> SparseData DNS D n1 n2 e 
{-# INLINE delay #-}
delay = SGeneric.delay


transpose :: Sparse DNS ty n1 n2 e
          => SparseData DNS ty n1 n2 e -> SparseData DNS D n2 n1 e
{-# INLINE transpose #-}
transpose = SGeneric.transpose


convert :: Sparse r2 D n1 n2 e 
        => SparseData DNS D n1 n2 e -> SparseData r2 D n1 n2 e 
{-# INLINE convert #-}
convert  = SGeneric.convert


map :: Sparse DNS ty n1 n2 e 
    => (e -> b) -> SparseData DNS ty n1 n2 e -> SparseData DNS D n1 n2 b 
{-# INLINE map #-}
map = SGeneric.map 


zipWith :: (Sparse DNS ty n1 n2 a, Sparse DNS ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c) -> SparseData DNS ty n1 n2 a 
        -> SparseData DNS ty1 n1 n2 b -> SparseData DNS D n1 n2 c
{-# INLINE zipWith #-}
zipWith = SGeneric.zipWith


(#+) :: (Sparse DNS ty n1 n2 a)  
     => SparseData DNS ty n1 n2 a -> SparseData DNS ty n1 n2 a -> SparseData DNS D n1 n2 a
{-# INLINE (#+) #-}
(#+) = SGeneric.add 

(#-) :: (Sparse DNS ty n1 n2 a) 
     => SparseData DNS ty n1 n2 a -> SparseData DNS ty n1 n2 a -> SparseData DNS D n1 n2 a
{-# INLINE (#-) #-}
(#-) = SGeneric.minus 


scale :: (Sparse DNS ty n1 n2 a) 
      => a -> SparseData DNS ty n1 n2 a -> SparseData DNS D n1 n2 a 
{-# INLINE scale #-}
scale = SGeneric.scale 