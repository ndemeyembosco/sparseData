{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds 
           , ScopedTypeVariables #-}




module DelayedBlas.Data.Matrix.Parallel.Dense.DENSE where 

import qualified Data.Vector.Unboxed as V  
import Control.DeepSeq ( NFData(..), deepseq ) 
import GHC.TypeLits ( KnownNat, natVal ) 
import Data.Array.Repa.Eval (fillChunkedP)
import System.IO.Unsafe (unsafePerformIO)
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector.Unboxed.Mutable as VM 


import DelayedBlas.Data.Matrix.Parallel.Generic.Generic as DGeneric
    ( Undelay(..),
      Matrix(s_index, MatrixData),
      MatrixData(SDelayed),
      RepIndex(D, U),
      delay,
      transpose,
      convert,
      map,
      zipWith,
      add,
      minus,
      scale )


data DNS 
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Matrix DNS U n1 n2 e where
    data MatrixData DNS U n1 n2 e = DNS { vals :: V.Vector e }
    s_index (DNS !vals) (!r, !c)  = vals V.! (r * w + c)
                           where 
                              !w = fromIntegral $! natVal (Proxy :: Proxy n1)
                              -- h = fromIntegral $ natVal (Proxy :: Proxy n2)


instance (Matrix DNS D n1 n2 e, Matrix DNS U n1 n2 e) => Undelay DNS n1 n2 e where  
    {-# INLINABLE s_undelay #-}
    s_undelay (SDelayed func) = DNS vals
      where 
          vals = v `deepseq` v 
          w    = fromIntegral $ natVal (Proxy :: Proxy n1) 
          h    = fromIntegral $ natVal (Proxy :: Proxy n2)    
          v    = unsafePerformIO $ do 
                             (vec :: VM.IOVector e) <- VM.new (w*h) 
                             fillChunkedP (w*h) (VM.unsafeWrite vec) (\n -> let (!r, !c) = n `divMod` w in if r < h then func (r, c) else 0)
                             V.unsafeFreeze vec  
          -- vals = (V.unfoldrN (w * h) (\n -> let (r, c) =  n `divMod` w 
          --                                in if r < h then Just (func (r, c), n+1)
          --                                   else Nothing) 0) 
    non_zeros (DNS vals) = vals 



instance (Undelay DNS n1 n2 e) => Eq (MatrixData DNS U n1 n2 e) where 
    {-# INLINE (==) #-}
    DNS vals == DNS vals' = vals == vals'



instance (Eq (MatrixData DNS U n1 n2 e), Undelay DNS n1 n2 e) => Eq (MatrixData DNS D n1 n2 e) where 
    {-# INLINE (==) #-}
    arr1 == arr2 = s_undelay arr1 == s_undelay arr2


instance NFData (MatrixData DNS U n1 n2 e) where 
    {-# INLINE rnf #-}
    rnf (DNS vals) = DNS vals `seq` ()

instance (Show e, Undelay DNS n1 n2 e, Matrix DNS ty n1 n2 e) => Show (MatrixData DNS ty n1 n2 e) where 
  show arr = let darr = DelayedBlas.Data.Matrix.Parallel.Dense.DENSE.delay arr in 
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


delay :: Matrix DNS ty n1 n2 e 
      => MatrixData DNS ty n1 n2 e -> MatrixData DNS D n1 n2 e 
{-# INLINE delay #-}
delay = DGeneric.delay


transpose :: Matrix DNS ty n1 n2 e
          => MatrixData DNS ty n1 n2 e -> MatrixData DNS D n2 n1 e
{-# INLINE transpose #-}
transpose = DGeneric.transpose


convert :: Matrix r2 D n1 n2 e 
        => MatrixData DNS D n1 n2 e -> MatrixData r2 D n1 n2 e 
{-# INLINE convert #-}
convert  = DGeneric.convert


map :: Matrix DNS ty n1 n2 e 
    => (e -> b) -> MatrixData DNS ty n1 n2 e -> MatrixData DNS D n1 n2 b 
{-# INLINE map #-}
map = DGeneric.map 


zipWith :: (Matrix DNS ty n1 n2 a, Matrix DNS ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c) -> MatrixData DNS ty n1 n2 a 
        -> MatrixData DNS ty1 n1 n2 b -> MatrixData DNS D n1 n2 c
{-# INLINE zipWith #-}
zipWith = DGeneric.zipWith


(#+) :: (Matrix DNS ty n1 n2 a)  
     => MatrixData DNS ty n1 n2 a -> MatrixData DNS ty n1 n2 a -> MatrixData DNS D n1 n2 a
{-# INLINE (#+) #-}
(#+) = DGeneric.add 

(#-) :: (Matrix DNS ty n1 n2 a) 
     => MatrixData DNS ty n1 n2 a -> MatrixData DNS ty n1 n2 a -> MatrixData DNS D n1 n2 a
{-# INLINE (#-) #-}
(#-) = DGeneric.minus 


scale :: (Matrix DNS ty n1 n2 a) 
      => a -> MatrixData DNS ty n1 n2 a -> MatrixData DNS D n1 n2 a 
{-# INLINE scale #-}
scale = DGeneric.scale 