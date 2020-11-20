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
import qualified SparseBlas.Data.Matrix.Parallel.Sparse.COO as O 
import Control.Parallel.Strategies ( using, NFData )
import Data.Vector.Strategies (parVector )
import Control.DeepSeq 
import Data.Array.Repa.Eval (fillChunkedP, fillLinearS)
import System.IO.Unsafe (unsafePerformIO)

import SparseBlas.Data.Matrix.Parallel.Generic.Generic as SGeneric
    ( Undelay(..),
      Sparse(s_dims, s_index, SparseData),
      SparseData(SDelayed),
      RepIndex(D, U),
      delay,
      transpose,
      convert,
      empty,
      map,
      zipWith,
      add,
      minus,
      scale )

data DNS 
instance (NFData e, Num e, Eq e, V.Unbox e) => Sparse DNS U e where
    data SparseData DNS U e = DNS { vals :: V.Vector e, width :: {-# UNPACK #-} !Int, height :: {-# UNPACK #-} !Int }

    {-# INLINE s_index #-}
    s_index (DNS vals w _) (r, c) = vals V.! (r * w + c) 
    {-# INLINE s_dims #-}
    s_dims (DNS vals w h)         = (w, h)


instance (NFData e, Num e, Eq e, Sparse DNS D e, Sparse DNS U e) => Undelay DNS e where  
    {-# INLINE s_undelay #-}
    s_undelay (SDelayed (h, w) func) = DNS vals w h 
      where 
          vals = v `deepseq` v     
          v    = unsafePerformIO $ do 
                             (vec :: VM.IOVector e) <- VM.new (w*h) 
                             fillChunkedP (w*h) (VM.unsafeWrite vec) (\n -> let (!r, !c) = n `divMod` w in func (r, c))
                             v  <- V.unsafeFreeze vec 
                             return v 


        -- vals = (V.unfoldrN (w * h) (\n -> let (r, c) =  n `divMod` w 
        --                                  in if r < h then Just (func (r, c), n+1)
        --                                     else Nothing) 0) `using` (parVector 4) 
    non_zeros (DNS vals w h) = vals 


-- vals = do 
    -- vec <- newMVec 
    -- fillChunkedP (w*h) (unsafeWriteMVec vec) (func) 
    -- unsafeFreeze vec 

instance (Undelay DNS e) => Eq (SparseData DNS U e) where 
    {-# INLINE (==) #-}
    arr1@(DNS vals w h) == arr2@(DNS vals' w' h') = and [vals == vals', w == w', h == h']



instance (Eq (SparseData DNS U e), Undelay DNS e) => Eq (SparseData DNS D e) where 
    {-# INLINE (==) #-}
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)


instance NFData e => NFData (SparseData DNS U e) where 
    {-# INLINE rnf #-}
    rnf (DNS vals w h) = let ((), (), ()) = (rnf vals, rnf w, rnf h) in (DNS vals w h) `seq` ()

instance (Show e, Undelay DNS e, Sparse DNS ty e) => Show (SparseData DNS ty e) where 
  show arr = let darr = SparseBlas.Data.Matrix.Parallel.Dense.DENSE.delay arr in 
              case s_undelay darr of 
                DNS vals h w ->  unlines ["DENSE", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n"
                                                        ]


delay :: (NFData e, Num e, Eq e, Sparse DNS ty e) 
      => SparseData DNS ty e -> SparseData DNS D e 
{-# INLINE delay #-}
delay = SGeneric.delay


transpose :: (NFData e, Sparse DNS ty e) 
          => SparseData DNS ty e -> SparseData DNS D e
{-# INLINE transpose #-}
transpose = SGeneric.transpose


convert :: Sparse r2 D e 
        => SparseData DNS D e -> SparseData r2 D e 
{-# INLINE convert #-}
convert  = SGeneric.convert


empty :: Sparse DNS ty a => SparseData DNS ty a -> Bool 
{-# INLINE empty #-}
empty = SGeneric.empty 


map :: Sparse DNS ty e 
    => (e -> b) -> SparseData DNS ty e -> SparseData DNS D b 
{-# INLINE map #-}
map = SGeneric.map 


zipWith :: (Sparse DNS ty a, Sparse DNS ty1 b, ty ~ ty1) 
        => (a -> b -> c) -> SparseData DNS ty a 
        -> SparseData DNS ty1 b -> SparseData DNS D c
{-# INLINE zipWith #-}
zipWith = SGeneric.zipWith


(#+) :: (Sparse DNS ty a, Num a)  
     => SparseData DNS ty a -> SparseData DNS ty a -> SparseData DNS D a
{-# INLINE (#+) #-}
(#+) = SGeneric.add 

(#-) :: (Sparse DNS ty a, Num a) 
     => SparseData DNS ty a -> SparseData DNS ty a -> SparseData DNS D a
{-# INLINE (#-) #-}
(#-) = SGeneric.minus 


scale :: (Sparse DNS ty a, Num a) 
      => a -> SparseData DNS ty a -> SparseData DNS D a 
{-# INLINE scale #-}
scale = SGeneric.scale 