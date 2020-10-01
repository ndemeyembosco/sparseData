{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds 
           , ScopedTypeVariables #-}




module Data.Matrix.Dense.DENSE where 

import qualified Data.Vector.Unboxed as U 
import qualified Data.Matrix.Sparse.COO as O 

import Data.Matrix.Generic 

data DNS 
instance (U.Unbox e, Num e, Eq e) => Sparse DNS U e where
    data SparseData DNS U e = DNS { vals :: U.Vector e, width :: !Int, height :: !Int }

    s_index (DNS vals w _) (r, c) = vals U.! (r * w + c) 
    s_dims (DNS vals w h)         = (w, h)


instance (U.Unbox e, Num e, Eq e, Sparse DNS D e, Sparse DNS U e) => Undelay DNS e where  
    s_undelay (SDelayed (h, w) func) = DNS vals w h 
      where 
        vals = U.unfoldrN (w * h) (\n -> let (r, c) =  n `divMod` w 
                                         in if r < h then Just (func (r, c), n+1)
                                            else Nothing) 0
    non_zeros (DNS vals w h) = vals 


instance (Undelay DNS e) => Eq (SparseData DNS U e) where 
    arr1@(DNS vals w h) == arr2@(DNS vals' w' h') = and [vals == vals', w == w', h == h']



instance (Eq (SparseData DNS U e), Undelay DNS e) => Eq (SparseData DNS D e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)


instance (Show e, Undelay DNS e, Sparse DNS ty e) => Show (SparseData DNS ty e) where 
  show arr = let darr = UDNS.delay arr in 
              case s_undelay darr of 
                DNS vals h w ->  unlines ["DENSE", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n"
                                                        ]


delay :: (U.Unbox e, Num e, Eq e, Sparse DNS ty e) 
      => SparseData DNS ty e -> SparseData DNS D e 
delay = SGeneric.delay


transpose :: (U.Unbox e, Sparse DNS ty e) 
          => SparseData DNS ty e -> SparseData DNS D e
transpose = SGeneric.transpose


convert :: Sparse r2 D e 
        => SparseData DNS D e -> SparseData r2 D e 
convert  = SGeneric.convert


empty :: Sparse DNS ty a => SparseData DNS ty a -> Bool 
empty = SGeneric.empty 


map :: Sparse DNS ty e 
    => (e -> b) -> SparseData DNS ty e -> SparseData DNS D b 
map = SGeneric.map 


zipWith :: (Sparse DNS ty a, Sparse DNS ty1 b, ty ~ ty1) 
        => (a -> b -> c) -> SparseData DNS ty a 
        -> SparseData DNS ty1 b -> SparseData DNS D c
zipWith = SGeneric.zipWith


(#+) :: (Sparse DNS ty a, Num a)  
     => SparseData DNS ty a -> SparseData DNS ty a -> SparseData DNS D a
(#+) = SGeneric.add 

(#-) :: (Sparse DNS ty a, Num a) 
     => SparseData DNS ty a -> SparseData DNS ty a -> SparseData DNS D a
(#-) = SGeneric.minus 


scale :: (Sparse DNS ty a, Num a) 
      => a -> SparseData DNS ty a -> SparseData DNS D a 
scale = SGeneric.scale 