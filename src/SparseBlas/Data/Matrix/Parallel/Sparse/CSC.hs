{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds 
           , ScopedTypeVariables #-}


module SparseBlas.Data.Matrix.Parallel.Sparse.CSC where 

import qualified SparseBlas.Data.Matrix.Parallel.Sparse.COO as O 
import qualified SparseBlas.Data.Matrix.Parallel.Dense.DENSE as D 
import qualified Data.Vector.Unboxed as V 
import Control.Parallel.Strategies 
import Data.Vector.Strategies
import Control.DeepSeq 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic as SGeneric

import GHC.TypeLits 
import Data.Proxy



data CSC 
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Sparse CSC U n1 n2 e where 
    data instance SparseData CSC U n1 n2 e = CSC {
                                                col_offsets   :: V.Vector Int 
                                              , row_index_csc :: V.Vector Int 
                                              , csc_vals      :: V.Vector e 
                                            }

    s_index (CSC col_offs row_index vals) (r, c) = el 
                                    where 
                                        to_slice = col_offs V.! r 
                                        to_start = case col_offs V.!? (r - 1) of 
                                                         Nothing -> 0 
                                                         Just n  -> n 
                                        vec      = V.slice to_start 
                                                           (to_slice - to_start)
                                                           $ V.zip row_index vals 
                                        el  = case V.find (\(x, _) -> x == r) vec of 
                                                     Nothing -> 0 
                                                     Just (_, a1) -> a1 


instance NFData (SparseData CSC U n1 n2 e) where 
  rnf (CSC cols rows vals) = let ((), (), ()) = (rnf cols, rnf rows, rnf vals) in (CSC cols rows vals) `seq` ()


instance (Sparse CSC D n1 n2 e, Sparse CSC U n1 n2 e) => Undelay CSC n1 n2 e where  
    s_undelay (SDelayed func) = CSC c_offs rows vals 
                                 where 
                                   vals_r c = (V.unfoldrN w (\r -> 
                                                    if func (r, c) /= 0 
                                                    then Just ((func (r,c), c), c + 1) 
                                                    else Nothing) 0) 
                                   cols      = parMap rpar (\c -> 
                                                               vals_r c) 
                                                             [0..w-1]
                                   all_vals_r   = (V.concat cols) 
                                   c_counts     = (V.fromList 
                                                  $ Prelude.map V.length 
                                                                cols) 
                                   c_offs       = (V.scanl (+) 0 c_counts) 
                                   (vals, rows) = V.unzip all_vals_r
                                   w            = fromIntegral $ natVal (Proxy :: Proxy n1)
                                   h            = fromIntegral $ natVal (Proxy :: Proxy n2)
    non_zeros (CSC _ _ vals) = vals 


instance (Sparse D.DNS U n1 n2 e, Undelay CSC n1 n2 e) => Eq (SparseData CSC U n1 n2 e) where 
  arr1 == arr2 = arr1_coo == arr2_coo 
          where 
            (arr1_coo :: SparseData O.COO U n1 n2 e) = manifest_convert arr1 
            (arr2_coo :: SparseData O.COO U n1 n2 e) = manifest_convert arr2


instance (Eq (SparseData CSC U n1 n2 e), Undelay CSC n1 n2 e) => Eq (SparseData CSC D n1 n2 e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)


instance (Show e, Undelay CSC n1 n2 e, Sparse CSC ty n1 n2 e) => Show (SparseData CSC ty n1 n2 e) where 
  show arr = let darr = SparseBlas.Data.Matrix.Parallel.Sparse.CSC.delay arr in 
              case s_undelay darr of 
                CSC offs rows vals ->  unlines ["CSC", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n" 
                                                        , "column offsets: "
                                                        , "\n", show offs, "\n"
                                                        , "rows: "
                                                        , "\n", show rows]

          where 
            w = fromIntegral $ natVal (Proxy :: Proxy n1) 
            h = fromIntegral $ natVal (Proxy :: Proxy n2)

delay :: (Sparse CSC ty n1 n2 e) 
      => SparseData CSC ty n1 n2 e -> SparseData CSC D n1 n2 e 
delay = SGeneric.delay


transpose :: (Sparse CSC ty n1 n2 e) 
          => SparseData CSC ty n1 n2 e -> SparseData CSC D n2 n1 e
transpose = SGeneric.transpose


convert :: Sparse r2 D n1 n2 e 
        => SparseData CSC D n1 n2 e -> SparseData r2 D n1 n2 e 
convert  = SGeneric.convert


map :: Sparse CSC ty n1 n2 e 
    => (e -> b) -> SparseData CSC ty n1 n2 e -> SparseData CSC D n1 n2 b 
map = SGeneric.map 


zipWith :: (Sparse CSC ty n1 n2 a, Sparse CSC ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c) -> SparseData CSC ty n1 n2 a 
        -> SparseData CSC ty1 n1 n2 b -> SparseData CSC D n1 n2 c
zipWith = SGeneric.zipWith


(#+) :: (Sparse CSC ty n1 n2 a)  
     => SparseData CSC ty n1 n2 a -> SparseData CSC ty n1 n2 a -> SparseData CSC D n1 n2 a
(#+) = SGeneric.add 

(#-) :: (Sparse CSC ty n1 n2 a) 
     => SparseData CSC ty n1 n2 a -> SparseData CSC ty n1 n2 a -> SparseData CSC D n1 n2 a
(#-) = SGeneric.minus 



scale :: (Sparse CSC ty n1 n2 a) 
      => a -> SparseData CSC ty n1 n2 a -> SparseData CSC D n1 n2 a 
scale = SGeneric.scale 
