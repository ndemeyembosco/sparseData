{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds 
           , ScopedTypeVariables, Strict, StrictData #-}


module SparseBlas.Data.Matrix.Parallel.Sparse.CSR where 

import qualified Data.Vector as V  
import Control.DeepSeq 


import SparseBlas.Data.Matrix.Parallel.Generic.Generic as SGeneric
    ( Undelay(..),
      Sparse(s_dims, s_index, SparseData),
      SparseData(SDelayed),
      RepIndex(D, U),
      delay,
      transpose,
      convert,
      manifest_convert,
      empty,
      map,
      zipWith,
      add,
      minus,
      scale ) 
import qualified SparseBlas.Data.Matrix.Parallel.Sparse.COO as O 
import Control.Parallel.Strategies 
import Data.Vector.Strategies



data CSR   
instance (NFData e, Num e, Eq e) => Sparse CSR U e where 
    data instance SparseData CSR U e = CSR { row_offsets     :: V.Vector Int
                                          ,  col_index_csr   :: V.Vector Int
                                          ,  csr_vals        :: V.Vector e
                                          ,  csr_height      :: !Int
                                          ,  csr_width       :: !Int
                                          } 
    -- indexing is big o of maximum number of elements per row
    s_index (CSR row_offs col_index vals h w) (r, c) = el
                                 where
                                   to_slice = row_offs V.! r 
                                   to_start = case row_offs V.!? (r - 1) of 
                   -- error ("access out of bounds here" ++ show row_offs)
                                                  Nothing -> 0 
                                                  Just n  -> n 
                                   vec      = V.slice to_start 
                                                      (to_slice - to_start) 
                                                      $ V.zip col_index vals 
                                   el = case V.find (\(x, _) -> x == c) vec of 
                                               Nothing -> 0 -- error index element non-existent 
                                               Just (_, a1)  -> a1  
    s_dims (CSR _ _ _ h w) = (w, h)


instance NFData e => NFData (SparseData CSR U e) where 
  rnf (CSR rows cols vals h w) = let ((), (), (), (), ()) = (rnf rows, rnf cols, rnf vals, rnf h, rnf w) in (CSR rows cols vals h w) `seq` ()

instance (NFData e, Num e, Eq e, Sparse CSR D e, Sparse CSR U e) => Undelay CSR e where  
    s_undelay (SDelayed (h, w) func) = CSR r_offs cols vals h w 
                                 where 
                                   vals_r r = (V.unfoldrN w (\c -> 
                                                    if func (r, c) /= 0 
                                                    then Just ((func (r,c), c), c + 1) 
                                                    else Nothing) 0) `using` (parVector 2) 
                                   rows      = parMap rpar (\r -> 
                                                               vals_r r) 
                                                             [0..h-1]
                                   all_vals_c   = (V.concat rows) `using` (parVector 2) 
                                   r_counts     = (V.fromList 
                                                  $ Prelude.map V.length 
                                                                rows) `using` (parVector 2)
                                   r_offs       = (V.scanl (+) 0 r_counts) `using` (parVector 2)
                                   (vals, cols) = V.unzip all_vals_c
    non_zeros (CSR _ _ vals _ _) = vals 


instance (Sparse O.COO U e, Undelay CSR e) => Eq (SparseData CSR U e) where 
  arr1 == arr2 = arr1_coo == arr2_coo 
          where 
            (arr1_coo :: SparseData O.COO U e) = manifest_convert arr1 
            (arr2_coo :: SparseData O.COO U e) = manifest_convert arr2

instance (Eq (SparseData CSR U e), Undelay CSR e) => Eq (SparseData CSR D e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)


instance (Show e, Undelay CSR e, Sparse CSR ty e) => Show (SparseData CSR ty e) where 
  show arr = let darr = SparseBlas.Data.Matrix.Parallel.Sparse.CSR.delay arr in 
              case s_undelay darr of 
                CSR offs cols vals h w ->  unlines ["CSR", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n" 
                                                        , "row offsets: "
                                                        , "\n", show offs, "\n"
                                                        , "columns: "
                                                        , "\n", show cols]




delay :: (NFData e, Num e, Eq e, Sparse CSR ty e) 
      => SparseData CSR ty e -> SparseData CSR D e 
delay = SGeneric.delay


transpose :: (NFData e, Sparse CSR ty e) 
          => SparseData CSR ty e -> SparseData CSR D e
transpose = SGeneric.transpose


convert :: Sparse r2 D e 
        => SparseData CSR D e -> SparseData r2 D e 
convert  = SGeneric.convert


empty :: Sparse CSR ty a => SparseData CSR ty a -> Bool 
empty = SGeneric.empty 


map :: Sparse CSR ty e 
    => (e -> b) -> SparseData CSR ty e -> SparseData CSR D b 
map = SGeneric.map 


zipWith :: (Sparse CSR ty a, Sparse CSR ty1 b, ty ~ ty1) 
        => (a -> b -> c) -> SparseData CSR ty a 
        -> SparseData CSR ty1 b -> SparseData CSR D c
zipWith = SGeneric.zipWith


(#+) :: (Sparse CSR ty a, Num a)  
     => SparseData CSR ty a -> SparseData CSR ty a -> SparseData CSR D a
(#+) = SGeneric.add 

(#-) :: (Sparse CSR ty a, Num a) 
     => SparseData CSR ty a -> SparseData CSR ty a -> SparseData CSR D a
(#-) = SGeneric.minus 



scale :: (Sparse CSR ty a, Num a) 
      => a -> SparseData CSR ty a -> SparseData CSR D a 
scale = SGeneric.scale 

