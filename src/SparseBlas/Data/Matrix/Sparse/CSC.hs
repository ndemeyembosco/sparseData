{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds 
           , ScopedTypeVariables #-}


module SparseBlas.Data.Matrix.Sparse.CSC where 

import qualified SparseBlas.Data.Matrix.Sparse.COO as O 
import qualified SparseBlas.Data.Matrix.Dense.DENSE as D 
import qualified Data.Vector.Unboxed as U 
import SparseBlas.Data.Matrix.Generic.Generic as SGeneric
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



data CSC 
instance (U.Unbox e, Num e, Eq e) => Sparse CSC U e where 
    data instance SparseData CSC U e = CSC {
                                          col_offsets   :: U.Vector Int 
                                        , row_index_csc :: U.Vector Int 
                                        , csc_vals      :: U.Vector e 
                                        , csc_height    :: !Int 
                                        , csc_width     :: !Int  
                                      }

    s_index (CSC col_offs row_index vals h w) (r, c) = el 
                                    where 
                                        to_slice = col_offs U.! r 
                                        to_start = case col_offs U.!? (r - 1) of 
                                                         Nothing -> 0 
                                                         Just n  -> n 
                                        vec      = U.slice to_start 
                                                           (to_slice - to_start)
                                                           $ U.zip row_index vals 
                                        el  = case U.find (\(x, _) -> x == r) vec of 
                                                     Nothing -> 0 
                                                     Just (_, a1) -> a1 
    s_dims (CSC _ _ _ h w) = (w, h)


instance (U.Unbox e, Num e, Eq e, Sparse CSC D e, Sparse CSC U e) => Undelay CSC e where  
    s_undelay (SDelayed (h, w) func) = CSC c_offs rows vals h w 
                                 where 
                                   vals_r c = U.unfoldrN w (\r -> 
                                                    if func (r, c) /= 0 
                                                    then Just ((func (r,c), c), c + 1) 
                                                    else Nothing) 0  
                                   cols      = Prelude.map (\c -> 
                                                               vals_r c) 
                                                             [0..w-1]
                                   all_vals_r   = U.concat cols 
                                   c_counts     = U.fromList 
                                                  $ Prelude.map U.length 
                                                                cols 
                                   c_offs       = U.scanl (+) 0 c_counts
                                   (vals, rows) = U.unzip all_vals_r
    non_zeros (CSC _ _ vals _ _) = vals 


instance (Sparse D.DNS U e, Undelay CSC e) => Eq (SparseData CSC U e) where 
  arr1 == arr2 = arr1_coo == arr2_coo 
          where 
            (arr1_coo :: SparseData D.DNS U e) = manifest_convert arr1 
            (arr2_coo :: SparseData D.DNS U e) = manifest_convert arr2


instance (Eq (SparseData CSC U e), Undelay CSC e) => Eq (SparseData CSC D e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)


instance (Show e, Undelay CSC e, Sparse CSC ty e) => Show (SparseData CSC ty e) where 
  show arr = let darr = SparseBlas.Data.Matrix.Sparse.CSC.delay arr in 
              case s_undelay darr of 
                CSC offs rows vals h w ->  unlines ["CSC", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n" 
                                                        , "column offsets: "
                                                        , "\n", show offs, "\n"
                                                        , "rows: "
                                                        , "\n", show rows]


delay :: (U.Unbox e, Num e, Eq e, Sparse CSC ty e) 
      => SparseData CSC ty e -> SparseData CSC D e 
delay = SGeneric.delay


transpose :: (U.Unbox e, Sparse CSC ty e) 
          => SparseData CSC ty e -> SparseData CSC D e
transpose = SGeneric.transpose


convert :: Sparse r2 D e 
        => SparseData CSC D e -> SparseData r2 D e 
convert  = SGeneric.convert


empty :: Sparse CSC ty a => SparseData CSC ty a -> Bool 
empty = SGeneric.empty 


map :: Sparse CSC ty e 
    => (e -> b) -> SparseData CSC ty e -> SparseData CSC D b 
map = SGeneric.map 


zipWith :: (Sparse CSC ty a, Sparse CSC ty1 b, ty ~ ty1) 
        => (a -> b -> c) -> SparseData CSC ty a 
        -> SparseData CSC ty1 b -> SparseData CSC D c
zipWith = SGeneric.zipWith


(#+) :: (Sparse CSC ty a, Num a)  
     => SparseData CSC ty a -> SparseData CSC ty a -> SparseData CSC D a
(#+) = SGeneric.add 

(#-) :: (Sparse CSC ty a, Num a) 
     => SparseData CSC ty a -> SparseData CSC ty a -> SparseData CSC D a
(#-) = SGeneric.minus 



scale :: (Sparse CSC ty a, Num a) 
      => a -> SparseData CSC ty a -> SparseData CSC D a 
scale = SGeneric.scale 
