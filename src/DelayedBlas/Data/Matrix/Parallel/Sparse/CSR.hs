{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds 
           , ScopedTypeVariables, Strict, StrictData #-}


module DelayedBlas.Data.Matrix.Parallel.Sparse.CSR where 

import qualified Data.Vector.Unboxed as V  
import Control.DeepSeq 


import DelayedBlas.Data.Matrix.Parallel.Generic.Generic as DGeneric
    -- ( Undelay(..),
    --   Sparse(s_dims, s_index, SparseData),
    --   SparseData(SDelayed),
    --   RepIndex(D, U),
    --   delay,
    --   transpose,
    --   convert,
    --   manifest_convert,
    --   empty,
    --   map,
    --   zipWith,
    --   add,
    --   minus,
    --   scale ) 
import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.COO as O 
import Control.Parallel.Strategies 
import Data.Vector.Strategies
import GHC.TypeLits 
import Data.Proxy



data CSR   
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Matrix CSR U n1 n2 e where 
    data instance MatrixData CSR U n1 n2 e = CSR { row_offsets     :: V.Vector Int
                                                ,  col_index_csr   :: V.Vector Int
                                                ,  csr_vals        :: V.Vector e
                                          -- ,  csr_height      :: !Int
                                          -- ,  csr_width       :: !Int
                                                 } 
    -- indexing is big o of maximum number of elements per row
    s_index (CSR row_offs col_index vals) (r, c) = el
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


instance NFData (MatrixData CSR U n1 n2 e) where 
  rnf (CSR rows cols vals) = let ((), (), ()) = (rnf rows, rnf cols, rnf vals) in (CSR rows cols vals) `seq` ()

instance (Matrix CSR D n1 n2 e, Matrix CSR U n1 n2 e) => Undelay CSR n1 n2 e where  
    s_undelay (SDelayed func) = CSR r_offs cols vals
                                 where 
                                   vals_r r = (V.unfoldrN w (\c -> 
                                                    if func (r, c) /= 0 
                                                    then Just ((func (r,c), c), c + 1) 
                                                    else Nothing) 0) 
                                   rows      = parMap rpar (\r -> 
                                                               vals_r r) 
                                                             [0..h-1]
                                   all_vals_c   = (V.concat rows) 
                                   r_counts     = (V.fromList 
                                                  $ Prelude.map V.length 
                                                                rows) 
                                   r_offs       = (V.scanl (+) 0 r_counts) 
                                   (vals, cols) = V.unzip all_vals_c
                                   h            = fromIntegral $ natVal (Proxy :: Proxy n1)
                                   w            = fromIntegral $ natVal (Proxy :: Proxy n2)
    non_zeros (CSR _ _ vals) = vals 


instance (Matrix O.COO U n1 n2 e, Undelay CSR n1 n2 e) => Eq (MatrixData CSR U n1 n2 e) where 
  arr1 == arr2 = arr1_coo == arr2_coo 
          where 
            (arr1_coo :: MatrixData O.COO U n1 n2 e) = manifest_convert arr1 
            (arr2_coo :: MatrixData O.COO U n1 n2 e) = manifest_convert arr2

instance (Eq (MatrixData CSR U n1 n2 e), Undelay CSR n1 n2 e) => Eq (MatrixData CSR D n1 n2 e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)


instance (Show e, Undelay CSR n1 n2 e, Matrix CSR ty n1 n2 e) => Show (MatrixData CSR ty n1 n2 e) where 
  show arr = let darr = DelayedBlas.Data.Matrix.Parallel.Sparse.CSR.delay arr in 
              case s_undelay darr of 
                CSR offs cols vals ->  unlines ["CSR", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n" 
                                                        , "row offsets: "
                                                        , "\n", show offs, "\n"
                                                        , "columns: "
                                                        , "\n", show cols]
          where 
            w = fromIntegral $ natVal (Proxy :: Proxy n1) 
            h = fromIntegral $ natVal (Proxy :: Proxy n2) 




delay :: (Matrix CSR ty n1 n2 e) 
      => MatrixData CSR ty n1 n2 e -> MatrixData CSR D n1 n2 e 
delay = DGeneric.delay


transpose :: (Matrix CSR ty n1 n2 e) 
          => MatrixData CSR ty n1 n2 e -> MatrixData CSR D n2 n1 e
transpose = DGeneric.transpose


convert :: Matrix r2 D n1 n2 e 
        => MatrixData CSR D n1 n2 e -> MatrixData r2 D n1 n2 e 
convert  = DGeneric.convert



map :: Matrix CSR ty n1 n2 e 
    => (e -> b) -> MatrixData CSR ty n1 n2 e -> MatrixData CSR D n1 n2 b 
map = DGeneric.map 


zipWith :: (Matrix CSR ty n1 n2 a, Matrix CSR ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c) -> MatrixData CSR ty n1 n2 a 
        -> MatrixData CSR ty1 n1 n2 b -> MatrixData CSR D n1 n2 c
zipWith = DGeneric.zipWith


(#+) :: (Matrix CSR ty n1 n2 a)  
     => MatrixData CSR ty n1 n2 a -> MatrixData CSR ty n1 n2 a -> MatrixData CSR D n1 n2 a
(#+) = DGeneric.add 

(#-) :: (Matrix CSR ty n1 n2 a) 
     => MatrixData CSR ty n1 n2 a -> MatrixData CSR ty n1 n2 a -> MatrixData CSR D n1 n2 a
(#-) = DGeneric.minus 



scale :: (Matrix CSR ty n1 n2 a) 
      => a -> MatrixData CSR ty n1 n2 a -> MatrixData CSR D n1 n2 a 
scale = DGeneric.scale 

