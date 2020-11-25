{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes
           , ScopedTypeVariables  
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds, Strict, StrictData #-}


module SparseBlas.Data.Matrix.Parallel.Sparse.ELL where 

import qualified Data.Vector.Unboxed as V 
import Control.Parallel.Strategies 
import Data.Vector.Strategies
import Control.DeepSeq 

import SparseBlas.Data.Matrix.Parallel.Generic.Generic as SGeneric
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
import qualified SparseBlas.Data.Matrix.Parallel.Sparse.COO as O
import GHC.TypeLits  
import Data.Proxy



data ELL   
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Sparse ELL U n1 n2 e where 
    data instance SparseData ELL U n1 n2 e = ELL 
                                              {   max_elem_row    :: !Int
                                                , col_index_ell :: V.Vector Int
                                                , ell_vals      :: V.Vector e
                                                } 
    -- indexing is big o of maximum number of elements per row
    s_index (ELL max_e_r col_ind vals) (r, c) = el 
       where 
         to_start = r  * max_e_r
         vec      = V.slice to_start max_e_r  $ V.zip col_ind vals -- is slicing by max_elem per row too
         el = case V.find (\(x,_) -> x == c) vec of 
                          Nothing      -> 0 
                          Just (_, a1) -> a1 

instance NFData (SparseData ELL U n1 n2 e) where 
  rnf (ELL max_e cols vals) = let ((), (), ()) = (rnf max_e, rnf cols, rnf vals) in (ELL max_e cols vals) `seq` () 

instance (Sparse ELL D n1 n2 e, Sparse ELL U n1 n2 e) => Undelay ELL n1 n2 e where 
    s_undelay (SDelayed func) = ELL r_max cols vals
       where  
         vals_r r = (V.unfoldrN w (\c -> 
                                    if func (r, c) /= 0 
                                    then Just ((func (r,c), c), c + 1) 
                                    else Just ((0, c), c + 1)) 0) 
         rows         = parMap rpar (\r -> vals_r r) [0..h-1]
         all_vals_c   = (V.concat rows)
         r_max        = let len_list = parMap rpar V.length rows 
                        in if not $ Prelude.null len_list
                             then Prelude.maximum len_list
                             else 0  
         (vals, cols) = V.unzip all_vals_c
         w            = fromIntegral $ natVal (Proxy :: Proxy n1)
         h            = fromIntegral $ natVal (Proxy :: Proxy n2)
    non_zeros (ELL _ _ vals) = vals 


instance (Sparse O.COO U n1 n2 e, Undelay ELL n1 n2 e) => Eq (SparseData ELL U n1 n2 e) where 
  arr1 == arr2 = arr1_coo == arr2_coo 
          where 
            (arr1_coo :: SparseData O.COO U n1 n2 e) = manifest_convert arr1 
            (arr2_coo :: SparseData O.COO U n1 n2 e) = manifest_convert arr2

instance (Eq (SparseData ELL U n1 n2 e), Undelay ELL n1 n2 e) => Eq (SparseData ELL D n1 n2 e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)



instance (Show e, Undelay ELL n1 n2 e, Sparse ELL ty n1 n2 e) => Show (SparseData ELL ty n1 n2 e) where 
  show arr = let darr = SparseBlas.Data.Matrix.Parallel.Sparse.ELL.delay arr in 
              case s_undelay darr of 
                ELL max_elem cols vals ->  unlines ["ELL", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n" 
                                                        , "maximum elemements per row: "
                                                        , "\n", show max_elem, "\n"
                                                        , "columns: "
                                                        , "\n", show cols]
          where 
            w = fromIntegral $ natVal (Proxy :: Proxy n2) 
            h = fromIntegral $ natVal (Proxy :: Proxy n1)


delay :: (Sparse ELL ty n1 n2 e) => SparseData ELL ty n1 n2 e -> SparseData ELL D n1 n2 e 
delay = SGeneric.delay


transpose :: (Sparse ELL ty n1 n2 e) 
          => SparseData ELL ty n1 n2 e -> SparseData ELL D n2 n1 e
transpose = SGeneric.transpose


convert :: Sparse r2 D n1 n2 e => SparseData ELL D n1 n2 e -> SparseData r2 D n1 n2 e 
convert  = SGeneric.convert 


map :: Sparse ELL ty n1 n2 e => (e -> b) -> SparseData ELL ty n1 n2 e -> SparseData ELL D n1 n2 b 
map = SGeneric.map 


zipWith :: (Sparse ELL ty n1 n2 a, Sparse ELL ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c)        -> SparseData ELL ty n1 n2 a 
        -> SparseData ELL ty1 n1 n2 b -> SparseData ELL D n1 n2 c
zipWith = SGeneric.zipWith


(#+) :: (Sparse ELL ty n1 n2 a) 
     => SparseData ELL ty n1 n2 a -> SparseData ELL ty n1 n2 a -> SparseData ELL D n1 n2 a
(#+) = SGeneric.add 

(#-) :: (Sparse ELL ty n1 n2 a) 
     => SparseData ELL ty n1 n2 a -> SparseData ELL ty n1 n2 a -> SparseData ELL D n1 n2 a
(#-) = SGeneric.minus 



scale :: (Sparse ELL ty n1 n2 a) 
      => a -> SparseData ELL ty n1 n2 a -> SparseData ELL D n1 n2 a 
scale = SGeneric.scale 

