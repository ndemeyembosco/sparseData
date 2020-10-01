{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes
           , ScopedTypeVariables  
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds, Strict, StrictData #-}


module Data.Matrix.Sparse.ELL where 

import qualified Data.Vector.Unboxed as U 

import Data.Matrix.Generic 
import qualified Data.Matrix.Sparse.COO as O



data ELL   
instance (U.Unbox e, Num e, Eq e) => Sparse ELL U e where 
    data instance SparseData ELL U e = ELL 
                                       { max_elem_row    :: !Int
                                         , col_index_ell :: U.Vector Int
                                         , ell_vals      :: U.Vector e
                                         , ell_height    :: !Int
                                         , ell_width     :: !Int
                                        } 
    -- indexing is big o of maximum number of elements per row
    s_index (ELL max_e_r col_ind vals h w) (r, c) = el 
       where 
         to_start = r  * max_e_r
         vec      = U.slice to_start max_e_r  $ U.zip col_ind vals -- is slicing by max_elem per row too
         el = case U.find (\(x,_) -> x == c) vec of 
                          Nothing      -> 0 
                          Just (_, a1) -> a1 
    s_dims (ELL _ _ _ h w) = (w, h)

instance (U.Unbox e, Num e, Eq e, Sparse ELL D e, Sparse ELL U e) => Undelay ELL e where 
    s_undelay (SDelayed (h, w) func) = ELL r_max cols vals h w 
       where  
         vals_r r = U.unfoldrN w (\c -> 
                                    if func (r, c) /= 0 
                                    then Just ((func (r,c), c), c + 1) 
                                    else Just ((0, c), c + 1)) 0
         rows         = Prelude.map (\r -> vals_r r) [0..h-1]
         all_vals_c   = U.concat rows
         r_max        = let len_list = Prelude.map U.length rows 
                        in if not $ Prelude.null len_list
                             then Prelude.maximum len_list
                             else 0  
         (vals, cols) = U.unzip all_vals_c
    non_zeros (ELL _ _ vals _ _) = vals 


instance (Sparse O.COO U e, Undelay ELL e) => Eq (SparseData ELL U e) where 
  arr1 == arr2 = arr1_coo == arr2_coo 
          where 
            (arr1_coo :: SparseData O.COO U e) = manifest_convert arr1 
            (arr2_coo :: SparseData O.COO U e) = manifest_convert arr2

instance (Eq (SparseData ELL U e), Undelay ELL e) => Eq (SparseData ELL D e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)



instance (Show e, Undelay ELL e, Sparse ELL ty e) => Show (SparseData ELL ty e) where 
  show arr = let darr = UELL.delay arr in 
              case s_undelay darr of 
                ELL max_elem cols vals h w ->  unlines ["ELL", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n" 
                                                        , "maximum elemements per row: "
                                                        , "\n", show max_elem, "\n"
                                                        , "columns: "
                                                        , "\n", show cols]


delay :: (U.Unbox e, Num e, Eq e, Sparse ELL ty e) => SparseData ELL ty e -> SparseData ELL D e 
delay = SGeneric.delay


transpose :: (U.Unbox e, Sparse ELL ty e) 
          => SparseData ELL ty e -> SparseData ELL D e
transpose = SGeneric.transpose


convert :: Sparse r2 D e => SparseData ELL D e -> SparseData r2 D e 
convert  = SGeneric.convert


empty :: Sparse ELL ty a => SparseData ELL ty a -> Bool 
empty = SGeneric.empty 


map :: Sparse ELL ty e => (e -> b) -> SparseData ELL ty e -> SparseData ELL D b 
map = SGeneric.map 


zipWith :: (Sparse ELL ty a, Sparse ELL ty1 b, ty ~ ty1) 
        => (a -> b -> c)        -> SparseData ELL ty a 
        -> SparseData ELL ty1 b -> SparseData ELL D c
zipWith = SGeneric.zipWith


(#+) :: (Sparse ELL ty a, Num a) 
     => SparseData ELL ty a -> SparseData ELL ty a -> SparseData ELL D a
(#+) = SGeneric.add 

(#-) :: (Sparse ELL ty a, Num a) 
     => SparseData ELL ty a -> SparseData ELL ty a -> SparseData ELL D a
(#-) = SGeneric.minus 



scale :: (Sparse ELL ty a, Num a) 
      => a -> SparseData ELL ty a -> SparseData ELL D a 
scale = SGeneric.scale 

