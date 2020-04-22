{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds #-}


module UELL where 

import qualified Data.Vector.Unboxed as U 

import SGeneric 



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
    s_index (ELL max_e_r col_ind vals h w) (r, c) = let (_, a1) = els in a1
       where 
         to_start = r * max_e_r
         vec      = U.slice to_start max_e_r $ U.zip col_ind vals 
         Just els = U.find (\(x,_) -> x == c) vec
    s_dims (ELL _ _ _ h w) = (w, h)
    s_undelay e (SDelayed (h, w) func) = ELL r_max cols vals h w 
       where  
         vals_r r = U.unfoldrN w (\c -> 
                                    if func (r, c) /= 0 
                                    then Just ((func (r,c), c), c + 1) 
                                    else Nothing) 0
         rows         = Prelude.map (\r -> vals_r r) [0..h-1]
         all_vals_c   = U.concat rows
         r_max        = U.maximum $ U.fromList $ Prelude.map U.length rows
         (vals, cols) = U.unzip all_vals_c


delay :: (U.Unbox e, Num e, Eq e) => SparseData ELL U e -> SparseData ELL D e 
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


zipWith :: (Sparse ELL ty a, Sparse ELL ty1 b) 
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

