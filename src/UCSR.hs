{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds #-}


module UCSR where 

import qualified Data.Vector.Unboxed as U 

import SGeneric 



data CSR   
instance (U.Unbox e, Num e, Eq e) => Sparse CSR U e where 
    data instance SparseData CSR U e = CSR { row_offsets     :: U.Vector Int
                                          ,  col_index_csr   :: U.Vector Int
                                          ,  csr_vals        :: U.Vector e
                                          ,  csr_height      :: !Int
                                          ,  csr_width       :: !Int
                                          } 
    -- indexing is big o of maximum number of elements per row
    s_index (CSR row_offs col_index vals h w) (r, c) = let (_, a1) = U.head els in a1
                                 where
                                   to_slice = row_offs U.! r 
                                   to_start = case row_offs U.!? (r - 1) of 
                                                  Nothing -> 0 -- error ("access out of bounds here" ++ show row_offs)
                                                  Just n  -> n 
                                   vec      = U.slice to_start (to_slice - to_start) $ U.zip col_index vals 
                                   els      = U.filter (\(x, _) -> x == c) vec
    s_dims (CSR _ _ _ h w) = (w, h)
    s_undelay e (SDelayed (h, w) func) = CSR r_offs cols vals h w 
                              where 
                                  vals_r r     = U.unfoldrN w (\c -> if func (r, c) /= 0 then Just ((func (r,c), c), c + 1) else Nothing) 0  
                                  rows         = Prelude.map (\r -> vals_r r) [0..h-1]
                                  all_vals_c   = U.concat rows 
                                  r_counts     = U.fromList $ Prelude.map U.length rows 
                                  r_offs       = U.scanl (+) 0 r_counts
                                  (vals, cols) = U.unzip all_vals_c




delay :: (U.Unbox e, Num e, Eq e) => SparseData CSR U e -> SparseData CSR D e 
delay = SGeneric.delay


transpose :: (U.Unbox e, Sparse CSR ty e) => SparseData CSR ty e -> SparseData CSR D e
transpose = SGeneric.transpose


convert :: Sparse r2 D e => SparseData CSR D e -> SparseData r2 D e 
convert  = SGeneric.convert


empty :: Sparse CSR ty a => SparseData CSR ty a -> Bool 
empty = SGeneric.empty 


map :: Sparse CSR ty e => (e -> b) -> SparseData CSR ty e -> SparseData CSR D b 
map = SGeneric.map 


zipWith :: (Sparse CSR ty a, Sparse CSR ty1 b) => (a -> b -> c) -> SparseData CSR ty a -> SparseData CSR ty1 b -> SparseData CSR D c
zipWith = SGeneric.zipWith


(#+) :: (Sparse CSR ty a, Num a) => SparseData CSR ty a -> SparseData CSR ty a -> SparseData CSR D a
(#+) = SGeneric.add 

(#-) :: (Sparse CSR ty a, Num a) => SparseData CSR ty a -> SparseData CSR ty a -> SparseData CSR D a
(#-) = SGeneric.minus 



scale :: (Sparse CSR ty a, Num a) => a -> SparseData CSR ty a -> SparseData CSR D a 
scale = SGeneric.scale 

