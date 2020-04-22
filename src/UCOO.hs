{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes
           , EmptyDataDecls 
           , UndecidableInstances, DataKinds #-}


module UCOO where 

import qualified Data.Vector.Unboxed as U 

import SGeneric 


-- data U
--------------- Unboxed --------------------

data COO 
--------------- COO ------------------------
instance (U.Unbox e, Num e, Eq e) => Sparse COO U e where 
    data instance SparseData COO U e   = COO 
                        {  coo_vals :: U.Vector (e, Int, Int)
                           ,  width :: !Int
                           , height :: !Int
                        }
    -- indexing is big o length of non zeros
    s_index (COO vals w h) (r, c) = let (a1, _, _) = U.head els in a1 
     where 
       els = U.filter (\(a, x, y) -> and [x == r, y == c]) vals
    s_dims (COO vals w h) = (w, h)
    
    -- s_undelay :: (U.Unbox e, Eq e) 
    -- => e -> SparseData r D e -> SparseData COO U e 
    s_undelay e (SDelayed (h, w) func) = COO vals w h 
      where 
       vals = U.filter (\(el, _, _) -> el /= e) 
                         $ U.generate (h * w) (\i -> 
                                                   let 
                                                     (r1, c1) = i `divMod` h 
                                                   in (func (r1, c1), r1, c1))


delay :: (U.Unbox e, Num e, Eq e) => SparseData COO U e -> SparseData COO D e 
delay = SGeneric.delay


transpose :: (U.Unbox e, Sparse COO ty e) 
          => SparseData COO ty e -> SparseData COO D e
transpose = SGeneric.transpose


convert :: Sparse r2 D e => SparseData COO D e -> SparseData r2 D e 
convert  = SGeneric.convert


empty :: Sparse COO ty a => SparseData COO ty a -> Bool 
empty = SGeneric.empty 


map :: Sparse COO ty e => (e -> b) -> SparseData COO ty e -> SparseData COO D b 
map = SGeneric.map 


zipWith :: (Sparse COO ty a, Sparse COO ty1 b) 
        => (a -> b -> c) -> SparseData COO ty a 
        -> SparseData COO ty1 b -> SparseData COO D c
zipWith = SGeneric.zipWith


(#+) :: (Sparse COO ty a, Num a) 
     => SparseData COO ty a -> SparseData COO ty a -> SparseData COO D a
(#+) = SGeneric.add 

(#-) :: (Sparse COO ty a, Num a) 
     => SparseData COO ty a -> SparseData COO ty a -> SparseData COO D a
(#-) = SGeneric.minus 



scale :: (Sparse COO ty a, Num a) 
      => a -> SparseData COO ty a -> SparseData COO D a 
scale = SGeneric.scale 

