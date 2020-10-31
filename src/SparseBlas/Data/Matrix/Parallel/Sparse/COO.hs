{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes
           , InstanceSigs
           , EmptyDataDecls
           , ScopedTypeVariables 
           , UndecidableInstances, DataKinds, Strict, StrictData #-}


module SparseBlas.Data.Matrix.Parallel.Sparse.COO where 

import qualified Data.Vector as V 
import Control.Parallel.Strategies
import Data.Vector.Strategies
import Prelude hiding (zipWith)
import Control.DeepSeq 

import SparseBlas.Data.Matrix.Parallel.Generic.Generic as SGeneric
    ( Undelay(..),
      Sparse(s_dims, s_index, SparseData),
      SparseData(SDelayed),
      RepIndex(D, U),
      delay,
      transpose,
      convert,
      empty,
      map,
      zipWith,
      add,
      minus,
      scale )

-- data U
--------------- Unboxed --------------------

data COO 
--------------- COO ------------------------
instance (NFData e, Num e, Eq e) => Sparse COO U e where 
    data SparseData COO U e   = COO 
                        {  coo_vals :: V.Vector (e, Int, Int)
                           ,  width :: !Int
                           , height :: !Int
                        }
    -- indexing is big o length of non zeros
    s_index (COO vals w h) (r, c) = els
     where 
       els = case V.find (\(a, x, y) -> and [x == r, y == c]) vals of 
                Nothing -> 0 --error "index element non-existent"
                Just (a1, _, _) -> a1
    s_dims (COO vals w h) = (w, h)


instance NFData e => NFData (SparseData COO U e) where 
  rnf (COO vals w h) = let ((), (), ()) = (rnf vals, rnf w, rnf h) in (COO vals w h) `seq` ()
    
instance (NFData e, Num e, Eq e, Sparse COO D e, Sparse COO U e) => Undelay COO e where  
    s_undelay (SDelayed (h, w) func) = COO vals w h 
      where 
        vals_r r = (V.unfoldrN w (\c -> 
                                  if func (r, c) /= 0 
                                  then Just ((func (r,c), c), c + 1) 
                                  else Nothing) 0) `using` (parVector 1)
        rows     = parMap rseq (\r -> V.map (\(x, c) -> (x, r, c)) (vals_r r)) [0..h-1]
        vals     = (V.concat rows) `using` (parVector 1)
    non_zeros (COO vals w h) = let (v, _, _) = V.unzip3 vals in v  
                      


instance (Undelay COO e) => Eq (SparseData COO U e) where 
    arr1 == arr2 = (and_v v_vec) == fromIntegral (V.length v_vec)    
           where 
            v_vec        = vals_vec mat 
            and_v  l     = V.foldr (+) 0 l   
            mat          = let 
                             (interm :: SparseData COO D e) =  SparseBlas.Data.Matrix.Parallel.Sparse.COO.zipWith (\x y -> 
                                                                          if x == y 
                                                                          then fromInteger 1 
                                                                          else 0) arr1 arr2
                           in (s_undelay :: SparseData COO D e -> SparseData COO U e) interm  
            vals_vec m   = V.map (\(a, _, _) -> a) (coo_vals m)


instance (Undelay COO e, Eq (SparseData COO U e)) => Eq (SparseData COO D e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)
    

instance (Show e, Undelay COO e, Sparse COO ty e) => Show (SparseData COO ty e) where 
  show arr = let darr = SparseBlas.Data.Matrix.Parallel.Sparse.COO.delay arr in 
             case s_undelay darr of 
               COO vs w h ->  unlines ["COO", "\n"
                                            , "________"
                                            , "(width, height): "
                                            , show (w, h), "\n"
                                            , "vals: "
                                            , "\n", show vs]


delay :: (NFData e, Num e, Eq e, Sparse COO ty e) => SparseData COO ty e -> SparseData COO D e 
delay = SGeneric.delay


transpose :: (NFData e, Sparse COO ty e) 
          => SparseData COO ty e -> SparseData COO D e
transpose = SGeneric.transpose


convert :: Sparse r2 D e => SparseData COO D e -> SparseData r2 D e 
convert  = SGeneric.convert


empty :: Sparse COO ty a => SparseData COO ty a -> Bool 
empty = SGeneric.empty 


map :: Sparse COO ty e => (e -> b) -> SparseData COO ty e -> SparseData COO D b 
map = SGeneric.map 


zipWith :: (Sparse COO ty a, Sparse COO ty1 b, ty ~ ty1) 
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

