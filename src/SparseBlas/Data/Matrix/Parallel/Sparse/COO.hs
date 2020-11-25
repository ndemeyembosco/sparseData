{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes
           , InstanceSigs
           , EmptyDataDecls
           , ScopedTypeVariables 
           , UndecidableInstances, DataKinds, Strict, StrictData #-}


module SparseBlas.Data.Matrix.Parallel.Sparse.COO where 

import qualified Data.Vector.Unboxed as V 
import Control.Parallel.Strategies
import Data.Vector.Strategies
import Prelude hiding (zipWith)
import Control.DeepSeq 
import GHC.TypeLits 
import Data.Proxy

import SparseBlas.Data.Matrix.Parallel.Generic.Generic as SGeneric
    -- ( 
    --   Undelay(..),
    --   Sparse(s_dims, s_index, SparseData),
    --   SparseData(SDelayed),
    --   RepIndex(D, U),
    --   delay,
    --   transpose,
    --   convert,
    --   empty,
    --   map,
    --   zipWith,
    --   add,
    --   minus,
    --   scale )

-- data U
--------------- Unboxed --------------------

data COO 
--------------- COO ------------------------
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Sparse COO U n1 n2 e where 
    data SparseData COO U n1 n2 e   = COO { coo_vals :: V.Vector (e, Int, Int) }
    -- indexing is big o length of non zeros
    s_index (COO vals) (r, c) = els
     where 
       els = case V.find (\(a, x, y) -> and [x == r, y == c]) vals of 
                Nothing -> 0 --error "index element non-existent"
                Just (a1, _, _) -> a1


instance NFData (SparseData COO U n1 n2 e) where 
  rnf (COO vals) = (COO vals) `seq` vals `seq` ()
    
instance (Sparse COO D n1 n2 e, Sparse COO U n1 n2 e) => Undelay COO n1 n2 e where  
    s_undelay (SDelayed func) = COO vals
      where 
        vals_r r = (V.unfoldrN w (\c -> 
                                  if func (r, c) /= 0 
                                  then Just ((func (r,c), c), c + 1) 
                                  else Nothing) 0) 
        rows     = Prelude.map (\r -> V.map (\(x, c) -> (x, r, c)) (vals_r r)) [0..h-1]
        vals     = (V.concat rows) 
        w        = fromIntegral $ natVal (Proxy :: Proxy n1) 
        h        = fromIntegral $ natVal (Proxy :: Proxy n2) 
    non_zeros (COO vals) = let (v, _, _) = V.unzip3 vals in v  
                      


instance (Undelay COO n1 n2 e) => Eq (SparseData COO U n1 n2 e) where 
    arr1 == arr2 = (and_v v_vec) == fromIntegral (V.length v_vec)    
           where 
            v_vec        = vals_vec mat 
            and_v  l     = V.foldr (+) 0 l   
            mat          = let 
                             (interm :: SparseData COO D n1 n2 e) =  SparseBlas.Data.Matrix.Parallel.Sparse.COO.zipWith (\x y -> 
                                                                          if x == y 
                                                                          then fromInteger 1 
                                                                          else 0) arr1 arr2
                           in (s_undelay :: SparseData COO D n1 n2 e -> SparseData COO U n1 n2 e) interm  
            vals_vec m   = V.map (\(a, _, _) -> a) (coo_vals m)


instance (Undelay COO n1 n2 e, Eq (SparseData COO U n1 n2 e)) => Eq (SparseData COO D n1 n2 e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)
    

instance (Show e, Undelay COO n1 n2 e, Sparse COO ty n1 n2 e) => Show (SparseData COO ty n1 n2 e) where 
  show arr = let darr = SparseBlas.Data.Matrix.Parallel.Sparse.COO.delay arr in 
             case s_undelay darr of 
               COO vs ->  unlines ["COO", "\n"
                                            , "________"
                                            , "(width, height): "
                                            , show (w, h), "\n"
                                            , "vals: "
                                            , "\n", show vs]
            where 
              w = fromIntegral $ natVal (Proxy :: Proxy n1) 
              h = fromIntegral $ natVal (Proxy :: Proxy n2)


delay :: (Sparse COO ty n1 n2 e) => SparseData COO ty n1 n2 e -> SparseData COO D n1 n2 e 
delay = SGeneric.delay


transpose :: (Sparse COO ty n1 n2 e) 
          => SparseData COO ty n1 n2 e -> SparseData COO D n2 n1 e
transpose = SGeneric.transpose


convert :: Sparse r2 D n1 n2 e => SparseData COO D n1 n2 e -> SparseData r2 D n1 n2 e 
convert  = SGeneric.convert


map :: Sparse COO ty n1 n2 e => (e -> b) -> SparseData COO ty n1 n2 e -> SparseData COO D n1 n2 b 
map = SGeneric.map 


zipWith :: (Sparse COO ty n1 n2 a, Sparse COO ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c) -> SparseData COO ty n1 n2 a 
        -> SparseData COO ty1 n1 n2 b -> SparseData COO D n1 n2 c
zipWith = SGeneric.zipWith


(#+) :: (Sparse COO ty n1 n2 a) 
     => SparseData COO ty n1 n2 a -> SparseData COO ty n1 n2 a -> SparseData COO D n1 n2 a
(#+) = SGeneric.add 

(#-) :: (Sparse COO ty n1 n2 a) 
     => SparseData COO ty n1 n2 a -> SparseData COO ty n1 n2 a -> SparseData COO D n1 n2 a
(#-) = SGeneric.minus 



scale :: (Sparse COO ty n1 n2 a) 
      => a -> SparseData COO ty n1 n2 a -> SparseData COO D n1 n2 a 
scale = SGeneric.scale 

