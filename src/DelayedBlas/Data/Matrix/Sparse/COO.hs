{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes
           , InstanceSigs
           , EmptyDataDecls
           , ScopedTypeVariables 
           , UndecidableInstances, DataKinds, Strict, StrictData #-}


module DelayedBlas.Data.Matrix.Sparse.COO where 

import qualified Data.Vector.Unboxed as U 
import Prelude hiding (zipWith)
import Control.Parallel.Strategies (NFData)

import DelayedBlas.Data.Matrix.Generic.Generic as DGeneric
    ( Undelay(..),
      Matrix(s_dims, s_index, MatrixData),
      MatrixData(SDelayed),
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
instance (U.Unbox e, Num e, Eq e, NFData e) => Matrix COO U e where 
    data MatrixData COO U e   = COO 
                        {  coo_vals :: U.Vector (e, Int, Int)
                           ,  width :: Int
                           , height :: Int
                        }
    -- indexing is big o length of non zeros
    s_index (COO vals w h) (r, c) = els
     where 
       els = case U.find (\(a, x, y) -> and [x == r, y == c]) vals of 
                Nothing -> 0 --error "index element non-existent"
                Just (a1, _, _) -> a1
    s_dims (COO vals w h) = (w, h)
    
instance (U.Unbox e, Num e, Eq e, Matrix COO D e, Matrix COO U e) => Undelay COO e where  
    s_undelay (SDelayed (h, w) func) = COO vals w h 
      where 
        vals_r r = U.unfoldrN w (\c -> 
                                  if func (r, c) /= 0 
                                  then Just ((func (r,c), c), c + 1) 
                                  else Nothing) 0
        rows     = Prelude.map (\r -> U.map (\(x, c) -> (x, r, c)) (vals_r r)) [0..h-1]
        vals     = U.concat rows 
    non_zeros (COO vals w h) = let (v, _, _) = U.unzip3 vals in v  
                      


instance (Undelay COO e) => Eq (MatrixData COO U e) where 
    arr1 == arr2 = (and_v v_vec) == fromIntegral (U.length v_vec)    
           where 
            v_vec        = vals_vec mat 
            and_v  l     = U.foldr (+) 0 l   
            mat          = let 
                             (interm :: MatrixData COO D e) =  DelayedBlas.Data.Matrix.Sparse.COO.zipWith (\x y -> 
                                                                          if x == y 
                                                                          then fromInteger 1 
                                                                          else 0) arr1 arr2
                           in (s_undelay :: MatrixData COO D e -> MatrixData COO U e) interm  
            vals_vec m   = U.map (\(a, _, _) -> a) (coo_vals m)


instance (Undelay COO e, Eq (MatrixData COO U e)) => Eq (MatrixData COO D e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)
    

instance (Show e, Undelay COO e, Matrix COO ty e) => Show (MatrixData COO ty e) where 
  show arr = let darr = DelayedBlas.Data.Matrix.Sparse.COO.delay arr in 
             case s_undelay darr of 
               COO vs w h ->  unlines ["COO", "\n"
                                            , "________"
                                            , "(width, height): "
                                            , show (w, h), "\n"
                                            , "vals: "
                                            , "\n", show vs]


delay :: (U.Unbox e, Num e, Eq e, Matrix COO ty e) => MatrixData COO ty e -> MatrixData COO D e 
delay = DGeneric.delay


transpose :: (U.Unbox e, Matrix COO ty e) 
          => MatrixData COO ty e -> MatrixData COO D e
transpose = DGeneric.transpose


convert :: Matrix r2 D e => MatrixData COO D e -> MatrixData r2 D e 
convert  = DGeneric.convert


empty :: Matrix COO ty a => MatrixData COO ty a -> Bool 
empty = DGeneric.empty 


map :: Matrix COO ty e => (e -> b) -> MatrixData COO ty e -> MatrixData COO D b 
map = DGeneric.map 


zipWith :: (Matrix COO ty a, Matrix COO ty1 b, ty ~ ty1) 
        => (a -> b -> c) -> MatrixData COO ty a 
        -> MatrixData COO ty1 b -> MatrixData COO D c
zipWith = DGeneric.zipWith


(#+) :: (Matrix COO ty a, Num a) 
     => MatrixData COO ty a -> MatrixData COO ty a -> MatrixData COO D a
(#+) = DGeneric.add 

(#-) :: (Matrix COO ty a, Num a) 
     => MatrixData COO ty a -> MatrixData COO ty a -> MatrixData COO D a
(#-) = DGeneric.minus 



scale :: (Matrix COO ty a, Num a) 
      => a -> MatrixData COO ty a -> MatrixData COO D a 
scale = DGeneric.scale 

