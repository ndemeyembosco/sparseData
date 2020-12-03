{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes
           , EmptyDataDecls
           , ScopedTypeVariables 
           , UndecidableInstances, DataKinds, Strict #-}


module DelayedBlas.Data.Matrix.Parallel.Sparse.COO where 

import qualified Data.Vector.Unboxed as V 
import Control.Parallel.Strategies ( NFData )
import Prelude hiding (zipWith)
import Control.DeepSeq ( NFData(..) ) 
import GHC.TypeLits ( KnownNat, natVal ) 
import Data.Proxy ( Proxy(..) )

import DelayedBlas.Data.Matrix.Parallel.Generic.Generic as DGeneric
    ( Undelay(..),
      Matrix(s_index, MatrixData),
      MatrixData(SDelayed),
      RepIndex(D, U),
      delay,
      transpose,
      convert,
      map,
      zipWith,
      add,
      minus,
      scale )


-- data U
--------------- Unboxed --------------------

data COO 
--------------- COO ------------------------
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Matrix COO U n1 n2 e where 
    data MatrixData COO U n1 n2 e   = COO { coo_vals :: V.Vector (e, Int, Int) }
    -- indexing is big o length of non zeros
    s_index (COO vals) (r, c) = els
     where 
       els = case V.find (\(a, x, y) -> (x == r) && (y == c)) vals of 
                Nothing -> 0 --error "index element non-existent"
                Just (a1, _, _) -> a1


instance NFData (MatrixData COO U n1 n2 e) where 
  rnf (COO vals) = COO vals `seq` vals `seq` ()
    
instance (Matrix COO D n1 n2 e, Matrix COO U n1 n2 e) => Undelay COO n1 n2 e where  
    s_undelay (SDelayed func) = COO vals
      where 
        vals_r r = V.unfoldrN w (\c -> 
                                  if func (r, c) /= 0 
                                  then Just ((func (r,c), c), c + 1) 
                                  else Nothing) 0 
        rows     = Prelude.map (\r -> V.map (\(x, c) -> (x, r, c)) (vals_r r)) [0..h-1]
        vals     = V.concat rows
        w        = fromIntegral $ natVal (Proxy :: Proxy n1) 
        h        = fromIntegral $ natVal (Proxy :: Proxy n2) 
    non_zeros (COO vals) = let (v, _, _) = V.unzip3 vals in v  
                      


instance (Undelay COO n1 n2 e) => Eq (MatrixData COO U n1 n2 e) where 
    arr1 == arr2 = and_v v_vec == fromIntegral (V.length v_vec)    
           where 
            v_vec        = vals_vec mat 
            and_v  l     = V.foldr (+) 0 l   
            mat          = let 
                             (interm :: MatrixData COO D n1 n2 e) =  DelayedBlas.Data.Matrix.Parallel.Sparse.COO.zipWith (\x y -> 
                                                                          if x == y 
                                                                          then 1 
                                                                          else 0) arr1 arr2
                           in (s_undelay :: MatrixData COO D n1 n2 e -> MatrixData COO U n1 n2 e) interm  
            vals_vec m   = V.map (\(a, _, _) -> a) (coo_vals m)


instance (Undelay COO n1 n2 e, Eq (MatrixData COO U n1 n2 e)) => Eq (MatrixData COO D n1 n2 e) where 
    arr1 == arr2 = s_undelay arr1 == s_undelay arr2
    

instance (Show e, Undelay COO n1 n2 e, Matrix COO ty n1 n2 e) => Show (MatrixData COO ty n1 n2 e) where 
  show arr = let darr = DelayedBlas.Data.Matrix.Parallel.Sparse.COO.delay arr in 
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


delay :: (Matrix COO ty n1 n2 e) => MatrixData COO ty n1 n2 e -> MatrixData COO D n1 n2 e 
delay = DGeneric.delay


transpose :: (Matrix COO ty n1 n2 e) 
          => MatrixData COO ty n1 n2 e -> MatrixData COO D n2 n1 e
transpose = DGeneric.transpose


convert :: Matrix r2 D n1 n2 e => MatrixData COO D n1 n2 e -> MatrixData r2 D n1 n2 e 
convert  = DGeneric.convert


map :: Matrix COO ty n1 n2 e => (e -> b) -> MatrixData COO ty n1 n2 e -> MatrixData COO D n1 n2 b 
map = DGeneric.map 


zipWith :: (Matrix COO ty n1 n2 a, Matrix COO ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c) -> MatrixData COO ty n1 n2 a 
        -> MatrixData COO ty1 n1 n2 b -> MatrixData COO D n1 n2 c
zipWith = DGeneric.zipWith


(#+) :: (Matrix COO ty n1 n2 a) 
     => MatrixData COO ty n1 n2 a -> MatrixData COO ty n1 n2 a -> MatrixData COO D n1 n2 a
(#+) = DGeneric.add 

(#-) :: (Matrix COO ty n1 n2 a) 
     => MatrixData COO ty n1 n2 a -> MatrixData COO ty n1 n2 a -> MatrixData COO D n1 n2 a
(#-) = DGeneric.minus 



scale :: (Matrix COO ty n1 n2 a) 
      => a -> MatrixData COO ty n1 n2 a -> MatrixData COO D n1 n2 a 
scale = DGeneric.scale 

