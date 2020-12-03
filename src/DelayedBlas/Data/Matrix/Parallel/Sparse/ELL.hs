{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, RankNTypes
           , ScopedTypeVariables  
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds, Strict #-}


module DelayedBlas.Data.Matrix.Parallel.Sparse.ELL where 

import qualified Data.Vector.Unboxed as V 
import Control.Parallel.Strategies ( NFData, parMap, rpar ) 
import Data.Vector.Strategies ()
import Control.DeepSeq ( NFData(rnf) ) 

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
      scale,
      manifestConvert )

import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.COO as O
import GHC.TypeLits ( KnownNat, natVal )  
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)



data ELL   
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Matrix ELL U n1 n2 e where 
    data instance MatrixData ELL U n1 n2 e = ELL 
                                              {   max_elem_row    :: !Int
                                                , col_index_ell :: V.Vector Int
                                                , ell_vals      :: V.Vector e
                                                } 
    -- indexing is big o of maximum number of elements per row
    s_index (ELL max_e_r col_ind vals) (r, c) = el 
       where 
         to_start = r  * max_e_r
         vec      = V.slice to_start max_e_r  $ V.zip col_ind vals -- is slicing by max_elem per row too
         el       = snd $ fromMaybe (0,0) $ V.find (\(x,_) -> x == c) vec 

instance NFData (MatrixData ELL U n1 n2 e) where 
  rnf (ELL max_e cols vals) = let ((), (), ()) = (rnf max_e, rnf cols, rnf vals) in ELL max_e cols vals `seq` () 

instance (Matrix ELL D n1 n2 e, Matrix ELL U n1 n2 e) => Undelay ELL n1 n2 e where 
    s_undelay (SDelayed func) = ELL r_max cols vals
       where  
         vals_r r = V.unfoldrN w (\c -> 
                                    if func (r, c) /= 0 
                                    then Just ((func (r,c), c), c + 1) 
                                    else Just ((0, c), c + 1)) 0
         rows         = parMap rpar vals_r [0..h-1]
         all_vals_c   = V.concat rows
         r_max        = let len_list = parMap rpar V.length rows 
                        in if not $ Prelude.null len_list
                             then Prelude.maximum len_list
                             else 0  
         (vals, cols) = V.unzip all_vals_c
         w            = fromIntegral $ natVal (Proxy :: Proxy n1)
         h            = fromIntegral $ natVal (Proxy :: Proxy n2)
    non_zeros (ELL _ _ vals) = vals 


instance (Matrix O.COO U n1 n2 e, Undelay ELL n1 n2 e) => Eq (MatrixData ELL U n1 n2 e) where 
  arr1 == arr2 = arr1_coo == arr2_coo 
          where 
            (arr1_coo :: MatrixData O.COO U n1 n2 e) = manifestConvert arr1 
            (arr2_coo :: MatrixData O.COO U n1 n2 e) = manifestConvert arr2

instance (Eq (MatrixData ELL U n1 n2 e), Undelay ELL n1 n2 e) => Eq (MatrixData ELL D n1 n2 e) where 
    arr1 == arr2 = s_undelay arr1 == s_undelay arr2



instance (Show e, Undelay ELL n1 n2 e, Matrix ELL ty n1 n2 e) => Show (MatrixData ELL ty n1 n2 e) where 
  show arr = let darr = DelayedBlas.Data.Matrix.Parallel.Sparse.ELL.delay arr in 
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


delay :: (Matrix ELL ty n1 n2 e) => MatrixData ELL ty n1 n2 e -> MatrixData ELL D n1 n2 e 
delay = DGeneric.delay


transpose :: (Matrix ELL ty n1 n2 e) 
          => MatrixData ELL ty n1 n2 e -> MatrixData ELL D n2 n1 e
transpose = DGeneric.transpose


convert :: Matrix r2 D n1 n2 e => MatrixData ELL D n1 n2 e -> MatrixData r2 D n1 n2 e 
convert  = DGeneric.convert 


map :: Matrix ELL ty n1 n2 e => (e -> b) -> MatrixData ELL ty n1 n2 e -> MatrixData ELL D n1 n2 b 
map = DGeneric.map 


zipWith :: (Matrix ELL ty n1 n2 a, Matrix ELL ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c)        -> MatrixData ELL ty n1 n2 a 
        -> MatrixData ELL ty1 n1 n2 b -> MatrixData ELL D n1 n2 c
zipWith = DGeneric.zipWith


(#+) :: (Matrix ELL ty n1 n2 a) 
     => MatrixData ELL ty n1 n2 a -> MatrixData ELL ty n1 n2 a -> MatrixData ELL D n1 n2 a
(#+) = DGeneric.add 

(#-) :: (Matrix ELL ty n1 n2 a) 
     => MatrixData ELL ty n1 n2 a -> MatrixData ELL ty n1 n2 a -> MatrixData ELL D n1 n2 a
(#-) = DGeneric.minus 



scale :: (Matrix ELL ty n1 n2 a) 
      => a -> MatrixData ELL ty n1 n2 a -> MatrixData ELL D n1 n2 a 
scale = DGeneric.scale 

