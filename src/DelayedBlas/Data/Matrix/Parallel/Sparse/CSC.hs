{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, RankNTypes 
           , FlexibleContexts
           , EmptyDataDecls 
           , AllowAmbiguousTypes 
           , UndecidableInstances, DataKinds 
           , ScopedTypeVariables #-}


module DelayedBlas.Data.Matrix.Parallel.Sparse.CSC where 

import qualified DelayedBlas.Data.Matrix.Parallel.Sparse.COO as O 
import qualified DelayedBlas.Data.Matrix.Parallel.Dense.DENSE as D 
import qualified Data.Vector.Unboxed as V 
import Control.Parallel.Strategies ( NFData, parMap, rpar ) 
import Control.DeepSeq ( NFData(..) ) 
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

import GHC.TypeLits ( KnownNat, natVal ) 
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)



data CSC 
instance (KnownNat n1, KnownNat n2, NFData e, Num e, Eq e, V.Unbox e) => Matrix CSC U n1 n2 e where 
    data instance MatrixData CSC U n1 n2 e = CSC {
                                                col_offsets   :: V.Vector Int 
                                              , row_index_csc :: V.Vector Int 
                                              , csc_vals      :: V.Vector e 
                                            }

    s_index (CSC col_offs row_index vals) (r, c) = el 
                                    where 
                                        to_slice = col_offs V.! c
                                        to_start = fromMaybe 0 (col_offs V.!? (c - 1))  
                                        vec      = V.slice to_start 
                                                           (to_slice - to_start)
                                                           $ V.zip row_index vals 
                                        el       = snd $ fromMaybe (0, 0) (V.find (\(x, _) -> x == r) vec) 


instance NFData (MatrixData CSC U n1 n2 e) where 
  rnf (CSC cols rows vals) = let (_, _, _) = (rnf cols, rnf rows, rnf vals) in CSC cols rows vals `seq` ()


instance (Matrix CSC D n1 n2 e, Matrix CSC U n1 n2 e) => Undelay CSC n1 n2 e where  
    s_undelay (SDelayed func) = CSC c_offs rows vals 
                                 where 
                                   vals_r c = V.unfoldrN h (\r -> 
                                                    if func (r, c) /= 0 
                                                    then Just ((func (r,c), c), c + 1) 
                                                    else Nothing) 0 
                                   cols      = parMap rpar vals_r
                                                             [0..w-1]
                                   all_vals_r   = V.concat cols 
                                   c_counts     = V.fromList 
                                                  $ Prelude.map V.length 
                                                                cols
                                   c_offs       = V.scanl (+) 0 c_counts 
                                   (vals, rows) = V.unzip all_vals_r
                                   w            = fromIntegral $ natVal (Proxy :: Proxy n1)
                                   h            = fromIntegral $ natVal (Proxy :: Proxy n2)
    non_zeros (CSC _ _ vals) = vals 


instance (Matrix D.DNS U n1 n2 e, Undelay CSC n1 n2 e) => Eq (MatrixData CSC U n1 n2 e) where 
  arr1 == arr2 = arr1_coo == arr2_coo 
          where 
            (arr1_coo :: MatrixData O.COO U n1 n2 e) = manifestConvert arr1 
            (arr2_coo :: MatrixData O.COO U n1 n2 e) = manifestConvert arr2


instance (Eq (MatrixData CSC U n1 n2 e), Undelay CSC n1 n2 e) => Eq (MatrixData CSC D n1 n2 e) where 
    arr1 == arr2 = s_undelay arr1 == s_undelay arr2


instance (Show e, Undelay CSC n1 n2 e, Matrix CSC ty n1 n2 e) => Show (MatrixData CSC ty n1 n2 e) where 
  show arr = let darr = DelayedBlas.Data.Matrix.Parallel.Sparse.CSC.delay arr in 
              case s_undelay darr of 
                CSC offs rows vals ->  unlines ["CSC", "\n"
                                                        , "________"
                                                        , "(height, width): "
                                                        , show (h, w), "\n"
                                                        , "non-zeros: "
                                                        , "\n", show vals, "\n" 
                                                        , "column offsets: "
                                                        , "\n", show offs, "\n"
                                                        , "rows: "
                                                        , "\n", show rows]

          where 
            w = fromIntegral $ natVal (Proxy :: Proxy n1) 
            h = fromIntegral $ natVal (Proxy :: Proxy n2)

delay :: (Matrix CSC ty n1 n2 e) 
      => MatrixData CSC ty n1 n2 e -> MatrixData CSC D n1 n2 e 
delay = DGeneric.delay


transpose :: (Matrix CSC ty n1 n2 e) 
          => MatrixData CSC ty n1 n2 e -> MatrixData CSC D n2 n1 e
transpose = DGeneric.transpose


convert :: Matrix r2 D n1 n2 e 
        => MatrixData CSC D n1 n2 e -> MatrixData r2 D n1 n2 e 
convert  = DGeneric.convert


map :: Matrix CSC ty n1 n2 e 
    => (e -> b) -> MatrixData CSC ty n1 n2 e -> MatrixData CSC D n1 n2 b 
map = DGeneric.map 


zipWith :: (Matrix CSC ty n1 n2 a, Matrix CSC ty1 n1 n2 b, ty ~ ty1) 
        => (a -> b -> c) -> MatrixData CSC ty n1 n2 a 
        -> MatrixData CSC ty1 n1 n2 b -> MatrixData CSC D n1 n2 c
zipWith = DGeneric.zipWith


(#+) :: (Matrix CSC ty n1 n2 a)  
     => MatrixData CSC ty n1 n2 a -> MatrixData CSC ty n1 n2 a -> MatrixData CSC D n1 n2 a
(#+) = DGeneric.add 

(#-) :: (Matrix CSC ty n1 n2 a) 
     => MatrixData CSC ty n1 n2 a -> MatrixData CSC ty n1 n2 a -> MatrixData CSC D n1 n2 a
(#-) = DGeneric.minus 



scale :: (Matrix CSC ty n1 n2 a) 
      => a -> MatrixData CSC ty n1 n2 a -> MatrixData CSC D n1 n2 a 
scale = DGeneric.scale 
