{-# LANGUAGE GADTs, ViewPatterns, BangPatterns, DataKinds, Strict, FlexibleContexts #-}


module BenchMark.DelayedBlas.Parallel.PKernels where 


import qualified Data.Vector.Unboxed as U 
-- import Data.Vector.Strategies 
import Control.Parallel.Strategies 
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Undelay(s_undelay),
      Matrix((#.), MatrixData),
      SVector,
      RepIndex(U, D),
      toVector,
      fromVector,
      vzipWith,
      (!+!),
      (!-!),
      (!*!),
      (!.!),
      delay,
      transpose,
      scale )
import GHC.TypeLits 
import Data.Proxy 




-- axpydot : 
axpydot ::  (Floating a, NFData a, U.Unbox a, KnownNat n)
        => SVector n a -> SVector n a -> SVector n a -> a 
        -> (SVector n a, a) 
axpydot w v u alpha = (z, r)
   where  
     z       = w !-! (alpha !*! v)
     r       = z !.! u 

-- axpydot : 
axpy ::  (Floating a, NFData a, U.Unbox a, KnownNat n)
     => a  -> SVector n a -> SVector n a -> SVector n a 
axpy a x y = a !*! x !+! y  


-- -- bicgk 
bicgk :: (Matrix rep ty n1 n2 a, Floating a) 
      => MatrixData rep ty n1 n2 a -> SVector n2 a 
      -> SVector n1 a -> (SVector n1 a, SVector n2 a)
bicgk a p r = (a #. p, transpose a #. r)



-- gemv 
gemv :: (Matrix rep ty n n a, Floating a) 
     => a -> a -> MatrixData rep ty n n a  -> SVector n a 
     -> SVector n a -> SVector n a 
{-# INLINE gemv #-}
gemv alpha beta a x y = (alpha `scale` a #. x) !+! (beta !*! y) 
{-# SCC gemv #-}


-- gemvt 
gemvt :: (Matrix rep ty n n a, Floating a) 
      => a -> a -> MatrixData rep ty n n a -> SVector n a 
      -> SVector n a -> (SVector n a, SVector n a)
gemvt alpha beta a y z = let x = (beta !*! ((transpose a) #. y)) !+! z in (x, alpha !*! (a #. x))   


-- gemver 
gesummv :: (Matrix rep ty n n a, Floating a) 
        => a -> a -> MatrixData rep ty n n a -> MatrixData rep ty n n a 
        -> SVector n a -> SVector n a 
gesummv alpha beta a b x = (alpha !*! (a #. x)) !+! (beta !*! (b #. x))


-- trilazy 
trilazy :: (Matrix rep ty n n a, Floating a) 
        => MatrixData rep ty n n a -> MatrixData rep ty n n a 
        -> SVector n a -> SVector n a
trilazy w_mat y_mat u = r !-! s 
   where 
     s = w_mat #. ((transpose y_mat) #. u)
     r = y_mat #. ((transpose w_mat) #. u)
