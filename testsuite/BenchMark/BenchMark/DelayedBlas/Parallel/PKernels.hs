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

-- axpyU ::  (Floating a, NFData a)
--      => a  -> U.Vector a -> U.Vector a -> SVector a 
-- axpyU a x y = let (x', y') = (fromVector x, fromVector y) in a !*! x' !+! y' 

-- axpyAllU ::  (Floating a, NFData a)
--      => a  -> U.Vector a -> U.Vector a -> U.Vector a 
-- axpyAllU a x y = (U.zipWith (+) (U.map (* a) x) y) `using` (parVector 4) 


-- -- two axpies 

-- twiceAxpyNoForce ::  (Floating a, NFData a) 
--      => a -> SVector a -> a -> SVector a -> SVector a 
-- twiceAxpyNoForce a x p y = let n = axpy a x p y in axpy a n p n 


-- twiceAxpyForce ::  (Floating a, NFData a)
--      => a -> SVector a -> a -> SVector a -> SVector a 
-- twiceAxpyForce a x p y = let n = toVector $ axpy a x p y in axpy a (fromVector n) p (fromVector n)  




-- vadd 
vadd :: (Floating a, NFData a, U.Unbox a, KnownNat n) 
     => SVector n a -> SVector n a 
     -> SVector n a -> SVector n a 
vadd v1 v2 v3 = v1 !+! v2 !+! v3 



-- waxpy 
waxpby :: (Floating a, NFData a, U.Unbox a, KnownNat n) 
       => a -> SVector n a -> a -> SVector n a 
       -> SVector n a  
waxpby a x b y = a !*! x !+! (b !*! y)



-- -- atax 
-- ataxNoForce :: (Sparse rep ty a, Floating a, U.Unbox a) 
--      => SparseData rep ty a -> SVector a -> SVector a  
-- ataxNoForce a x = (transpose a) #. (a #. x)  

-- ataxForce :: (Sparse rep ty a, Floating a, U.Unbox a) 
--      => SparseData rep ty a -> SVector a -> SVector a  
-- ataxForce a x = let x' = fromVector $! toVector $ mvec a x in (transpose a) #. x' 

mvec :: (Matrix rep ty n1 n2 a, Floating a, U.Unbox a) 
     => MatrixData rep ty n1 n2 a -> SVector n2 a -> SVector n1 a  
mvec a x =  a #. x

-- ataxTwiceNoForce :: (Matrix rep ty a, Floating a, U.Unbox a) 
--      => MatrixData rep ty a -> SVector a -> SVector a  
-- ataxTwiceNoForce a x = let x' = (ataxNoForce a x) in (transpose a) #. (a #. x')

-- ataxTwiceForce :: (Matrix rep ty a, Floating a, U.Unbox a) 
--      => MatrixData rep ty a -> SVector a -> SVector a  
-- ataxTwiceForce a x = let x' = fromVector $! toVector $ ataxForce a x in (transpose a) #. (a #. x')



-- ataxTwiceAddNoForce :: (Matrix rep ty a, Floating a, U.Unbox a) 
--      => MatrixData rep ty a -> SVector a -> SVector a  
-- ataxTwiceAddNoForce a x = let a' = transpose a in a' #. (a #. ((a' #. (a #. x)) !+! (a' #. (a #. x))))


-- ataxTwiceAddForce :: (Matrix rep D a, Matrix rep U a, Matrix rep ty a, Floating a, Undelay rep a) 
--      => MatrixData rep ty a -> SVector a -> SVector a  
-- ataxTwiceAddForce a x = let 
--                           a' = s_undelay $ (transpose a) 
--                           x' = toVector $ (delay a') #. (a #. x) 
--                         in (delay a') #. (a #. (fromVector x') !+! (fromVector x'))


-- -- bicgk 
bicgk :: (Matrix rep ty n1 n2 a, Floating a) 
      => MatrixData rep ty n1 n2 a -> SVector n2 a 
      -> SVector n1 a -> (SVector n1 a, SVector n2 a)
bicgk a p r = (a #. p, transpose a #. r)



-- aBx + y 
smvm_xpy :: (Matrix rep ty n n a, Floating a) 
         => MatrixData rep ty n n a -> U.Vector a -> U.Vector a 
         -> a -> SVector n a  
smvm_xpy !mat !vec1 !vec2 !alpha = ((alpha `scale` smat) #. svec1) ^+^ svec2 
   where 
     smat           = delay mat 
     (svec1, svec2) = (fromVector vec1, fromVector vec2) 
     (^+^)          = vzipWith (+)


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
