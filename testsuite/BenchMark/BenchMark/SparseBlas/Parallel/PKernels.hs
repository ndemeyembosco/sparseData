{-# LANGUAGE GADTs, ViewPatterns, BangPatterns, DataKinds, Strict, FlexibleContexts #-}


module BenchMark.SparseBlas.Parallel.PKernels where 


import qualified Data.Vector.Unboxed as U 
-- import Data.Vector.Strategies 
import Control.Parallel.Strategies 
import SparseBlas.Data.Matrix.Parallel.Generic.Generic
    ( Undelay(s_undelay),
      Sparse((#.), SparseData),
      SVector,
      RepIndex(U, D),
      to_vector,
      from_vector,
      vzipWith,
      (!+!),
      (!-!),
      (!*!),
      (!.!),
      delay,
      transpose,
      scale )




-- axpydot : 
axpydot ::  (Floating a, NFData a, U.Unbox a)
        => SVector a -> SVector a -> SVector a -> a 
        -> (SVector a, a) 
axpydot w v u alpha = (z, r)
   where  
     z       = w !-! (alpha !*! v)
     r       = z !.! u 

-- axpydot : 
axpy ::  (Floating a, NFData a, U.Unbox a)
     => a  -> SVector a -> SVector a -> SVector a 
axpy a x y = a !*! x !+! y  

-- axpyU ::  (Floating a, NFData a)
--      => a  -> U.Vector a -> U.Vector a -> SVector a 
-- axpyU a x y = let (x', y') = (from_vector x, from_vector y) in a !*! x' !+! y' 

-- axpyAllU ::  (Floating a, NFData a)
--      => a  -> U.Vector a -> U.Vector a -> U.Vector a 
-- axpyAllU a x y = (U.zipWith (+) (U.map (* a) x) y) `using` (parVector 4) 


-- -- two axpies 

-- twiceAxpyNoForce ::  (Floating a, NFData a) 
--      => a -> SVector a -> a -> SVector a -> SVector a 
-- twiceAxpyNoForce a x p y = let n = axpy a x p y in axpy a n p n 


-- twiceAxpyForce ::  (Floating a, NFData a)
--      => a -> SVector a -> a -> SVector a -> SVector a 
-- twiceAxpyForce a x p y = let n = to_vector $ axpy a x p y in axpy a (from_vector n) p (from_vector n)  




-- vadd 
vadd :: (Floating a, NFData a, U.Unbox a) 
     => SVector a -> SVector a 
     -> SVector a -> SVector a 
vadd v1 v2 v3 = v1 !+! v2 !+! v3 



-- waxpy 
waxpby :: (Floating a, NFData a, U.Unbox a) 
       => a -> SVector a -> a -> SVector a 
       -> SVector a  
waxpby a x b y = a !*! x !+! (b !*! y)



-- -- atax 
-- ataxNoForce :: (Sparse rep ty a, Floating a, U.Unbox a) 
--      => SparseData rep ty a -> SVector a -> SVector a  
-- ataxNoForce a x = (transpose a) #. (a #. x)  

-- ataxForce :: (Sparse rep ty a, Floating a, U.Unbox a) 
--      => SparseData rep ty a -> SVector a -> SVector a  
-- ataxForce a x = let x' = from_vector $! to_vector $ mvec a x in (transpose a) #. x' 

mvec :: (Sparse rep ty a, Floating a, U.Unbox a) 
     => SparseData rep ty a -> SVector a -> SVector a  
mvec a x =  a #. x

-- ataxTwiceNoForce :: (Sparse rep ty a, Floating a, U.Unbox a) 
--      => SparseData rep ty a -> SVector a -> SVector a  
-- ataxTwiceNoForce a x = let x' = (ataxNoForce a x) in (transpose a) #. (a #. x')

-- ataxTwiceForce :: (Sparse rep ty a, Floating a, U.Unbox a) 
--      => SparseData rep ty a -> SVector a -> SVector a  
-- ataxTwiceForce a x = let x' = from_vector $! to_vector $ ataxForce a x in (transpose a) #. (a #. x')



-- ataxTwiceAddNoForce :: (Sparse rep ty a, Floating a, U.Unbox a) 
--      => SparseData rep ty a -> SVector a -> SVector a  
-- ataxTwiceAddNoForce a x = let a' = transpose a in a' #. (a #. ((a' #. (a #. x)) !+! (a' #. (a #. x))))


-- ataxTwiceAddForce :: (Sparse rep D a, Sparse rep U a, Sparse rep ty a, Floating a, Undelay rep a) 
--      => SparseData rep ty a -> SVector a -> SVector a  
-- ataxTwiceAddForce a x = let 
--                           a' = s_undelay $ (transpose a) 
--                           x' = to_vector $ (delay a') #. (a #. x) 
--                         in (delay a') #. (a #. (from_vector x') !+! (from_vector x'))


-- -- bicgk 
bicgk :: (Sparse rep ty a, Floating a) 
      => SparseData rep ty a -> SVector a 
      -> SVector a -> (SVector a, SVector a)
bicgk a p r = (a #. p, transpose a #. r)



-- aBx + y 
smvm_xpy :: (Sparse rep ty a, Floating a) 
         => SparseData rep ty a -> U.Vector a -> U.Vector a 
         -> a -> SVector a  
smvm_xpy !mat !vec1 !vec2 !alpha = ((alpha `scale` smat) #. svec1) ^+^ svec2 
   where 
     smat           = delay mat 
     (svec1, svec2) = (from_vector vec1, from_vector vec2) 
     (^+^)          = vzipWith (+)


-- gemv 
gemv :: (Sparse rep ty a, Floating a) 
     => a -> a -> SparseData rep ty a  -> SVector a 
     -> SVector a -> SVector a 
{-# INLINE gemv #-}
gemv alpha beta a x y = (alpha `scale` a #. x) !+! (beta !*! y) 


-- gemvt 
gemvt :: (Sparse rep ty a, Floating a) 
      => a -> a -> SparseData rep ty a -> SVector a 
      -> SVector a -> (SVector a, SVector a)
gemvt alpha beta a y z = let x = (beta !*! ((transpose a) #. y)) !+! z in (x, alpha !*! (a #. x))   


-- gemver 
gesummv :: (Sparse rep ty a, Floating a) 
        => a -> a -> SparseData rep ty a -> SparseData rep ty a 
        -> SVector a -> SVector a 
gesummv alpha beta a b x = (alpha !*! (a #. x)) !+! (beta !*! (b #. x))


-- trilazy 
trilazy :: (Sparse rep ty a, Floating a) 
        => SparseData rep ty a -> SparseData rep ty a 
        -> SVector a -> SVector a
trilazy w_mat y_mat u = r !-! s 
   where 
     s = w_mat #. ((transpose y_mat) #. u)
     r = y_mat #. ((transpose w_mat) #. u)
