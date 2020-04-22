{-# LANGUAGE GADTs, ViewPatterns, BangPatterns, DataKinds #-}


module Kernels where 


import qualified Data.Vector.Unboxed as U  
import System.CPUTime 
import SGeneric 




-- axpydot : 
axpydot :: (Sparse rep ty a, U.Unbox a, Floating a) 
        => SparseData rep ty a -> SparseData rep ty a 
        -> U.Vector a -> a -> SVector a 
axpydot v w u alpha = z_trans #. su 
   where 
     su      = from_vector u 
     sw      = delay w 
     sv      = delay v 
     z       = sw #- (alpha `scale` sv)
     z_trans = transpose z 



-- vadd 
vadd :: (Sparse rep ty a, U.Unbox a, Floating a) 
     => SparseData rep ty a -> SparseData rep ty a 
     -> SparseData rep ty a -> SparseData rep D a 
vadd m1 m2 m3 = (sm1 #+ sm2) #+ sm3 
   where 
     sm1 = delay m1 
     sm2 = delay m2 
     sm3 = delay m3 


-- waxpy 
waxpy :: (Sparse rep ty a, U.Unbox a, Floating a) 
      => a -> SparseData rep ty a -> a -> SparseData rep ty a 
      -> SparseData rep D a  
waxpy w x b y = (w `scale` sx) #+ (b `scale` sy)
   where 
     sx = delay x 
     sy = delay y 


-- atax 
atax :: (Sparse rep ty a, U.Unbox a, Floating a) 
     => SparseData rep ty a -> U.Vector a -> SVector a  
atax a x = a_tr #. (sa #. sx) 
   where 
     sx   = from_vector x 
     sa   = delay a 
     a_tr = transpose sa 

-- -- bicgk 
bicgk :: (Sparse rep ty a, U.Unbox a, Floating a) 
      => SparseData rep ty a -> U.Vector a 
      -> U.Vector a -> (SVector a, SVector a)
bicgk a p r = (sa #. sp, transpose sa #. sr)
   where 
     sa = delay a 
     sp = from_vector p 
     sr = from_vector r 


-- aBx + y 
smvm_xpy :: (Sparse rep ty a, U.Unbox a, Floating a) 
         => SparseData rep ty a -> U.Vector a -> U.Vector a 
         -> a -> SVector a  
smvm_xpy !mat !vec1 !vec2 !alpha = ((alpha `scale` smat) #. svec1) ^+^ svec2 
   where 
     smat           = delay mat 
     (svec1, svec2) = (from_vector vec1, from_vector vec2) 
     (^+^)          = vzipWith (+)


-- gemv 
gemv :: (Sparse rep ty a, U.Unbox a, Floating a) 
     => a -> a -> SparseData rep ty a  -> U.Vector a 
     -> U.Vector a -> SVector a 
gemv alpha beta a x y = (alpha `scale` sa #. sx) ^+^ (vmap (*beta) sy) 
   where
     (^+^)   = vzipWith (+) 
     sa      = delay a 
     sx      = from_vector x 
     sy      = from_vector y 


-- gemvt 
gemvt :: (Sparse rep ty a, U.Unbox a, Floating a) 
      => a -> a -> SparseData rep ty a -> U.Vector a 
      -> U.Vector a -> U.Vector a -> (SVector a, SVector a)
gemvt alpha beta a x y z = (alpha `scale` sa #. sx
                              , (beta `scale` a_tr #. sy)  ^+^ sz) 
   where 
     (^+^)          = vzipWith (+)
     sa    = delay a 
     a_tr  = transpose sa 
     sx    = from_vector x 
     sy    = from_vector y 
     sz    = from_vector z     


-- gemver 
gesummv :: (Sparse rep ty a, U.Unbox a, Floating a) 
        => a -> a -> SparseData rep ty a -> SparseData rep ty a 
        -> U.Vector a -> SVector a 
gesummv alpha beta a b x = (alpha `scale` a #. sx) ^+^ (beta `scale` b #. sx)
   where 
     (^+^) = vzipWith (+)
     sx    = from_vector x 


-- trilazy 
trilazy :: (Sparse rep ty a, U.Unbox a, Floating a) 
        => SparseData rep ty a -> SparseData rep ty a 
        -> U.Vector a -> U.Vector  a -> SVector a
trilazy u_mat y_mat u y = sy ^-^ (sy_mat #. (u_mat_tr #. su)) 
                             ^-^ (su_mat #. (y_mat_tr #. su))
   where 
   (^-^)    = vzipWith (-)
   sy_mat   = delay y_mat 
   su_mat   = delay u_mat 
   u_mat_tr = transpose su_mat
   y_mat_tr = transpose sy_mat
   sy       = from_vector y 
   su       = from_vector u 
