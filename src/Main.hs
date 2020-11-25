{-# LANGUAGE GADTs
         , ViewPatterns
         , BangPatterns
         , DataKinds #-}


module Main where 


import qualified Data.Vector.Unboxed as U  
import           Text.Parsec.String     (Parser, parseFromFile)
-- import SparseData
-- import SparseBlas.Data.Matrix.Generic.Generic as SGeneric
--     ( Sparse((#.), SparseData), SVector, vmap, vzipWith, vsum ) 


import SparseBlas.Data.Matrix.Parallel.Generic.Generic 
import SparseBlas.Data.Matrix.Parallel.Sparse.COO
import SparseBlas.Data.Matrix.Parallel.Sparse.CSR
import SparseBlas.Data.Matrix.Parallel.Sparse.ELL  
import SparseBlas.Data.Matrix.Parallel.Sparse.CSC 

-- cg :: (Num a, Sparse rep ty a, U.Unbox a, Eq a, Floating a) => Int -> SVector a -> SparseData rep ty a ->  SVector a -> (a, SVector a)
-- {-# INLINE cg #-}
-- cg !iters !z !a !x = 
--     let 
--         !r     = x 
--         !p     = r  
--         !rho   = r <.> r 
--     in go a z x rho p r 0 
--     where
--         {-# INLINE go #-}
--         go !a !z !x !d !p !r !i | i == iters     =  
--                                     let 
--                                         !residual = (a #. z) ^-^ x 
--                                         !to_ret = sqrt $ residual <.> residual 
--                                     in (to_ret, z) 
--                                 | otherwise =  
--                                     let 
--                                         !q         = a #. p 
--                                         !alpha     = d / (p <.> q) 
--                                         !new_z     = z ^+^ (alpha .* p) 
--                                         !new_r     = r ^-^ (alpha .* q)  
--                                         !new_d     = new_r <.> new_r 
--                                         !beta      = new_d / d  
--                                         !new_p     = new_r ^+^ (beta .* p) 
--                                     in go a new_z x new_d new_p new_r (i + 1)
--         (<.>) v1 v2   = vsum $ vzipWith (*) v1 v2 
--         (^+^)         = vzipWith (+)
--         (^-^)         = vzipWith (-)  
--         (.*) c        = vmap (*c)


main :: IO ()
main = undefined 













