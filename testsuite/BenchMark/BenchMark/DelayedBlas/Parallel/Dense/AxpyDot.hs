{-# LANGUAGE DataKinds, 
             RankNTypes, 
             BangPatterns, 
             ScopedTypeVariables, 
             FlexibleContexts, 
             KindSignatures, 
             LambdaCase, 
             TypeOperators #-}

module BenchMark.DelayedBlas.Parallel.Dense.AxpyDot where


import qualified Data.Vector.Unboxed as UNB 
-- import BenchMark.DelayedBlas.Parallel.PKernels ( gemv )
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Matrix(MatrixData), 
             RepIndex(U), 
             toVector, 
             fromVector, 
             SVector, 
             scale, 
             (#.), 
             (!+!), 
             (!*!),
             (!-!), 
             (!.!),
             MatrixException(..)) 
import DelayedBlas.Data.Matrix.Parallel.Dense.DENSE
    (  MatrixData(DNS), DNS ) 
import Control.DeepSeq (deepseq, NFData)
import System.Environment (getArgs)
import GHC.TypeLits ( KnownNat) 
import Data.Proxy () 
import Data.Maybe () 
import Control.Exception (catch) 
import Axpy 



axpydot ::  (Floating a, NFData a, UNB.Unbox a, KnownNat n)
        => SVector n a -> SVector n a -> SVector n a -> a 
        -> (SVector n a, a) 
{-# INLINE axpydot #-}
axpydot y x u alpha = (z, r)
   where 
     {-# INLINE z #-} 
     z       = axpy alpha x y 
     r       = z !.! u 
{-# SCC axpydot #-}



bench_axpydot :: IO () 
bench_axpydot = do
   (dimension': _) <- getArgs 
   let 
       !dimension = (read dimension') :: Int 
       !v1        = UNB.replicate dimension 3.4568  :: UNB.Vector Double
       !v2        = UNB.replicate dimension 1.2345  :: UNB.Vector Double 
       !v3        = UNB.replicate dimension 8.7653  :: UNB.Vector Double
       !v_func1   = fromVector v1 :: SVector 20000 Double     -- assuming dimension = 20000 
       !v_func2   = fromVector v2 :: SVector 20000 Double 
       !v_func3   = fromVector v3 :: SVector 20000 Double
       !alpha     = 1.0 
       {-# SCC ans_vec "force_axpydot" #-} 
       (!ans_vec', !r) = axpydot v_func1 v_func2 v_func3 alpha
       !ans_vec        = toVector ans_vec'
   print $ "sum after axpydot : " ++ (show $ UNB.sum ans_vec) ++ "\n"
   print $ "residual after axpydot : " ++ (show r) ++ "\n"
   print $ "sum v1 after axpydot : " ++ (show $ UNB.sum v1) ++ "\n"
   print $ "sum v2 after axpydot : " ++ (show $ UNB.sum v2) ++ "\n"
   print $ "sum v3 after axpydot : " ++ (show $ UNB.sum v3) ++ "\n"
   return () 

hs :: IO ()
hs = main 




main :: IO ()
main = do
      print "starting axpy benchmark"
      bench_axpydot  `catch` \case 
                        InvalidVectorLength   -> putStrLn "fromVector received vector argument with invalid length" 
                        FromVecLengthMismatch -> putStrLn "length of vector in type, and length of vector at runtime do not match!" 
                        NullVector            -> putStrLn "A null vector was given as argument to fromVector. This is not allowed."
      return ()