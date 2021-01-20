{-# LANGUAGE DataKinds, 
            RankNTypes, 
            BangPatterns, 
            ScopedTypeVariables, 
            FlexibleContexts, 
            KindSignatures, 
            LambdaCase #-}

module BenchMark.DelayedBlas.Parallel.Dense.Gemvt where


import qualified Data.Vector.Unboxed as UNB 
-- import BenchMark.DelayedBlas.Parallel.PKernels ( gemv )
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Matrix(MatrixData), 
             RepIndex(U), 
             toVector, 
             fromVector, 
             SVector, 
             scale,
             transpose, 
             (#.), 
             (!+!), 
             (!*!), 
             MatrixException(..)) 
import DelayedBlas.Data.Matrix.Parallel.Dense.DENSE
    (  MatrixData(DNS), DNS ) 
import Control.DeepSeq (deepseq)
import System.Environment (getArgs)
import GHC.TypeLits ( KnownNat ) 
import Data.Proxy () 
import Data.Maybe () 
import Control.Exception (catch)



-- gemvt  
gemvt :: (Matrix rep ty n n a, Floating a) 
      => a -> a -> MatrixData rep ty n n a -> SVector n a 
      -> SVector n a -> SVector n a
{-# INLINE gemvt #-}
gemvt alpha beta a x y = (alpha `scale` (transpose a) #. x) !+! (beta !*! y)
{-# SCC gemvt #-}


genMatrix ::  Int -> Int -> (forall n1 n2. (KnownNat n1, KnownNat n2) => MatrixData DNS U n1 n2 Double)  
genMatrix width height  = let to_ret = UNB.generate (width * height) (\_ -> 12.3245) in (DNS to_ret) :: MatrixData DNS U widthT heightT Double

sum_mat :: MatrixData DNS U n1 n2 Double -> Double
sum_mat (DNS m) = UNB.sum m 



bench_gemvt :: IO () 
bench_gemvt = do
   (dimension': _) <- getArgs 
   let 
       !dimension = (read dimension') :: Int 
       !m         = (genMatrix dimension dimension :: MatrixData DNS U 20000 20000 Double)
       !v1        = UNB.replicate dimension 3.4568 :: UNB.Vector Double
       !v2        = UNB.replicate dimension 1.2345  :: UNB.Vector Double 
       !v_func1   = fromVector v1
       !v_func2   = fromVector v2 
       !alpha     = 1.0 
       !beta      = 1.0  
       {-# SCC ans_vec "force_gemvt1" #-}
       !ans_vec = toVector $ gemvt alpha beta m v_func1 v_func2 
   print $ "sum after gemvt: " ++ (show $ UNB.sum ans_vec) ++ "\n"
   return () 


hs :: IO ()
hs = main 




main :: IO ()
main = do
      print "starting dense benchmark"
      bench_gemvt  `catch` \case 
                        InvalidVectorLength   -> putStrLn "fromVector received vector argument with invalid length" 
                        FromVecLengthMismatch -> putStrLn "length of vector in type, and length of vector at runtime do not match!" 
                        NullVector            -> putStrLn "A null vector was given as argument to fromVector. This is not allowed."
      return ()