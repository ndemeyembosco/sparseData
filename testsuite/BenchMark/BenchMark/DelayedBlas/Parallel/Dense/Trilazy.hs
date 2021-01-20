{-# LANGUAGE DataKinds, 
            RankNTypes, 
            BangPatterns, 
            ScopedTypeVariables, 
            FlexibleContexts, 
            KindSignatures, 
            LambdaCase #-}

module BenchMark.DelayedBlas.Parallel.Dense.Trilazy where


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
             (!-!),
             MatrixException(..)) 
import DelayedBlas.Data.Matrix.Parallel.Dense.DENSE
    (  MatrixData(DNS), DNS ) 
import Control.DeepSeq (deepseq)
import System.Environment (getArgs)
import GHC.TypeLits ( KnownNat ) 
import Data.Proxy () 
import Data.Maybe () 
import Control.Exception (catch)



{-# INLINE trilazy #-}
trilazy :: (Matrix rep ty n n a, Floating a) 
        => MatrixData rep ty n n a -> MatrixData rep ty n n a 
        -> SVector n a -> SVector n a
trilazy w_mat y_mat u = r !-! s 
   where 
     {-# INLINE s #-}
     s = w_mat #. ((transpose y_mat) #. u)
     {-# INLINE r #-}
     r = y_mat #. ((transpose w_mat) #. u)
{-# SCC trilazy #-}


genMatrix ::  Int -> Int -> (forall n1 n2. (KnownNat n1, KnownNat n2) => MatrixData DNS U n1 n2 Double)  
genMatrix width height  = let to_ret = UNB.generate (width * height) (\_ -> 12.3245) in (DNS to_ret) :: MatrixData DNS U widthT heightT Double

sum_mat :: MatrixData DNS U n1 n2 Double -> Double
sum_mat (DNS m) = UNB.sum m 



bench_trilazy :: IO () 
bench_trilazy = do
   (dimension': _) <- getArgs 
   let 
       !dimension = (read dimension') :: Int 
       !m1         = (genMatrix dimension dimension :: MatrixData DNS U 1000 1000 Double)
       !m2         = (genMatrix dimension dimension :: MatrixData DNS U 1000 1000 Double)
       !v1        = UNB.replicate dimension 3.4568 :: UNB.Vector Double
       !v_func1   = fromVector v1 
       !alpha     = 1.0 
       !beta      = 1.0 
       {-# SCC ans_vec "force_trilazy" #-} 
       !ans_vec = toVector $ trilazy m1 m2 v_func1 
   print $ "sum after trilazy: " ++ (show $ UNB.sum ans_vec) ++ "\n"
   print $ "sum v1 after trilazy: " ++ (show $ UNB.sum v1) ++ "\n"
   print $ "sum m1 after trilazy: " ++ (show $ sum_mat m1) ++ "\n"
   print $ "sum m2 after trilazy: " ++ (show $ sum_mat m2) ++ "\n"
   return () 


hs :: IO ()
hs = main 




main :: IO ()
main = do
      print "starting dense benchmark"
      bench_trilazy  `catch` \case 
                        InvalidVectorLength   -> putStrLn "fromVector received vector argument with invalid length" 
                        FromVecLengthMismatch -> putStrLn "length of vector in type, and length of vector at runtime do not match!" 
                        NullVector            -> putStrLn "A null vector was given as argument to fromVector. This is not allowed."
      return ()