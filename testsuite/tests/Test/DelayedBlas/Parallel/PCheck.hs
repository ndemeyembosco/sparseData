{-# LANGUAGE RankNTypes, AllowAmbiguousTypes, LambdaCase #-}
module Test.DelayedBlas.Parallel.PCheck where
import DelayedBlas.Parallel.Dense.DENSE ( test_dense ) 
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic (MatrixException (..))
import Control.Exception ( catch ) 



    
hs = main  

main :: IO () 
main = do 
 test_dense `catch` \case 
                        InvalidVectorLength   -> putStrLn "fromVector received vector argument with invalid length" 
                        FromVecLengthMismatch -> putStrLn "length of vector in type, and length of vector at runtime do not match!" 
                        NullVector            -> putStrLn "A null vector was given as argument to fromVector. This is not allowed."
                      
 return () 





