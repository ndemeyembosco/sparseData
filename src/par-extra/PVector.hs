{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE BangPatterns, DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module PVector where 

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Mutable as BV
import qualified Data.Vector as BU 
import qualified Data.Vector.Generic as G 
import qualified Data.Vector.Unboxed as U 
import Control.Monad.Par 
-- import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Combinator (parMap)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Primitive
import Control.Monad 
import Control.DeepSeq 
import GHC.Generics (Generic)
import GHC.Base                 (quotInt, remInt)
import Patterns 


data family PVector rep sh e 

data N 
data P 

data instance PVector N sh e = Normal sh (BU.Vector e) -- to keep in Unboxed vec, lookinto implementing function 
                                                       -- to thaw, and freeze in parallel (a version for each of 
                                                       -- of the combinators)

data instance PVector P sh e = Partitioned { vec_extent :: sh 
                                           , regions :: [Region sh e] }

								   
data Region sh a 
              = Region {  regionRange :: Range sh 
                        , regionGen :: Generator sh a}

data Range sh = RangeAll 
			  | RangeRects {rangeMatch :: sh -> Bool
						   , rangeRects :: [Rect sh]}
						   
data Rect sh = Rect sh sh 

data Generator sh a = GenNormal {genVector :: BU.Vector a}


data Stencil sh a = Stencil {
						   stencilSize :: sh 
						,  stencilZero :: a 
						,  stencilAcc  :: sh -> a -> a -> a 
					}
					

makeStencil :: Num a => sh ->  (sh -> Maybe a) -> Stencil sh a 
makeStencil ex getCoeff = Stencil ex 0
					       $ \ix val acc -> case getCoeff ix of 
										      Nothing    -> acc 
										      Just coeff -> acc + val * coeff
				   

infixl 3 :. 
data Z = Z deriving (Show, Read, Eq, Ord)
data tail :. head = !tail :. !head deriving (Show, Read, Eq, Ord)

type DIM0 = Z 
type DIM1 = DIM0 :. Int 
type DIM2 = DIM1 :. Int 
type DIM3 = DIM2 :. Int 


ix1 :: Int -> DIM1
ix1 x = Z :. x
{-# INLINE ix1 #-}

ix2 :: Int -> Int -> DIM2
ix2 y x = Z :. y :. x
{-# INLINE ix2 #-}

ix3 :: Int -> Int -> Int -> DIM3
ix3 z y x = Z :. z :. y :. x
{-# INLINE ix3 #-}

class Shape sh where 
	toIndex   :: sh -> sh -> Int 
	fromIndex :: sh -> Int -> sh 
	size      :: sh -> Int 

instance Shape Z where
	{-# INLINE [1] size #-}
	size _                  = 1
	{-# INLINE [1] toIndex #-}
	toIndex _ _             = 0
	{-# INLINE [1] fromIndex #-}
	fromIndex _ _           = Z


instance Shape sh => Shape (sh :. Int) where
        {-# INLINE [1] size #-}
        size  (sh1 :. n)
                = size sh1 * n

        {-# INLINE [1] toIndex #-}
        toIndex (sh1 :. sh2) (sh1' :. sh2')
                = toIndex sh1 sh1' * sh2 + sh2'

        {-# INLINE [1] fromIndex #-}
        fromIndex (ds :. d) n
                = fromIndex ds (n `quotInt` d) :. r
                where
                -- If we assume that the index is in range, there is no point
                -- in computing the remainder for the highest dimension since
                -- n < d must hold. This saves one remInt per element access which
                -- is quite a big deal.
                r       | size ds == 1  = n   -- use rank instead 
                        | otherwise     = n `remInt` d 
                 
                   
 
-- shape polymorphic, parallel functions
mapNormal :: (Shape sh, NFData b) => (a -> b) -> PVector N sh a -> PVector N sh b 
mapNormal f (Normal sh vec) = runPar $ do 
				  ivars <- BU.mapM (\e -> spawnP $ f e) vec
				  vec1  <- BU.mapM get ivars  
				  return $ Normal sh vec1





func :: IO ()
func = return ()
