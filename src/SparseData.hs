{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}


module SparseData where 

import qualified Data.Vector.Mutable as VB 
import qualified Data.Vector as VU 
import qualified Data.Vector.Unboxed as U 
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Generic as G 
import qualified Control.Monad.Par as P 
-- import qualified Control.Monad.Par.IO as P (ParIO, runParIO, IVar)
import Control.Monad.Par.Class  
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Primitive
import Control.Monad.IO.Class (liftIO)
import Control.Monad 
import Control.Monad.ST (runST)
import Patterns (parDivConqGenV, parDivConqZipsGenV)


-- instance PrimMonad P.ParIO where 
--   type PrimState P.ParIO = RealWorld 
--   primitive = liftIO . primitive 

parMap :: (NFData a, NFData b) => (a -> b) -> [a] -> [b] 
parMap !f !xs = P.runPar $ mapM (P.spawnP . f) xs >>= mapM get

smapPar :: (NFData a, NFData b) => (a -> b) -> VU.Vector a -> VU.Vector b 
smapPar !f !xs = P.runPar $ VU.mapM (P.spawnP . f) xs >>= VU.mapM get
  --  unsafePerformIO $ P.runParIO $ do 
  --         let !len = U.length vec
  --         !mvec    <- U.unsafeThaw vec   
  --         !ivars <- forM [0..len - 1] $ \i -> do 
  --           !e    <- V.read mvec i 
  --           ((!ivar) :: P.IVar b) <- new 
  --           fork $ let !ans = f e in put ivar ans 
  --           return ivar 

  --         !to_ret <- mapM get ivars 
  --         return to_ret

          -- forM_ [0..len - 1] $ \i -> do 
          --   let !ivar = ivars VU.! i 
          --   !e    <- get ivar 
          --   -- V.write mvec1 i e 
          -- liftIO $! U.unsafeFreeze mvec1 



{-# INLINE mapParN #-}
mapParN :: (NFData a, NFData b, U.Unbox a, U.Unbox b, Ord b) => Int -> (a -> b) -> U.Vector a -> U.Vector b
mapParN !n !f !v = parDivConqGenV v n (U.map f) (U.++)


{-# INLINE imapParN #-}
imapParN :: (NFData a, NFData b, U.Unbox a, U.Unbox b, Ord b) => Int -> (Int -> a -> b) -> U.Vector a -> U.Vector b
imapParN !n !f !v = parDivConqGenV v n (U.imap f) (U.++) 


{-# INLINE filterParN #-}
filterParN :: (NFData a, U.Unbox a, Ord a) => Int -> (a -> Bool) -> U.Vector a -> U.Vector a
filterParN !n !f !v = parDivConqGenV v n (U.filter f) (U.++)


{-# INLINE ifilterParN #-}
ifilterParN :: (NFData a, U.Unbox a, Ord a) => Int -> (Int -> a -> Bool) -> U.Vector a -> U.Vector a
ifilterParN !n !f !v = parDivConqGenV v n (U.ifilter f) merge

{-# INLINE mapMaybeParN #-}
mapMaybeParN :: (NFData a, NFData b, U.Unbox a, U.Unbox b, Ord b) => Int -> (a -> Maybe b) -> U.Vector a -> U.Vector b
mapMaybeParN !n !f !v = parDivConqGenV v n (U.mapMaybe f) merge


{-# INLINE imapMaybeParN #-}
imapMaybeParN :: (NFData a, NFData b, U.Unbox a, U.Unbox b, Ord b) => Int -> (Int -> a -> Maybe b) -> U.Vector a -> U.Vector b
imapMaybeParN !n !f !v = parDivConqGenV v n (U.imapMaybe f) merge


{-# INLINE zipWithParN #-}
zipWithParN :: (NFData a, NFData b, NFData c 
              , U.Unbox a, U.Unbox b, U.Unbox c
              , Ord c) 
            => Int -> (a -> b -> c) 
            -> U.Vector a -> U.Vector b -> U.Vector c 
zipWithParN par_n f v1 v2 = imapParN par_n (\(!i) (!x) -> app_func f x i v2) v1 
                      where 
                        app_func !f !x !i !vec = U.head $! imapMaybeParN par_n (\(!j) (!y) -> if i == j then Just $ f x y else Nothing) vec   




merge :: (U.Unbox a, Ord a) => U.Vector a -> U.Vector a  -> U.Vector a
{-# INLINE merge #-}
merge !v1 !v2  = unsafePerformIO $ do  
      to_return <- V.new fin_len
      mv1       <- U.unsafeThaw v1 
      mv2       <- U.unsafeThaw v2 
      go 0 0 0 mv1 mv2 to_return
  where
    len_v1 = U.length v1 
    len_v2 = U.length v2 
    fin_len = len_v1 + len_v2 
    {-# INLINE go #-}
    go i j k vec1 vec2 to_return 
                    | (i < len_v1 && j < len_v2) = do 
                                    e1 <- V.read vec1 i 
                                    e2 <- V.read vec2 j 
                                    if e1 <= e2 
                                      then do 
                                        V.write to_return k e1 
                                        go (i + 1) j (k + 1) vec1 vec2 to_return
                                      else do 
                                        V.write to_return k e2 
                                        go i (j + 1) (k + 1) vec1 vec2 to_return
                    | i < len_v1 = do 
                      e <- V.read vec1 i 
                      V.write to_return k e 
                      go (i + 1) j (k + 1) vec1 vec2 to_return
                    | j < len_v2 = do 
                      e <- V.read vec2 j 
                      V.write to_return k e 
                      go i (j + 1) (k + 1) vec1 vec2 to_return
                    | otherwise = do 
                      v_to_return <- U.unsafeFreeze to_return
                      return v_to_return


{-# INLINE splitVec #-}
splitVec :: U.Unbox a => Int -> U.Vector a -> [U.Vector a]
splitVec = splitVecG


{-# INLINE splitVecG #-}
splitVecG :: G.Vector v a  => Int -> v a -> [v a]
splitVecG !n !v | n == v_length     = [v]
                | otherwise = if null rem_list then map (\start -> G.slice start slice_sz v) s_list 
                              else (map (\start -> G.slice start slice_sz v) s_list) ++ rem_list
            where
              !v_length = G.length v 
              (!slice_sz, !rem) = v_length `divMod` n 
              !s_list   = 0 : (map (*slice_sz) [1..(n - 1)])
              !rem_list = [G.slice ((v_length - rem)) rem v] 
			

-----------------------------------------------------------------------------------------------------------
---------------------------------- Sparse data core -------------------------------------------------------
-----------------------------------------------------------------------------------------------------------



data Nat = Z | S Nat 

fromNat :: Nat -> Int 
fromNat Z = 0 
fromNat (S n) = 1 + fromNat n 


toNat :: Int -> Nat 
toNat 0 = Z 
toNat n = S (toNat (n - 1))



----------------------------------------------------------------------------------


class Sparse rep a where 
  data SparseData rep a :: * 
  smap                  :: (U.Unbox a, U.Unbox b, NFData a, NFData b, Ord b) => (a -> b) -> SparseData rep a -> SparseData rep b  
  szipWith              :: (U.Unbox a, U.Unbox b, U.Unbox c, Num a, Num b, Num c) => (a -> b -> c) -> SparseData rep a -> SparseData rep b  -> SparseData rep c  
  (#.)                  :: (U.Unbox a, Num a) => SparseData rep a  -> U.Vector a ->  U.Vector a 
  



-- the coordinate representation 

data COO 

instance Sparse COO a where 
  data instance SparseData COO a  = COO { coo_vals :: U.Vector (a, Int, Int)}
  smap !f !v = let 
                  vec  = coo_vals v   
               in v { coo_vals = mapParN 1 (\(x, i, j) -> (f x, i, j)) vec} 
  szipWith !func !v1 !v2 = if U.null vec1 
                           then COO $ U.map (\(t, i, j) -> (func 0 t, i, j)) vec2 
                           else if U.null vec2 
                            then COO $ U.map (\(t, i, j) -> (func t 0, i, j)) vec1  
                            else COO (U.map (\(!x, !i, !j) -> app_func func x i j vec2) vec1)   
                  where  
                      vec1 = coo_vals v1
                      vec2 = coo_vals v2 
                      app_func !f !a !i !j !vec = U.head $!                                 -- consider changing this implementation
                                                       U.mapMaybe (\(!b, !x, !y) -> if i == x && j == y then Just (f a b, i, j) else Nothing) vec -- exploit parallelism

  (#.) !mat !vec  =  undefined
    -- U.imap (\(!i) (!x) -> U.sum $! U.map (\(!a, _, _) -> a*x) $! colums i mat) vec 
    --         where
    --           colums !i !m = U.filter (\(!a, _, !n) -> n == i) $! coo_vals m 

            
-- -- the compressed sparse row representation

-- data CSR 

-- instance Sparse CSR a where 
--   data instance SparseData CSR a = CSR { row_offsets :: U.Vector Int, columns :: U.Vector Int,  csr_vals :: U.Vector a, max_col :: !Int}
--   smap f v = let vec = csr_vals v in v {csr_vals = mapParN 1 f vec}
--   szipWith !func !v1@CSR{row_offsets = rows_1, columns = cols_1, csr_vals = vals_1, max_col = m_1} 
--                  !v2@CSR{row_offsets = rows_2, columns = cols_2, csr_vals = vals_2, max_col = m_2} = CSR {
--                                                                                                            row_offsets  = U.zipWith max rows_1 rows_2
--                                                                                                           , columns     = cols 
--                                                                                                           , csr_vals    = vals 
--                                                                                                           , max_col     = max_e
--                                                                                                           }
--                                                                               where 
--                                                                                 (cols, vals) = mkVec max_e min_e
--                                                                                 max_e  = max m_1 m_2
--                                                                                 min_e  = min m_1 m_2 
--                                                                                 mkVec !len_max !len_min  = unsafePerformIO $ do 
--                                                                                         !ans_cols  <- V.new len_max  
--                                                                                         !ans_vals  <- V.new len_max

--                                                                                         !mcols_1 <- U.unsafeThaw cols_1 
--                                                                                         !mcols_2 <- U.unsafeThaw cols_2 

--                                                                                         !mvals_1 <- U.unsafeThaw vals_1 
--                                                                                         !mvals_2 <- U.unsafeThaw vals_2


--                                                                                         forM_ [0..len_min - 1] $ \i -> do 
--                                                                                           !x <- V.read mcols_1 i
--                                                                                           !y <- V.read mcols_2 i  

--                                                                                           !a <- V.read mvals_1 i 
--                                                                                           !b <- V.read mvals_2 i 

--                                                                                           if x < y then do 
--                                                                                                 V.write ans_cols i x          -- write a 
--                                                                                                 V.write ans_cols (i + 1) y    -- write b 

--                                                                                                 V.write ans_vals i (func a 0) 
--                                                                                                 V.write ans_vals (i + i) (func 0 b)
--                                                                                           else if x > y then do 
--                                                                                                 V.write ans_cols (i + 1) x    -- write b 
--                                                                                                 V.write ans_cols i y          -- write a 

--                                                                                                 V.write ans_vals (i + 1) (func a 0) 
--                                                                                                 V.write ans_vals i (func 0 b)
--                                                                                           else do 
--                                                                                                 V.write ans_cols i x     -- need to apply func on both a and b
--                                                                                                 V.write ans_vals i (func a b)
--                                                                                         if len_min == m_1 then do 
--                                                                                           forM_ [len_min..len_max - 1] $ \i -> do 
--                                                                                             !y <- V.read mcols_2 i 
--                                                                                             V.write ans_cols i y 

--                                                                                             !b <- V.read mvals_2 i 
--                                                                                             V.write ans_vals i (func 0 b) 
--                                                                                         else do 
--                                                                                           forM_ [len_min..len_max - 1] $ \i -> do 
--                                                                                             !x <- V.read mcols_1 i 
--                                                                                             V.write ans_cols i x 

--                                                                                             !a <- V.read mvals_1 i 
--                                                                                             V.write ans_vals i (func a 0) 

--                                                                                         U.unsafeFreeze mcols_1
--                                                                                         U.unsafeFreeze mcols_2 

--                                                                                         U.unsafeFreeze mvals_1 
--                                                                                         U.unsafeFreeze mvals_2 

--                                                                                         !vans_cols <- U.unsafeFreeze ans_cols 
--                                                                                         !vans_vals <- U.unsafeFreeze ans_vals
--                                                                                         return (vans_cols, vans_vals)   
                                                                                                
--   (#.) !mat@CSR{row_offsets = rows, columns = cols, csr_vals = vals, max_col = _} !vec = U.imap (\i x -> dot_p i cols (find_row i x vals) vec) $ U.init rows
--                                                                               where
--                                                                                 dot_p i cols !v1 !v2 = U.sum $ U.imap (\j e -> if (cols U.! i) == j then e * (v1 U.! i) else 0) v2 
--                                                                                 find_row index elem values = U.slice index ((rows U.! (index + 1)) - elem) vals 


      
-- ----------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------- Generic operations & algorithms (any representation) ---------------------------------
-- ----------------------------------------------------------------------------------------------------------------------------


scale   :: (U.Unbox a, Num a, Ord a, Sparse rep a, NFData a) => a -> SparseData rep a  -> SparseData rep a  
scale x = smap (*x)


(#+) :: (Sparse rep a, Num a, U.Unbox a) => SparseData rep a  -> SparseData rep a  -> SparseData rep a  
(#+) = szipWith (+)  

(#-) :: (Sparse rep a, Num a, U.Unbox a) => SparseData rep a  -> SparseData rep a  -> SparseData rep a 
(#-) = szipWith (-)







