{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
           , FlexibleInstances, BangPatterns, RankNTypes 
           , FlexibleContexts 
           , AllowAmbiguousTypes
           , InstanceSigs
           , EmptyDataDecls
           , ScopedTypeVariables 
           , UndecidableInstances, DataKinds, Strict, StrictData #-}


module UCOO where 

import qualified Data.Vector.Unboxed as U 

import SGeneric 

-- data U
--------------- Unboxed --------------------

data COO 
--------------- COO ------------------------
instance (U.Unbox e, Num e, Eq e) => Sparse COO U e where 
    data SparseData COO U e   = COO 
                        {  coo_vals :: U.Vector (e, Int, Int)
                           ,  width :: Int
                           , height :: Int
                        }
    -- indexing is big o length of non zeros
    s_index (COO vals w h) (r, c) = els
     where 
       els = case U.find (\(a, x, y) -> and [x == r, y == c]) vals of 
                Nothing -> 0 --error "index element non-existent"
                Just (a1, _, _) -> a1
    s_dims (COO vals w h) = (w, h)
    
instance (U.Unbox e, Num e, Eq e, Sparse COO D e, Sparse COO U e) => Undelay COO e where  
    s_undelay (SDelayed (h, w) func) = COO vals w h 
      where 
        vals_r r = U.unfoldrN w (\c -> 
                                  if func (r, c) /= 0 
                                  then Just ((func (r,c), c), c + 1) 
                                  else Nothing) 0
        rows     = Prelude.map (\r -> U.map (\(x, c) -> (x, r, c)) (vals_r r)) [0..h-1]
        vals     = U.concat rows 
    non_zeros (COO vals w h) = let (v, _, _) = U.unzip3 vals in v  
                      


instance (Undelay COO e) => Eq (SparseData COO U e) where 
    arr1 == arr2 = (and_v v_vec) == fromIntegral (U.length v_vec)    
           where 
            v_vec        = vals_vec mat 
            and_v  l     = U.foldr (+) 0 l   
            mat          = let 
                             (interm :: SparseData COO D e) =  UCOO.zipWith (\x y -> 
                                                                          if x == y 
                                                                          then fromInteger 1 
                                                                          else 0) arr1 arr2
                           in (s_undelay :: SparseData COO D e -> SparseData COO U e) interm  
            vals_vec m   = U.map (\(a, _, _) -> a) (coo_vals m)


instance (Undelay COO e, Eq (SparseData COO U e)) => Eq (SparseData COO D e) where 
    arr1 == arr2 = (s_undelay arr1) == (s_undelay arr2)
    

instance (Show e, Undelay COO e, Sparse COO ty e) => Show (SparseData COO ty e) where 
  show arr = let darr = UCOO.delay arr in 
             case s_undelay darr of 
               COO vs w h ->  unlines ["COO", "\n"
                                            , "________"
                                            , "(width, height): "
                                            , show (w, h), "\n"
                                            , "vals: "
                                            , "\n", show vs]


delay :: (U.Unbox e, Num e, Eq e, Sparse COO ty e) => SparseData COO ty e -> SparseData COO D e 
delay = SGeneric.delay


transpose :: (U.Unbox e, Sparse COO ty e) 
          => SparseData COO ty e -> SparseData COO D e
transpose = SGeneric.transpose


convert :: Sparse r2 D e => SparseData COO D e -> SparseData r2 D e 
convert  = SGeneric.convert


empty :: Sparse COO ty a => SparseData COO ty a -> Bool 
empty = SGeneric.empty 


map :: Sparse COO ty e => (e -> b) -> SparseData COO ty e -> SparseData COO D b 
map = SGeneric.map 


zipWith :: (Sparse COO ty a, Sparse COO ty1 b, ty ~ ty1) 
        => (a -> b -> c) -> SparseData COO ty a 
        -> SparseData COO ty1 b -> SparseData COO D c
zipWith = SGeneric.zipWith


(#+) :: (Sparse COO ty a, Num a) 
     => SparseData COO ty a -> SparseData COO ty a -> SparseData COO D a
(#+) = SGeneric.add 

(#-) :: (Sparse COO ty a, Num a) 
     => SparseData COO ty a -> SparseData COO ty a -> SparseData COO D a
(#-) = SGeneric.minus 



scale :: (Sparse COO ty a, Num a) 
      => a -> SparseData COO ty a -> SparseData COO D a 
scale = SGeneric.scale 

