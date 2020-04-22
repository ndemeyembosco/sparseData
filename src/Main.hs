{-# LANGUAGE GADTs, ViewPatterns, BangPatterns #-}


module Main where 


import Control.Applicative 
import           Data.Functor.Identity
import qualified Text.Parsec            as P
import           Text.Parsec.Token
import           Text.Parsec.Char 
import           Text.Parsec.Combinator
import           Text.Parsec.Language
import           System.Environment 
import           System.IO
import           Data.List 
import qualified Data.Vector.Unboxed as U  
import qualified System.Random.PCG.Fast.Pure as SR 
import System.CPUTime 
import Text.Printf  
import           Text.Parsec.String     (Parser, parseFromFile)
import SparseData 



-- lexer :: TokenParser u
-- lexer = makeTokenParser $
-- 	emptyDef
-- 	{ reservedNames   = []}


-- mysymbol :: String -> Parser String
-- mysymbol = symbol lexer


-- parseName :: Parser (String, String)
-- parseName = (,) <$> ident <*> ident 

-- ident :: Parser String
-- ident = identifier lexer

-- mydouble :: Parser Double
-- mydouble = float lexer

-- mysigned_double :: Parser Double 
-- mysigned_double = choice [signeddouble, (choice [P.try mydouble, (fromInteger <$> myinteger)])] 
--             where 
--                 signeddouble = makeNum <$> (choice [mysymbol "-", mysymbol "+"]) <*> (choice [P.try mydouble, (fromInteger <$> myinteger)])
--                 makeNum "-" d = negate d 
--                 makeNum "+" d = d 


-- myinteger :: Parser Integer 
-- myinteger = integer lexer

-- mywhiteSpace :: Parser ()
-- mywhiteSpace = whiteSpace lexer


-- mycomma :: Parser String 
-- mycomma = comma lexer 

-- myparse :: Parser a -> String -> Either P.ParseError a
-- myparse p = P.parse p ""


-- data MMExchange where 
--     MMCOO :: (Integer, Integer, Integer) -> [(Integer, Integer, Double)] -> MMExchange 
--     deriving Show


-- data FormatType = Sparse | Dense deriving Show 

-- parseFormatType :: Parser FormatType 
-- parseFormatType = getMatType <$> ((skipMany1 (mysymbol "%")) *> (sepBy1 ident mywhiteSpace))
--           where 
--               getMatType s | (unwords s) == "MatrixMarket matrix coordinate real general" = Sparse 
--                            | (unwords s) == "MatrixMarket matrix array real general"      = Dense -- not sure if this is actually right?  
--                            | otherwise                                                    = error "unsupported header!"


-- parseCommentLine :: Parser String 
-- parseCommentLine = (mysymbol "%") *> (manyTill anyChar endOfLine)


-- parseDims :: Parser (Integer, Integer, Integer) 
-- parseDims = (,,) <$> myinteger <*> myinteger <*> myinteger

-- parseMatEntry :: Parser (Integer, Integer, Double) 
-- parseMatEntry = (,,) <$> myinteger <*> myinteger <*> mysigned_double


-- parseAllEntries :: Parser [(Integer, Integer, Double)]
-- parseAllEntries = manyTill parseMatEntry eof 

-- parseMMExchange :: Parser MMExchange 
-- parseMMExchange = (manyTill parseCommentLine (mysymbol "!")) 
--                *> (MMCOO <$> parseDims <*> parseAllEntries)


-- getRandomVals ::  Double -> Int -> SR.GenIO -> IO (U.Vector Double)
-- getRandomVals m n gen = U.replicateM n $ SR.uniformR (-m, m) gen


-- mm_to_s_data :: MMExchange -> SparseData COO U Double 
-- mm_to_s_data (MMCOO (fromInteger -> w, fromInteger -> h, _) entries) 
--                       =  COO {
--                              coo_vals= (U.fromList $ map (\(fromInteger -> i
--                                                             , fromInteger -> j
--                                                             , d) -> (d, i, j)) entries)
--                             , width=w, height=h}


-- axpydot : 
axpydot :: (Sparse rep ty a, U.Unbox a, Floating a) => SparseData rep ty a -> SparseData rep ty a -> U.Vector a -> a -> SVector a 
axpydot v w u alpha = z_trans #. su 
                  where 
                      su      = from_vector u 
                      sw      = delay w 
                      sv      = delay v 
                      z       = sw #- (alpha `scale` sv)
                      z_trans = s_transpose z 
                      s_transpose (SDelayed (w, h) f) = SDelayed (h, w) (\(i, j) -> f (j, i))



-- vadd 
vadd :: (Sparse rep ty a, U.Unbox a, Floating a) => SparseData rep ty a -> SparseData rep ty a -> SparseData rep ty a -> SparseData rep D a 
vadd m1 m2 m3 = (sm1 #+ sm2) #+ sm3 
            where 
                sm1 = delay m1 
                sm2 = delay m2 
                sm3 = delay m3 


-- waxpy 
waxpy :: (Sparse rep ty a, U.Unbox a, Floating a) => a -> SparseData rep ty a -> a -> SparseData rep ty a -> SparseData rep D a  
waxpy w x b y = (w `scale` sx) #+ (b `scale` sy)
            where 
                sx = delay x 
                sy = delay y 


-- atax 
atax :: (Sparse rep ty a, U.Unbox a, Floating a) => SparseData rep ty a -> U.Vector a -> SVector a  
atax a x = a_tr #. (sa #. sx) 
        where 
            sx   = from_vector x 
            sa   = delay a 
            a_tr = s_transpose sa 

-- -- bicgk 
bicgk :: (Sparse rep ty a, U.Unbox a, Floating a) => SparseData rep ty a -> U.Vector a -> U.Vector a -> (SVector a, SVector a)
bicgk a p r = (sa #. sp, s_transpose sa #. sr)
       where 
           sa = delay a 
           sp = from_vector p 
           sr = from_vector r 


-- aBx + y 
smvm_xpy :: (Sparse rep ty a, U.Unbox a, Floating a) => SparseData rep ty a -> U.Vector a -> U.Vector a -> a -> SVector a  
smvm_xpy !mat !vec1 !vec2 !alpha = ((alpha `scale` smat) #. svec1) ^+^ svec2 
                        where 
                            smat           = delay mat 
                            (svec1, svec2) = (from_vector vec1, from_vector vec2) 
                            (^+^)          = szipWith_i (+)


-- gemv 
gemv :: (Sparse rep ty a, U.Unbox a, Floating a) => a -> a -> SparseData rep ty a  -> U.Vector a -> U.Vector a -> SVector a 
gemv alpha beta a x y = (alpha `scale` sa #. sx) ^+^ (smap_i (*beta) sy) 
                    where
                       (^+^)   = szipWith_i (+) 
                       sa      = delay a 
                       sx      = from_vector x 
                       sy      = from_vector y 


-- gemvt 
gemvt :: (Sparse rep ty a, U.Unbox a, Floating a) => a -> a -> SparseData rep ty a -> U.Vector a -> U.Vector a -> U.Vector a -> (SVector a, SVector a)
gemvt alpha beta a x y z = (alpha `scale` sa #. sx, (beta `scale` a_tr #. sy) ^+^ sz) 
                    where 
                        (^+^)          = szipWith_i (+)
                        sa    = delay a 
                        a_tr  = s_transpose sa 
                        sx    = from_vector x 
                        sy    = from_vector y 
                        sz    = from_vector z     


-- gemver 
gesummv :: (Sparse rep ty a, U.Unbox a, Floating a) => a -> a -> SparseData rep ty a -> SparseData rep ty a -> U.Vector a -> SVector a 
gesummv alpha beta a b x = (alpha `scale` a #. sx) ^+^ (beta `scale` b #. sx)
                    where 
                        (^+^)          = szipWith_i (+)
                        sx = from_vector x 


-- trilazy 
trilazy :: (Sparse rep ty a, U.Unbox a, Floating a) => SparseData rep ty a -> SparseData rep ty a -> U.Vector a -> U.Vector  a -> SVector a
trilazy u_mat y_mat u y = sy ^-^ (sy_mat #. (u_mat_tr #. su)) ^-^ (su_mat #. (y_mat_tr #. su))
                    where 
                        (^-^) = szipWith_i (-)
                        sy_mat = delay y_mat 
                        su_mat = delay u_mat 
                        u_mat_tr = s_transpose su_mat
                        y_mat_tr = s_transpose sy_mat
                        sy       = from_vector y 
                        su       = from_vector u 



cg :: (Num a, Sparse rep ty a, U.Unbox a, Eq a, Floating a) => Int -> SVector a -> SparseData rep ty a ->  SVector a -> (a, SVector a)
{-# INLINE cg #-}
cg !iters !z !a !x = 
    let 
        !r     = x 
        !p     = r  
        !rho   = r <.> r 
    in go a z x rho p r 0 
    where
        {-# INLINE go #-}
        go !a !z !x !d !p !r !i | i == iters     =  
                                    let 
                                        !residual = (a #. z) ^-^ x 
                                        !to_ret = sqrt $ residual <.> residual 
                                    in (to_ret, z) 
                                | otherwise =  
                                    let 
                                        !q         = a #. p 
                                        !alpha     = d / (p <.> q) 
                                        !new_z     = z ^+^ (alpha .* p) 
                                        !new_r     = r ^-^ (alpha .* q)  
                                        !new_d     = new_r <.> new_r 
                                        !beta      = new_d / d  
                                        !new_p     = new_r ^+^ (beta .* p) 
                                    in go a new_z x new_d new_p new_r (i + 1)
        (<.>) v1 v2   = sum_i $ szipWith_i (*) v1 v2 
        (^+^)         = szipWith_i (+)
        (^-^)         = szipWith_i (-)  
        (.*) c        = smap_i (*c)


main :: IO ()
main = undefined 
    -- do 
    -- (filename:_) <- getArgs
    -- filestr      <- readFile filename
    -- case myparse parseMMExchange filestr of 
    --     Left  err    -> print err 
    --     Right mmdata@(MMCOO (w, fromInteger -> h, nnz) es) -> do 
    --         let seed = 271828183
    --         my_gen    <- SR.initialize seed
    --         vec1   <- getRandomVals 20 h my_gen
    --         vec2   <- getRandomVals 20 h my_gen
    --         alpha  <- SR.uniformR (-5, 5) my_gen
    --         start <- getCPUTime 
    --         let !ans = smvm_xpy (mm_to_s_data mmdata) vec1 vec2 alpha 
    --         let !v = to_vector ans 
    --         -- print $ U.take 10 v  
    --         end  <- getCPUTime
    --         let diff = (fromIntegral (end - start)) / (10^12)
    --         printf "Time in seconds =   %0.9f sec\n" (diff :: Double)
    --         print "success!"













