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



lexer :: TokenParser u
lexer = makeTokenParser $
	emptyDef
	{ reservedNames   = []}


mysymbol :: String -> Parser String
mysymbol = symbol lexer


parseName :: Parser (String, String)
parseName = (,) <$> ident <*> ident 

ident :: Parser String
ident = identifier lexer

mydouble :: Parser Double
mydouble = float lexer

mysigned_double :: Parser Double 
mysigned_double = choice [signeddouble, (choice [P.try mydouble, (fromInteger <$> myinteger)])] 
            where 
                signeddouble = makeNum <$> (choice [mysymbol "-", mysymbol "+"]) <*> (choice [P.try mydouble, (fromInteger <$> myinteger)])
                makeNum "-" d = negate d 
                makeNum "+" d = d 


myinteger :: Parser Integer 
myinteger = integer lexer

mywhiteSpace :: Parser ()
mywhiteSpace = whiteSpace lexer


mycomma :: Parser String 
mycomma = comma lexer 

myparse :: Parser a -> String -> Either P.ParseError a
myparse p = P.parse p ""


data MMExchange where 
    MMCOO :: (Integer, Integer, Integer) -> [(Integer, Integer, Double)] -> MMExchange 
    deriving Show


data FormatType = Sparse | Dense deriving Show 

parseFormatType :: Parser FormatType 
parseFormatType = getMatType <$> ((skipMany1 (mysymbol "%")) *> (sepBy1 ident mywhiteSpace))
          where 
              getMatType s | (unwords s) == "MatrixMarket matrix coordinate real general" = Sparse 
                           | (unwords s) == "MatrixMarket matrix array real general"      = Dense -- not sure if this is actually right?  
                           | otherwise                                                    = error "unsupported header!"


parseCommentLine :: Parser String 
parseCommentLine = (mysymbol "%") *> (manyTill anyChar endOfLine)


parseDims :: Parser (Integer, Integer, Integer) 
parseDims = (,,) <$> myinteger <*> myinteger <*> myinteger

parseMatEntry :: Parser (Integer, Integer, Double) 
parseMatEntry = (,,) <$> myinteger <*> myinteger <*> mysigned_double


parseAllEntries :: Parser [(Integer, Integer, Double)]
parseAllEntries = manyTill parseMatEntry eof 

parseMMExchange :: Parser MMExchange 
parseMMExchange = (manyTill parseCommentLine (mysymbol "!")) 
               *> (MMCOO <$> parseDims <*> parseAllEntries)


getRandomVals ::  Double -> Int -> SR.GenIO -> IO (U.Vector Double)
getRandomVals m n gen = U.replicateM n $ SR.uniformR (-m, m) gen


mm_to_s_data :: MMExchange -> SparseData COO U Double 
mm_to_s_data (MMCOO (fromInteger -> w, fromInteger -> h, _) entries) 
                      =  COO {
                             coo_vals= (U.fromList $ map (\(fromInteger -> i
                                                            , fromInteger -> j
                                                            , d) -> (d, i, j)) entries)
                            , width=w, height=h}


smvm_xpy :: SparseData COO U Double -> U.Vector Double -> U.Vector Double -> Double -> SVector Double 
smvm_xpy !mat !vec1 !vec2 !alpha = ((alpha `scale` smat) #. svec1) ^+^ svec2 
                        where 
                            smat           = delay mat 
                            (svec1, svec2) = (from_vector vec1, from_vector vec2) 
                            (^+^)          = szipWith_i (+)


main :: IO ()
main = do 
    (filename:_) <- getArgs
    filestr      <- readFile filename
    case myparse parseMMExchange filestr of 
        Left  err    -> print err 
        Right mmdata@(MMCOO (w, fromInteger -> h, nnz) es) -> do 
            let seed = 271828183
            my_gen    <- SR.initialize seed
            vec1   <- getRandomVals 20 h my_gen
            vec2   <- getRandomVals 20 h my_gen
            alpha  <- SR.uniformR (-5, 5) my_gen
            start <- getCPUTime 
            let !ans = smvm_xpy (mm_to_s_data mmdata) vec1 vec2 alpha 
            let !v = to_vector ans 
            -- print $ U.take 10 v  
            end  <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^12)
            printf "Time in seconds =   %0.9f sec\n" (diff :: Double)
            print "success!"













