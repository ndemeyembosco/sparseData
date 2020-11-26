{-# LANGUAGE ViewPatterns, DataKinds, GADTs, RankNTypes #-}

module Util.Parser.MMParser where 

import qualified Data.Vector.Unboxed as U 
import System.Environment (getArgs)
import System.Directory


import qualified Text.Parsec            as P
import           Text.Parsec.Token
import           Text.Parsec.Char 
import           Text.Parsec.Combinator
import           Text.Parsec.Language
import qualified SparseBlas.Data.Matrix.Parallel.Generic.Generic as G 
import SparseBlas.Data.Matrix.Generic.Generic hiding (map)
import qualified SparseBlas.Data.Matrix.Parallel.Sparse.COO as O 
import SparseBlas.Data.Matrix.Sparse.COO hiding (map)
import           Text.Parsec.String     (Parser, parseFromFile)
import GHC.TypeLits 
import Data.Proxy 


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
        
         
parseNegDouble :: Parser Double 
parseNegDouble =  (\s -> read s :: Double) 
                  <$> ((mysymbol "-.")  
                  *> (("0." ++) 
                  <$> P.many mydigit)) 

parseLZDouble :: Parser Double
parseLZDouble = (\s -> read s :: Double) 
                <$> ((mysymbol ".")  
                *> (("0." ++) 
                <$> P.many mydigit))

mysigned_double :: Parser Double 
mysigned_double = (P.try parseNegDouble) P.<|> (P.try signeddouble) P.<|>  (P.try mydouble) P.<|> (P.try (fromInteger <$> myinteger)) P.<|> (P.try parseLZDouble) 
   where 
     signeddouble = makeNum <$> (mysymbol "-" P.<|> mysymbol "+") 
                            <*> (P.try mydouble P.<|> (fromInteger <$> myinteger))
     makeNum "-" d = negate d 
     makeNum "+" d = d 


myinteger :: Parser Integer 
myinteger = integer lexer

mydigit :: Parser Char 
mydigit = digit 

mywhiteSpace :: Parser ()
mywhiteSpace = whiteSpace lexer


mycomma :: Parser String 
mycomma = comma lexer 

myparse :: Parser a -> String -> Either P.ParseError a
myparse p = P.parse p ""


data MMExchange where 
   Empty :: MMExchange
   MMCOO :: (Integer, Integer, Integer) 
         -> [(Integer, Integer, Double)] 
         -> MMExchange 
  deriving Show


data FormatType = Sparse | Dense deriving Show 

parseFormatType :: Parser FormatType 
parseFormatType = getMatType <$> ((skipMany1 (mysymbol "%")) 
                             *> (sepBy1 ident mywhiteSpace))
   where 
     getMatType s 
         | (unwords s) == "MatrixMarket matrix coordinate real general" 
          = Sparse 
         | (unwords s) == "MatrixMarket matrix array real general"      
          = Dense -- not sure if this is actually right?  
         | otherwise                                                    
          = error "unsupported header!"


parseCommentLine :: Parser String 
parseCommentLine = (mysymbol "%") *> (manyTill anyChar endOfLine)


parseDims :: Parser (Integer, Integer, Integer) 
parseDims = (,,) <$> myinteger <*> myinteger <*> myinteger

parseMatEntry :: Parser (Integer, Integer, Double) 
parseMatEntry = (,,) <$> myinteger <*> myinteger <*> mysigned_double


parseAllEntries :: Parser [(Integer, Integer, Double)]
parseAllEntries = manyTill (parseMatEntry) eof 

parseMMExchange :: Parser MMExchange 
parseMMExchange = (P.many parseCommentLine) 
               *> (MMCOO <$> parseDims <*> parseAllEntries)


mm_to_s_data :: MMExchange -> SparseData COO U Double 
mm_to_s_data Empty = COO (U.fromList []) 0 0 
mm_to_s_data (MMCOO (fromInteger -> w, fromInteger -> h, _) entries) 
                   =  COO 
                     {
                       coo_vals= (U.fromList $ map (\(fromInteger -> i
                                                    , fromInteger -> j
                                                    , d) -> (d, i, j)) 
                                               entries)
                     , width=w
                     , height=h}



mm_to_s_data_empty :: MMExchange -> G.SparseData O.COO G.U 0 0 Double 
mm_to_s_data_empty Empty = O.COO (U.fromList [])

mm_to_s_data_p :: MMExchange -> (forall n1 n2. (KnownNat n1, KnownNat n2) => G.SparseData O.COO G.U n1 n2 Double)  
mm_to_s_data_p (MMCOO (fromInteger -> w, fromInteger -> h, _) entries) 
                   =  case (someNatVal $ toInteger w, someNatVal $ toInteger h) of 
                         (Just w1, Just h1) -> (O.COO 
                                                {
                                                  O.coo_vals= (U.fromList $ map (\(fromInteger -> i
                                                                                , fromInteger -> j
                                                                                , d) -> (d, i, j)) 
                                                                          entries)
                                                  } :: G.SparseData O.COO G.U w1 h1 Double)
                         _                  -> error "invalid width and height for exchange data"



-- main :: IO () 
-- main = do 
--   args <- getArgs 
--   let 
--     fname = head args 
--   fcontents <- readFile fname 
--   case myparse parseMMExchange fcontents of 
--     Right mat -> print mat 
--     Left err  -> error $ show err 

