{-# LANGUAGE ViewPatterns, DataKinds, GADTs #-}

module MMParser where 

import qualified Data.Vector.Unboxed as U 


import qualified Text.Parsec            as P
import           Text.Parsec.Token
import           Text.Parsec.Char 
import           Text.Parsec.Combinator
import           Text.Parsec.Language
import SGeneric hiding (map)
import UCOO hiding (map)
import           Text.Parsec.String     (Parser, parseFromFile)


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
mysigned_double = choice [
                    signeddouble
                 , (choice [
                            P.try mydouble
                          , (fromInteger <$> myinteger)
                           ])
                 , parseNegDouble
                 , parseLZDouble] 
   where 
     signeddouble = makeNum <$> (choice [mysymbol "-", mysymbol "+"]) 
                            <*> (choice [
                                          P.try mydouble
                                        , (fromInteger <$> myinteger)
                                        ])
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
mm_to_s_data Empty = undefined 
mm_to_s_data (MMCOO (fromInteger -> w, fromInteger -> h, _) entries) 
                   =  COO 
                     {
                       coo_vals= (U.fromList $ map (\(fromInteger -> i
                                                    , fromInteger -> j
                                                    , d) -> (d, i, j)) 
                                               entries)
                     , width=w
                     , height=h}
