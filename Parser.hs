{-# LANGUAGE RankNTypes #-}
module Parser (parseTerm) where

import Syntax

import           Text.Parsec hiding(State)
import           Text.Parsec.String
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language
import           Data.Char

{- --------------------------------------------------------
               インデックスを付けるための機構
   -------------------------------------------------------- -}

data Context = Ctx {
      depth   :: Int,
      binding :: [(Char, Int)]
    }

updateCtx :: Context -> Char -> Context
updateCtx (Ctx d b) c = (Ctx (d+1) ((c, d):b))

freeVar :: Char -> Int
freeVar c = (ord c) - (ord 'a')

{- 外側から小さい値を使う de Bruijin レベルを使う．
   束縛変数ならばそのまま値を返してよく，
   自由変数ならば深さの分だけシフトしてインデックスとする．-}
index :: Context -> Char -> Int
index (Ctx depth bind) v =
    case lookup v bind of
      Just d  -> d
      Nothing -> depth + freeVar v



{- --------------------------------------------------------
                          パーサー
   -------------------------------------------------------- -}
parseTerm :: String -> Check Term
parseTerm input =
    case parse (term $ Ctx 0 []) "" input of
      Left er -> Left $ show er
      Right r -> Right r

def :: LanguageDef st
def = emptyDef {
        P.opLetter        = oneOf "\\:->",
        P.reservedOpNames = ["\\", ":", "->"],
        P.reservedNames   = ["if", "then", "else", "true", "false", "Bool"]
      }

lexer :: P.TokenParser st
lexer = P.makeTokenParser def

operator :: Parser String
operator = P.operator lexer

var :: Parser Char
var = do
  i <- P.identifier lexer
  if length i == 1 && (head i) `elem` ['a'..'z']
  then return $ head i
  else fail "variable must be one character"

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parens :: forall a. Parser a -> Parser a
parens = P.parens lexer

brackets :: forall a. Parser a -> Parser a
brackets = P.brackets lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

term :: Context -> Parser Term
term c = do
  whiteSpace
  t <- termBody c
  eof >> (return t)

termBody :: Context -> Parser Term
termBody c = abst c <|> app c

abst :: Context -> Parser Term
abst c = do
  a  <- (reservedOp "\\") >> var
  ty <- optionMaybe (reservedOp ":" >> tyAnnot)
  t  <- termBody (updateCtx c a)
  case ty of Just ty' -> return $ TmAbs a ty' t
             Nothing  -> fail "annotate type"

tyAnnot :: Parser TyTerm
tyAnnot = tyunit `chainl1` (reservedOp "->" >> return TyArr)

tyunit :: Parser TyTerm
tyunit =  (symbol "Bool" >> return TyBool)
      <|> parens tyAnnot

app :: Context -> Parser Term
app c = unit c `chainl1` (return TmApp)

unit :: Context -> Parser Term
unit c =  termif c
      <|> parens (termBody c)
      <|> try (symbol "true"  >> return TmTrue)
      <|> try (symbol "false" >> return TmFalse)
      <|> do
        a <- var
        return $ TmVar (index c a) a

termif :: Context -> Parser Term
termif c = do
  p <- try (symbol "if")   >> termBody c
  t <- try (symbol "then") >> termBody c
  f <- try (symbol "else") >> termBody c
  return $ TmIf p t f
