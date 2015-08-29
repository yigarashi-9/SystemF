{-# LANGUAGE RankNTypes #-}
module Parser (parseTerm) where

import Syntax

import           Text.Parsec hiding(State)
import           Text.Parsec.String
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language
import           Data.Char
import           Control.Monad
import           Control.Applicative((<*))

{- --------------------------------------------------------
               インデックスを付けるための機構
   -------------------------------------------------------- -}

data Context = Ctx {
      depth     :: Int,
      binding   :: [(Char, Int)],
      tyDepth   :: Int,
      tyBinding :: [(Char, Int)]
    }

updateCtx :: Context -> Char -> Context
updateCtx (Ctx d b tyD tyB) c = (Ctx (d+1) ((c, d):b) tyD tyB)

updateTyCtx :: Context -> Char -> Context
updateTyCtx (Ctx d b tyD tyB) c = (Ctx d b (tyD+1) ((c, tyD):tyB))

freeVar :: Char -> Int
freeVar c = (ord c) - (ord 'a')

freshTyVar :: Char -> Int
freshTyVar c = (ord c) - (ord 'A')

{- 外側から小さい値を使う de Bruijin レベルを使う．
   束縛変数ならばそのまま値を返してよく，
   自由変数ならば深さの分だけシフトしてインデックスとする．-}
index :: Context -> Char -> Int
index (Ctx depth bind _ _) v =
    case lookup v bind of
      Just d  -> d
      Nothing -> depth + freeVar v


tyIndex :: Context -> Char -> Int
tyIndex (Ctx _ _ depth bind) v =
    case lookup v bind of
      Just d  -> d
      Nothing -> depth + freshTyVar v

{- --------------------------------------------------------
                          パーサー
   -------------------------------------------------------- -}
parseTerm :: String -> Check Term
parseTerm input =
    case parse (term $ Ctx 0 [] 0 []) "" input of
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

identifier :: Parser String
identifier = P.identifier lexer

var :: Parser Char
var = do
  i <- identifier
  if length i == 1 && head i `elem` ['a'..'z']
  then return $ head i
  else fail "variable must be one lower character"

tyVar :: Parser Char
tyVar = do
  i <- identifier
  if length i == 1 && head i `elem` ['A'..'Z']
  then return $ head i
  else fail "variable must be one upper character"

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parens :: forall a. Parser a -> Parser a
parens = P.parens lexer

brackets :: forall a. Parser a -> Parser a
brackets = P.brackets lexer

braces :: forall a. Parser a -> Parser a
braces = P.braces lexer

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
termBody c =  try (abst c)
          <|> try (tyAbst c)
          <|> try (generalApp c)
          <|> termif c

generalApp :: Context -> Parser Term
generalApp c = unit c >>= appChain c

appChain :: Context -> Term -> Parser Term
appChain c t =  try (do t' <- unit c
                        appChain c (TmApp t t'))
            <|> try (do t' <- brackets (tyAnnot c)
                        appChain c (TmTyApp t t'))
            <|> return t

abst :: Context -> Parser Term
abst c = do
  a  <- (reservedOp "\\") >> var
  ty <- optionMaybe (reservedOp ":" >> tyAnnot c)
  t  <- termBody (updateCtx c a)
  case ty of Just ty' -> return $ TmAbs a ty' t
             Nothing  -> fail "annotate type"

unit :: Context -> Parser Term
unit c =  try (liftM2 (foldl TmProj) (unit_rec c) (many $ symbol "." >> identifier))
      <|> unit_rec c

unit_rec :: Context -> Parser Term
unit_rec c =  parens (termBody c)
          <|> try (liftM TmRcd (braces $ record c `sepBy1` symbol ","))
          <|> try (symbol "true"  >> return TmTrue)
          <|> try (symbol "false" >> return TmFalse)
          <|> do a <- var
                 return $ TmVar (index c a) a

record :: Context -> Parser (String, Term)
record c = liftM2 (,) (identifier <* symbol "=") (termBody c)

termif :: Context -> Parser Term
termif c = do
  p <- try (symbol "if")   >> termBody c
  t <- try (symbol "then") >> termBody c
  f <- try (symbol "else") >> termBody c
  return $ TmIf p t f

tyAnnot :: Context -> Parser TyTerm
tyAnnot c = tyunit c `chainl1` (reservedOp "->" >> return TyArr)

tyunit :: Context -> Parser TyTerm
tyunit c =  try (symbol "Bool" >> return TyBool)
        <|> try (symbol "Top" >> return TyTop)
        <|> try (parens $ tyAnnot c)
        <|> try (liftM TyRcd (braces $ tyRecord c `sepBy1` symbol ","))
        <|> do v <- tyVar
               return $ TyVar v (tyIndex c v)

tyRecord :: Context -> Parser (String, TyTerm)
tyRecord c = liftM2 (,) (identifier <* symbol ":") (tyAnnot c)

tyAbst :: Context -> Parser Term
tyAbst c = do
  v  <- (reservedOp "\\") >> tyVar
  t  <- termBody (updateTyCtx c v)
  return $ TmTyAbs v t
