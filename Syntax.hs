
module Syntax where

type Check = Either String

data Term = TmTrue
          | TmFalse
          | TmIf    Term Term Term
          | TmVar   Int Char
          | TmAbs   Char TyTerm Term
          | TmApp   Term Term
          deriving(Eq)

instance Show Term where
    show TmTrue         = "true"
    show TmFalse        = "false"
    show (TmIf c t f)   = concat ["if ",show c," then ",show t," else ",show f]
    show (TmVar _ s)    = [s]
    show (TmAbs c ty t) = concat ["(\\", [c], " ", show t, ")"]
    show (TmApp a b)    = show a ++  " " ++ show b

data TyTerm = TyArr TyTerm TyTerm
            | TyBool
            deriving(Eq)

instance Show TyTerm where
    show (TyArr t1 t2) = show t1 ++ " -> " ++ show t2
    show TyBool        = "Bool"
