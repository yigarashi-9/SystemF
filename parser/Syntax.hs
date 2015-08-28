{-# LANGUAGE DeriveGeneric #-}

module Syntax where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS

type Check = Either String

data Term = TmTrue
          | TmFalse
          | TmIf    Term Term Term
          | TmVar   Int Char
          | TmAbs   Char TyTerm Term
          | TmApp   Term Term
          | TmTyAbs Char Term
          | TmTyApp Term TyTerm
          | TmRcd   [(String, Term)]
          | TmProj  Term String
          deriving(Eq, Generic)

instance Show Term where
    show TmTrue         = "true"
    show TmFalse        = "false"
    show (TmIf c t f)   = concat ["if ",show c," then ",show t," else ",show f]
    show (TmVar _ s)    = [s]
    show (TmAbs c ty t) = concat ["(\\", [c], " ", show t, ")"]
    show (TmApp a b)    = show a ++  " " ++ show b
    show (TmTyAbs c t)  = concat ["(\\", [c], " ", show t, ")"]
    show (TmTyApp t ty) = concat [show t, " [", show ty, "]"]

instance ToJSON Term


data TyTerm = TyArr TyTerm TyTerm
            | TyBool
            | TyVar Char Int
            | TyRcd [(String, TyTerm)]
            deriving(Eq, Generic)

instance Show TyTerm where
    show (TyArr t1 t2) = show t1 ++ " -> " ++ show t2
    show TyBool        = "Bool"
    show (TyVar c i)   = show c ++ ":" ++ show i

instance ToJSON TyTerm

astToJson :: Term -> String
astToJson = LBS.unpack . encode
