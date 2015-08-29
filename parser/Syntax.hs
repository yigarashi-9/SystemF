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

instance ToJSON Term


data TyTerm = TyArr TyTerm TyTerm
            | TyBool
            | TyTop
            | TyVar Char Int
            | TyRcd [(String, TyTerm)]
            deriving(Eq, Generic)

instance ToJSON TyTerm

astToJson :: Term -> String
astToJson = LBS.unpack . encode
