{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable  #-}
module Compilers.Hopc.Frontend.KTree (KTree(..)
                                     ,KId
                                     ,KName
                                     ,KString
                                     ) where

import Data.Data
import Data.Typeable
import Text.PrettyPrint.HughesPJClass

import Data.Generics.PlateData

import qualified Data.ByteString.Char8 as S
import Control.Monad.Error

type KString = String 

type KId = KString 

type KName = KString 

--data TType = TLambda | TVar

data KTree =   KUnit
             | KInt Integer
             | KStr String
             | KLet KId KTree KTree
             | KVar KId
             | KLambda [KId] KTree
             | KApp KId [KId]
             deriving (Show, Eq, Data, Typeable)

instance Pretty KTree where
    pPrintPrec _ _ (KUnit)  = text "" 
    pPrintPrec _ _ (KInt n) = integer n 
    pPrintPrec _ _ (KStr s) = (text . show) s
    pPrintPrec _ _ (KVar v) = text v
    pPrintPrec l p (KApp n a) = prettyParen True $ text n <+> ( fsep $ map text a )
    pPrintPrec l p (KLet i e1 e2) = prettyParen True $ text "let"
                                    <+> (prettyParen True $ text i <+> pPrintPrec l p e1) 
                                    $$ nest 2 (pPrintPrec l p e2)
    pPrintPrec l p (KLambda args e) = prettyParen True $ text "lambda"
                                      <+> (prettyParen True (fsep $ map text args))
                                      <+> pPrintPrec l p e

