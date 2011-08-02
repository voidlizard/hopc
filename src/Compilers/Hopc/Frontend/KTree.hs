{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}
module Compilers.Hopc.Frontend.KTree (KTree(..)
                                     ,KId
                                     ,KName
                                     ) where

import qualified Data.ByteString.Char8 as S
import Control.Monad.Error

type KString = S.ByteString

type KId = KString 

type KName = KString 

data KExpr = KExpr deriving Show

data KTree =   KInt Integer
             | KStr String
             | KLet KId KExpr KExpr
             | KVar KId
             | KApp KId [KId]
             deriving Show

