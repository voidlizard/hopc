{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}
module Compilers.Hopc.Frontend.KTree (KTree(..)
                                     ,KId
                                     ,KName
                                     ) where

import qualified Data.ByteString.Char8 as S
import Control.Monad.Error

type KString = String 

type KId = KString 

type KName = KString 

data KTree =   KInt Integer
             | KStr String
             | KLet KId KTree KTree
             | KVar KId
             | KApp KId [KId]
             deriving Show

