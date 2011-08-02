{-# LANGUAGE OverloadedStrings #-}
module Compilers.Hopc.Frontend.Lisp.Parse (parseModule, parseExpr) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.ErrM
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Par
import Compilers.Hopc.Frontend.KTree


parseModule :: ByteString -> Either Error Module 
parseModule = withError . pModule . myLexer
    where withError (Ok tree) = Right tree
          withError (Bad s)   = Left $ ParseError s   -- TODO: normal error data

parseExpr :: ByteString -> Either Error Exp
parseExpr = undefined
