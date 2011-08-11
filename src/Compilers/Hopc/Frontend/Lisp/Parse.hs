{-# LANGUAGE OverloadedStrings #-}
module Compilers.Hopc.Frontend.Lisp.Parse (parseExpr, parseTop) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

import Compilers.Hopc.Compile
import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.ErrM
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Par
import Compilers.Hopc.Frontend.KTree

parseExpr :: ByteString -> Either Error Exp
parseExpr = withError . pExp . myLexer

parseTop :: ByteString -> Either Error TopLevel
parseTop = withError . pTopLevel . myLexer

withError (Ok tree) = Right tree
withError (Bad s)   = Left $ ParseError s   -- TODO: normal error data
