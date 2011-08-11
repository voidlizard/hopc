{-# LANGUAGE OverloadedStrings #-}
module Compilers.Hopc.Frontend.Lisp.Parse (parseExpr, parseTop) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Control.Monad.Error

import Compilers.Hopc.Compile
import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.ErrM
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Par
import Compilers.Hopc.Frontend.KTree

parseExpr :: ByteString -> CompileM Exp
parseExpr = withError . pExp . myLexer

parseTop :: ByteString -> CompileM TopLevel
parseTop = withError . pTopLevel . myLexer

withError (Ok tree) = return tree --undefined --Right tree
withError (Bad s)   = throwError $ ParseError s --undefined --Left $ ParseError s   -- TODO: normal error data
