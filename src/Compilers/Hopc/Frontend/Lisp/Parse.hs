{-# LANGUAGE OverloadedStrings #-}
module Compilers.Hopc.Frontend.Lisp.Parse where

import Data.ByteString (ByteString)

import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.ErrM
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Par
import Compilers.Hopc.Frontend.KTree


parseToAst :: ByteString -> Either Error Module 
parseToAst = withError . pModule . myLexer
    where withError (Ok tree) = Right tree
          withError (Bad s)   = Left $ ParseError s   -- TODO: normal error data

buildKTree :: Module -> Either Error KTreeModule 
buildKTree (Module exps) = Right $ foldl node [] exps
    where node acc (Integer x) = undefined -- acc ++ [KInt x]
          node acc d@(List (Symbol (TkSymbol "def"):_ )) = acc ++ parseDef d
          node acc x           = error $ "Unsupported " ++ show x -- TODO: Normal error handling

parseDef d@(List [Symbol (TkSymbol "def"), Symbol (TkSymbol name), args, exp]) = error "Got cool function definition!"
parseDef d@(List [Symbol (TkSymbol "def"), Symbol (TkSymbol name), exp])       = error "Got cool constant definition!"
parseDef _ = error "Got shitty definition" 


