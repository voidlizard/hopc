{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable  #-}

module Compilers.Hopc.Backend.TinyC.IR where

import Data.Data
import Data.Typeable

import Data.Generics.PlateData

type LabelId = String
type RId = Int

data IR = IR [Label] deriving (Eq, Show, Data, Typeable)

data Label = Label LabelId [Instr] deriving (Eq, Show, Data, Typeable)

data R = R RId deriving (Eq, Show, Data, Typeable)

data Instr =   MOV R R
             | CALL LabelId LabelId
             | CONST LabelId R
             | NOP
             deriving (Eq, Show, Data, Typeable)

