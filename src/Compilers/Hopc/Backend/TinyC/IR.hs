{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable  #-}

module Compilers.Hopc.Backend.TinyC.IR where

import Data.Data
import Data.Typeable

import Data.Generics.PlateData
import Text.PrettyPrint.HughesPJClass

type LabelId = String
type RId = Int

data IR = IR [Instr] deriving (Eq, Show, Data, Typeable)

data R = R RId deriving (Eq, Show, Data, Typeable)

data JumpCnd  = JumpFake R deriving (Eq, Show, Data, Typeable)

type Desc = String

data Instr = I Op Desc
             deriving (Eq, Show, Data, Typeable)

data Op =   MOV   R R
          | CALL  LabelId LabelId
          | CONST LabelId R
          | CJUMP JumpCnd LabelId
          | JUMP  LabelId
          | LABEL LabelId
          | NOP   
          deriving (Eq, Show, Data, Typeable)

op :: Op -> Instr
op x = I x ""

opc :: Op -> Desc -> Instr
opc x s = I x s

instance Pretty Instr where
    pPrintPrec _ _ x = undefined

instance Pretty Op where
    pPrintPrec _ _ x = undefined
