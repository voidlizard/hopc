{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable  #-}

module Compilers.Hopc.Backend.TinyC.VM where

import Compilers.Hopc.Id (KId)

import Data.Data
import Data.Typeable

import Data.Generics.PlateData
import Text.PrettyPrint.HughesPJClass
import Text.Printf

type LabelId = String
type RId = Int

data VM = VM [Instr] deriving (Eq, Show, Data, Typeable)

data R = R RId deriving (Eq, Ord, Show, Data, Typeable)

data JumpCnd  = JumpFake R deriving (Eq, Show, Data, Typeable)

type Desc = String

data Instr = I Op Desc
             deriving (Eq, Show, Data, Typeable)

data Proc = Proc KId [Instr]

data Op =   MOV   R R
          | CALL  LabelId LabelId
          | CALL_FOREIGN LabelId [R] LabelId
          | CALL_LOCAL   LabelId [R] LabelId
          | CALL_CLOSURE R [R] LabelId
          | MAKE_CLOSURE LabelId [R]
          | CONST LabelId R
          | CJUMP JumpCnd LabelId LabelId
          | JUMP  LabelId
          | LABEL LabelId
          | RET
          | NOP   
          deriving (Eq, Show, Data, Typeable)

op :: Op -> Instr
op x = I x ""

opc :: Op -> Desc -> Instr
opc x s = I x s

instance Pretty VM where
    pPrintPrec l p (VM op) = vcat $ map (pPrintPrec l p) op

instance Pretty Instr where
    pPrintPrec l p (I (LABEL l1) dsc) = text (l1++":") $$ nest 42 (text " ;" <+> text dsc)
    pPrintPrec l p (I op@(RET) dsc)   = text "" $$ nest 10 (pPrintPrec l p op
                                                $$ nest 32 (text " ;" <+> text dsc))
                                                $+$ nest 0 (text "")

    pPrintPrec l p (I op dsc) = text "" 
                                $$ nest 10 (pPrintPrec l p op $$ nest 32 (text " ;" <+> text dsc))

instance Pretty R where
    pPrintPrec l p (R id) = text (printf "r%s" (show id))

instance Pretty JumpCnd where
    pPrintPrec l p (JumpFake r) = prettyParen True $ text "some" <+> pPrintPrec l p r

instance Pretty Op where
    pPrintPrec l p (MOV r1 r2)  = text "mov"     <+> (pPrintPrec l p r1) <+> (pPrintPrec l p r2)
    pPrintPrec l p (CALL l1 l2) = text "call"    <+> text l1
    pPrintPrec l p (CALL_FOREIGN l1 r _) = text "call-foreign" <+> text l1 <+> fsep (map (pPrintPrec l p) r)
    pPrintPrec l p (CALL_LOCAL l1 r _) = text "call-local" <+> text l1 <+> fsep (map (pPrintPrec l p) r)
    pPrintPrec l p (CALL_CLOSURE r rs _) = text "call-closure" <+> pPrintPrec l p r <+> fsep (map (pPrintPrec l p) rs)
    pPrintPrec l p (MAKE_CLOSURE l1 rs) = text "make-closure" <+> text l1 <+> fsep (map (pPrintPrec l p) rs)
    pPrintPrec l p (CONST l1 r) = text "const"   <+> text l1 <+> pPrintPrec l p r
    pPrintPrec l p (CJUMP c l0 l1) = text "jmp-cnd" <+> (pPrintPrec l p c) <+> text l0 <+> text l1
    pPrintPrec l p (JUMP l1)    = text "jmp"     <+> text l1
    pPrintPrec l p (LABEL l1)   = text "label"   <+> text l1 <+> text ":"
    pPrintPrec l p (NOP)        = text "nop"
    pPrintPrec l p (RET)        = text "ret"

