{-# LANGUAGE EmptyDataDecls, OverloadedStrings, DeriveDataTypeable  #-}

module Compilers.Hopc.Backend.TinyC.IR where

import Data.Data
import Data.Typeable

import Data.Generics.PlateData
import Text.PrettyPrint.HughesPJClass
import Text.Printf

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
          | CALL_FOREIGN LabelId [R]
          | CONST LabelId R
          | CJUMP JumpCnd LabelId
          | JUMP  LabelId
          | LABEL LabelId
          | RET
          | NOP   
          deriving (Eq, Show, Data, Typeable)

op :: Op -> Instr
op x = I x ""

opc :: Op -> Desc -> Instr
opc x s = I x s

instance Pretty IR where
    pPrintPrec l p (IR op) = vcat $ map (pPrintPrec l p) op

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
    pPrintPrec l p (CALL_FOREIGN l1 r) = text "call-foreign" <+> text l1 <+> hcat (map (pPrintPrec l p) r)
    pPrintPrec l p (CONST l1 r) = text "const"   <+> text l1 <+> pPrintPrec l p r
    pPrintPrec l p (CJUMP c l1) = text "jmp-cnd" <+> (pPrintPrec l p c) <+> text l1
    pPrintPrec l p (JUMP l1)    = text "jmp"     <+> text l1
    pPrintPrec l p (LABEL l1)   = text "label"   <+> text l1 <+> text ":"
    pPrintPrec l p (NOP)        = text "nop"
    pPrintPrec l p (RET)        = text "ret"

