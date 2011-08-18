module Compilers.Hopc.Backend.TinyC.RegsType where

import Compilers.Hopc.Backend.TinyC.R

data Raw16Regs = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
         deriving (Eq, Ord, Show)

instance R Raw16Regs where
    wtf R1 = True
    wtf _ = False

