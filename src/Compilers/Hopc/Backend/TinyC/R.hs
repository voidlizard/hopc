module Compilers.Hopc.Backend.TinyC.R where

class R a where
    avail   :: [a]
    allRegs :: [a]

