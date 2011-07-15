{-# LANGUAGE OverloadedStrings #-}
module Compilers.Hopc.Backend.DumbC where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Text.Printf

import Compilers.Hopc.Error
import Compilers.Hopc.Frontend.KTree

outModule :: KTreeModule -> Either Error BS.ByteString
outModule exps = 
    let chunks = foldl node [] exps
    in Right $ BS.intercalate "\n" chunks

    where node acc _        = undefined

          setint n = C.pack $ printf "SETVAR(R1, ICONST(%d));" n

