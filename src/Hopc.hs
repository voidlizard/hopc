module Main where

import System.Environment (getArgs)
import qualified Data.ByteString as BS

import Control.Monad.Error
import Compilers.Hopc.Frontend.Lisp.Parse
import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Backend.DumbC

main = do
    (file:_) <- getArgs
    s <- BS.readFile file
--    let ktree = buildKTree tree
    let code = parseToAst s >>= buildKTree >>= outModule
    case code of
        Left err -> print err
        Right c  -> BS.putStrLn c
