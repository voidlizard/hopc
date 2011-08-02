module Main where

import System.Environment (getArgs)
import System.IO (stdin)
import qualified Data.ByteString as BS

import Control.Monad.Error
import Compilers.Hopc.Frontend.Lisp.Parse
import Compilers.Hopc.Frontend.KTree
import qualified Compilers.Hopc.Frontend.Lisp.KNormalize as K
--import Compilers.Hopc.Backend.DumbC
import Debug.Trace

import Text.PrettyPrint.HughesPJClass (prettyShow)

main = do
    (x:_) <- getArgs
    e <- withInput parseExpr x
    let k = either (const $ error "Parse error") K.kNormalizeExp e
    putStrLn $ prettyShow k
    error "done"

withInput :: (BS.ByteString -> b) -> String -> IO b

withInput fn "-" = do
    s <- BS.hGetContents stdin
    return $ fn s

withInput fn x = error "File input is not supported yet"

