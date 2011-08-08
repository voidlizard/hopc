module Main where

import System.Environment (getArgs)
import System.IO (stdin)
import qualified Data.ByteString as BS

import Control.Monad.Error
import Compilers.Hopc.Frontend.Lisp.Parse
import Compilers.Hopc.Frontend.KTree
import qualified Compilers.Hopc.Frontend.Lisp.KNormalize as K
import qualified Compilers.Hopc.Frontend.AlphaConv as A
import qualified Compilers.Hopc.Frontend.BetaReduction as B
import qualified Compilers.Hopc.Frontend.LetFlatten as L
import qualified Compilers.Hopc.Frontend.Closure as C
--import Compilers.Hopc.Backend.DumbC
import Debug.Trace

import Text.PrettyPrint.HughesPJClass (prettyShow)

main = do
    (x:_) <- getArgs
    e <- withInput parseTop x
--    print e
--    error "stop"
    let k = either (const $ error "Parse error") K.kNormalizeTop e
    let k' = C.convert $ L.flatten $ B.betaReduce $ A.alphaConv k
--    let k' = L.flatten $ B.betaReduce $ A.alphaConv k
--    let k' = B.betaReduce $ A.alphaConv k --k --L.flatten $ B.betaReduce $ A.alphaConv k
    putStrLn $ prettyShow k'
    error "done"

withInput :: (BS.ByteString -> b) -> String -> IO b

withInput fn "-" = do
    s <- BS.hGetContents stdin
    return $ fn s

withInput fn x = error "File input is not supported yet"

