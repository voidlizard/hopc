module Main where

import System.Environment (getArgs)
import System.IO (stdin)
import qualified Data.ByteString as BS

import Control.Monad.Error
import Compilers.Hopc.Frontend.KTree
import qualified Compilers.Hopc.Typing.Types as T
import qualified Compilers.Hopc.Typing.Infer as I
--import Compilers.Hopc.Backend.DumbC
import Debug.Trace

data MType = MVar T.TypeId | MInt | MString deriving (Eq, Show)

instance T.TType MType  where
    occurs t x  = False

    subst ta a x@(MVar s) = if ta == s then a else x
    subst ta a x = x

    isVar (MVar _)  = True
    isVar x         = False

    merge x y = Nothing

    typeid (MVar s)  = s
    typeid x         = "unknown"

main = do
    let t1 = [(MVar "x", MInt), (MVar "y", MVar "x"), (MVar "y", MString)]
    let t2 = I.infer t1
    print t2
    

