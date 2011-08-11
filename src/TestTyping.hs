module Main where

import System.Environment (getArgs)
import System.IO (stdin)
import qualified Data.ByteString as BS

import Control.Monad.Error
import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Typing.Types
import qualified Compilers.Hopc.Typing.Infer as I
--import Compilers.Hopc.Backend.DumbC
import Debug.Trace

data MType = MVar TypeId | MInt | MString | MTuple [MType]  deriving (Eq, Show)

instance TType MType  where

    occurs t (MTuple v) = occursList t v
    occurs t x  = False

    subst ta a x@(MVar s) = if ta == s then a else x
    subst ta a x@(MTuple v) = MTuple $ substList ta a v
    subst ta a x = x

    isVar (MVar _)  = True
    isVar x         = False

    merge x y = Nothing

    typeid (MVar s)  = s
    typeid x         = "unknown"

main = do
    let t1 = [(MVar "x", MInt), (MVar "y", MVar "x"), (MVar "z", MTuple [MVar "x", MVar "y"])]
    let t2 = I.infer t1
    print t2
    

