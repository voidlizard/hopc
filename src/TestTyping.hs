module Main where

import System.Environment (getArgs)
import System.IO (stdin)
import qualified Data.ByteString as BS

import Control.Monad.Error
import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Typing.Types
import qualified Compilers.Hopc.Typing.Infer as I
import Compilers.Hopc.Frontend.Types
--import Compilers.Hopc.Backend.DumbC
import Debug.Trace
import Data.List

main = do

  let constr = [ (TVar "_tmp0_0",TUnit)
                ,(TVar "_tmp1_1",TUnit)
                ,(TVar "q_2",TFun TFunLocal [] TUnit)
                ,(TVar "f1_3",TFun TFunLocal [TVar "fput_5"] (TAppl "fput_5"))
                ,(TVar "_tmp3_4",TStr)
                ,(TVar "_tmp3_4",TStr)
                ,(TVar "_tmp5_6",TStr)
                ,(TVar "_tmp7_7",TFun TFunLocal [] TUnit)
                ,(TVar "_tmp7_7",TVar "fput_5")
                ,(TVar "display",TFun (TFunForeign False "display") [TStr] TUnit)
                ,(TVar "newline",TFun (TFunForeign False "newline") [TUnit] TUnit)]

  let (Right t2) = I.infer constr
  putStrLn $ intercalate "\n" (map show constr)
  putStrLn "--"
  putStrLn $ intercalate "\n" (map show t2)

