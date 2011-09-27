module Main where

import System.Environment (getArgs)
import System.IO (stdin)
import qualified Data.ByteString as BS

import Data.Either
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Control.Monad.Error
import Compilers.Hopc.Frontend.Lisp.Parse
import Compilers.Hopc.Frontend.KTree
import qualified Compilers.Hopc.Frontend.Lisp.KNormalize as K
import qualified Compilers.Hopc.Frontend.AlphaConv as A
import qualified Compilers.Hopc.Frontend.BetaReduction as B
import qualified Compilers.Hopc.Frontend.Const as Cn
import qualified Compilers.Hopc.Frontend.LetFlatten as L
import qualified Compilers.Hopc.Frontend.Closure as C
import Compilers.Hopc.Frontend.Types
import qualified Compilers.Hopc.Frontend.Eliminate as E
import qualified Compilers.Hopc.Typing.Infer as I
import Compilers.Hopc.Typing.Types

import Compilers.Hopc.Compile
import Compilers.Hopc.Error

import qualified Compilers.Hopc.Frontend.KTyped as KT

import Compiler.Hoopl

--import Compilers.Hopc.Backend.DumbC
import Debug.Trace
import Text.Printf

import Text.PrettyPrint.HughesPJClass (prettyShow)

import Compilers.Hopc.Backend.TinyC.IR hiding (Proc)
import qualified Compilers.Hopc.Backend.TinyC.IR as I
import qualified Compilers.Hopc.Backend.TinyC.FromClosure as FC
import qualified Compilers.Hopc.Backend.TinyC.Live as L
import qualified Compilers.Hopc.Backend.TinyC.Opt as O
import qualified Compilers.Hopc.Backend.TinyC.Regs as R
import Compilers.Hopc.Backend.TinyC.VM
import qualified Compilers.Hopc.Backend.TinyC.VM as V
import qualified Compilers.Hopc.Backend.TinyC.CWriter as W


data Results = RVm [V.Proc] | RCCode [String]

runM :: M a -> a
runM m = runSimpleUniqueMonad $ runWithFuel 0 m

main = do
    (x:_) <- getArgs
    input x $ \s -> dumpResult $ runM $ do
        st <- runCompile initCompile $ do
          k  <- parseTop s >>= K.kNormalizeTop
          k' <- A.alphaConv k
          constr2 <- getConstraints
          constr <- KT.constraints k'
          constr' <- I.inferM (constr++constr2)
          addEntries False (map (\(a,b) -> (typeid a, b)) constr')
          k'' <- return k'  >>= Cn.propagate
                            >>= B.betaReduceM
                            >>= L.flattenM
          c1 <- C.conv2 k'' >>= E.eliminate
          procs <- FC.convert c1
          dict <- getEntries
          ep <- getEntryPoint >>= return.fromJust
          vm <- forM procs $ \p@(I.Proc {I.name = n, I.body = g, I.entry = e}) -> do
                  live <- lift $ L.live e g
                  let asap = spillASAP dict live p
                  alloc <- lift $ R.allocateLinearScan dict live asap p
                  lift $ fromIR dict live alloc p
          code <- W.write ep vm
          return $ RCCode code
--          return $ RVm vm
        return st

input "-" fn = BS.hGetContents stdin >>= fn
input x fn = BS.readFile x >>= fn

dumpConstraints (Right x) = trace ("TRACE: constraints PIU PIU \n" ++ intercalate "\n" (map show x) ++ "\n") $ return x 
dumpConstraints (Left _) = error "Type infer error"

dump x = liftIO $ putStrLn (prettyShow x) >> return x

dumpResult :: Either CompileError (Results, CompileState) -> IO ()

dumpResult (Right ((RVm p), _)) =
  forM_ p $ \(V.Proc {V.name = n, V.arity = arity, V.slotnum = sn, V.body = body}) -> do
    putStrLn ""
    putStrLn (printf "%s (%d) slotnum:%d" n arity sn)
    putStrLn $ intercalate "\n" $ map show body

dumpResult (Right ((RCCode c), _)) = do
  putStrLn ""
  putStrLn $ intercalate "\n" c
  putStrLn ""
--  forM_ p $ \(V.Proc {V.name = n, V.arity = arity, V.slotnum = sn, V.body = body}) -> do
--    putStrLn ""
--    putStrLn (printf "%s (%d) slotnum:%d" n arity sn)
--    putStrLn $ intercalate "\n" $ map show body


dumpResult (Right _) = error "Got some positive results but don't know what to do with them"
dumpResult (Left x) = print x

reportStatus (Left x)  = print x
reportStatus (Right x) = error "finished?"

