module Main where

import System.Environment (getArgs)
import System.IO (stdin)
import qualified Data.ByteString as BS

import Data.Either
import qualified Data.Map as M
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

main = do
    (x:_) <- getArgs
    input x $ \s -> do
        st <- runCompile initCompile $ do
               k <- parseTop s >>= K.kNormalizeTop  -- >>= dump 
--                               >>= A.alphaConvM    >>= dump


               k' <- A.alphaConv k -- >>= dump

               constr2 <- getConstraints 
               constr <- KT.constraints k'

--               dumpConstraints (Right (constr2++constr))

               constr' <- I.inferM (constr++constr2)

               addEntries False (map (\(a,b) -> (typeid a, b)) constr')

               k'' <- return k' >>= Cn.propagate
                                >>= B.betaReduceM
                                >>= L.flattenM

               c1 <- C.conv2 k'' >>= E.eliminate -- >>= dump -- FIXME: make-closure in tail position

               liftIO $ putStrLn $ prettyShow c1

               procs <- FC.convert c1

               dict <- getEntries

               forM_ procs $ \p@(I.Proc {I.name = n, I.body = g, I.entry = e}) -> do
--                   p'@(Proc {body = g'}) <- O.optimize O.deadAssignElim p
                   liftIO $ putStrLn $ " --- " ++ n
--                   liftIO $ putStrLn (showGraph show g)

                   x <- return $ runM $ do
                                   live <- L.live e g
                                   alloc <- R.allocateLinearScan dict live p
                                   fromIR dict live alloc p

                   let (Proc{V.name=n, arity=ar, slotnum=sn, V.body=ops}) = x
                   liftIO $ putStrLn $ printf "FUNCTION: %s(%d) slotnum: %d" n ar sn
                   forM_ ops $ \op -> do 
                       liftIO $ putStrLn $ show op

--                   qq <- O.optimize R.allocate p
--                   liftIO $ putStrLn " --- "
--                   liftIO $ putStrLn (showGraph show g')
--                   liftIO $ putStrLn (showGraph show g')
                   liftIO $ putStrLn "" 

               return ()

        reportStatus st

input "-" fn = BS.hGetContents stdin >>= fn
input x fn = BS.readFile x >>= fn

dumpConstraints (Right x) = trace ("TRACE: constraints PIU PIU \n" ++ intercalate "\n" (map show x) ++ "\n") $ return x 
dumpConstraints (Left _) = error "Type infer error"

dump x = liftIO $ putStrLn (prettyShow x) >> return x

reportStatus (Left x)  = print x
reportStatus (Right x) = error "finished?"

test f = do
    input f $ \s -> do
        st <- runCompile initCompile $ do
               k <- parseTop s >>= K.kNormalizeTop  >>= A.alphaConv

               constr2 <- getConstraints 
               constr <- KT.constraints k
               constr' <- I.inferM (constr++constr2)
               addEntries False (map (\(a,b) -> (typeid a, b)) constr')

               k' <- return k >>= Cn.propagate
                              >>= B.betaReduceM
                              >>= L.flattenM

               c1 <- C.conv2 k' >>= E.eliminate
               procs <- FC.convert c1

               forM procs $ \p@(I.Proc {I.body = g, entry = e}) -> do
                 return.((,) p) $ runM $ L.live e g

        return st

