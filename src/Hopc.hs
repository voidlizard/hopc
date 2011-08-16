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

import Compilers.Hopc.Backend.TinyC.IR
import qualified Compilers.Hopc.Backend.TinyC.FromClosure as FC

import Compilers.Hopc.Compile
import Compilers.Hopc.Error

import qualified Compilers.Hopc.Frontend.KTyped as KT

--import Compilers.Hopc.Backend.DumbC
import Debug.Trace

import Text.PrettyPrint.HughesPJClass (prettyShow)

main = do
    (x:_) <- getArgs
    input x $ \s -> do
        st <- runCompile initCompile $ do
               k <- parseTop s >>= K.kNormalizeTop -- >>= dump 
--                               >>= A.alphaConvM    >>= dump


               k' <- A.alphaConvM k >>= dump

               constr <- KT.constraints k'

               dumpConstraints (Right constr)

               constr' <- I.inferM constr

               addEntries False (map (\(a,b) -> (typeid a, b)) constr')

               ee <- getEntries
--               let eee = M.toList ee
               mapM_ (liftIO . print) (M.toList ee)

               k'' <- return k' >>= Cn.propagate
                                >>= B.betaReduceM
                                >>= L.flattenM

--               c1 <- C.convert k'' -- >>= E.eliminate
               c1 <- C.conv2 k'' -- >>= E.eliminate

--               C.addTopLevelFunctions c1

               liftIO $ putStrLn $ prettyShow c1

               ir <- FC.convert c1

               liftIO $ putStrLn "\n\nTinyC\n" 
               liftIO $ putStrLn $ prettyShow ir
               liftIO $ putStrLn "\n" 

        reportStatus st

    where input "-" fn = BS.hGetContents stdin >>= fn
          input x fn = BS.readFile x >>= fn

dumpConstraints (Right x) = trace ("TRACE: constraints PIU PIU \n" ++ intercalate "\n" (map show x) ++ "\n") $ return x 
dumpConstraints (Left _) = error "Type infer error"

dump x = liftIO $ putStrLn (prettyShow x) >> return x

reportStatus (Left x)  = print x
reportStatus (Right x) = error "finished?"

--test f = do
--    e <- withInput parseTop f
--    let k = either (const $ error "Parse error") K.kNormalizeTop e
--    return $ L.flatten $ B.betaReduce $ A.alphaConv k

