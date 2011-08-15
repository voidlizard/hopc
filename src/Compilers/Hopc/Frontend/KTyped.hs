module Compilers.Hopc.Frontend.KTyped where

import qualified Data.Map as M
import Data.List
import Data.Data hiding (typeOf)
import Data.Typeable hiding (typeOf)
import Data.Generics.PlateData
import Data.Generics.Biplate
import Control.Monad.State
import Control.Monad.Trans

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Frontend.Types

import Debug.Trace

type KTypedM = StateT (M.Map KId HType) CompileM

constraints :: KTree -> CompileM [(HType, HType)]
constraints k = do
   
    dict <- getEntries
    c <- evalStateT (mapM constr (universe k)) (init dict)

    trace ("TRACE: constraints \n" ++ intercalate "\n" (map show c)) $ return ()

    error "STOP"
--    return $ concat c 

    where constr :: KTree -> KTypedM [(HType, HType)]

          constr (KLet n e1 _) = do
            tp <- typeOf e1
            mem n tp
            return [(TVar n, tp)]

          constr (KLetR b e2) = do
            forM b $ \(n, e) -> do
                tp <- typeOf e
                mem n tp
                return $ (TVar n, tp)

          constr (KLambda args _) = do
            forM_ args $ \n -> trace ("TRACE: constr mem " ++ (show n)) $ mem n (TVar n)
            return []

          constr x = return []

--          tp :: KTree -> KTypedM KTree
--          tp x@(KLet n e1 e2) = undefined -- return $ tpB (n, x) : concat r
--          tp x@(KLetR bs e) = undefined -- return $ map tpB bs ++ concat r
--          tp x@(KApp n args)  = undefined -- return $ concat r -- undefined
--          tp x = undefined -- return $ concat r

--          tpB (n, x) = undefined -- return $ (TVar n, typeOf x)

          init x = x

          mem :: KId -> HType -> KTypedM ()
          mem n t = modify (M.insert n t) 

          remem' n = gets (M.lookup n)

          remem n = do
            v <- gets (M.lookup n)
            return $ maybe (TVar n) id v

          typeOf :: KTree -> KTypedM HType

          typeOf (KLet _ _ e) = typeOf e

          typeOf (KLetR _ e)  = typeOf e

          typeOf (KInt _) = return TInt

          typeOf (KStr _) = return TStr

          typeOf (KVar n) = remem n 

          typeOf (KUnit) = return TUnit

          typeOf (KLambda args b) = do
            rt <- typeOf b
            at <- mapM typeOf $ map KVar args
            return $ TFun TFunLocal at rt

          typeOf (KApp n args) = do
            st <- get

            tp <- remem' n
            return $ case tp of
                        Nothing -> TVar (n ++ "_fake")-- error $ "WTF? UNKNOWN TYPE FOR " ++ n -- FIXME: error handling
                        Just (TFun _ _ rt)  -> rt
                        x       -> error $ "NOT APPLICABLE  "  ++ n

          typeOf x = error ("oops " ++ show x)

