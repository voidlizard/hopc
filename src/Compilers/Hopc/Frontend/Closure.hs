{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
module Compilers.Hopc.Frontend.Closure where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
--import Control.Monad.Maybe
import Control.Monad.State
import Data.Data
import Data.Typeable
import Data.Generics.PlateData
import Text.PrettyPrint.HughesPJClass

import Text.Printf
import Debug.Trace

import Compilers.Hopc.Frontend.KTree

data Fun = Fun KId [KId] [KId] Closure deriving (Show, Eq, Data, Typeable)

data Closure =  CInt Integer
              | CStr String
              | CUnit
              | CLet  KId Closure Closure
              | CLetR [(KId, Closure)] Closure
              | CVar KId
              | CFun Fun
              | CMakeCls KId [KId]
              | CApplCls KId [KId]
              | CApplDir KId [KId] 
             deriving (Show, Eq, Data, Typeable)


data Conv = Conv { fns :: [(KId, Fun)], glob :: S.Set KId } deriving (Show)

type ConvM = State Conv

convert :: [KId] -> KTree -> Closure
convert g k = 
    let (cls, s) = runState (conv k) (convInit g)
        binds = bindsOfCls cls
    in eliminate $ CLetR (map bindsOfFn (fns s) ++ binds) (cOfCls cls)
    where bindsOfCls (CLet n e1 e2) = [(n, e1)]
          bindsOfCls (CLetR binds e2) = binds
          bindsOfCls x = []
          cOfCls (CLet _ _ e) = e
          cOfCls (CLetR _ e) = e
          cOfCls e = e
          bindsOfFn (n, f@(Fun nm args free c)) = (fname n, CFun f)

data Elim = Elim { efuncs :: (M.Map KId Fun), ebinds :: (M.Map KId KId) } deriving (Show)
type ElimM = State Elim 

eliminate :: Closure -> Closure
eliminate k = 
    let fns = M.fromList [ (nm, f)  | (CFun f@(Fun nm args free _)) <- universe k ]
        b1  = [ (n, fn)   | (CLet n m@(CMakeCls fn args) _) <- universe k ] :: [(KId, KId)]
        b2  = foldl withBind b1 $ concat [binds | (CLetR binds _) <- universe k]
        fs  = foldl (withFn fns) (M.empty) b2
--     in trace ( show fs) k
     in rewrite (tr fns) k
    where withBind acc (n, b@(CMakeCls fn args)) = (n, fn) : acc
          withBind acc x = acc
          withFn :: M.Map KId Fun -> M.Map KId Fun -> (KId, KId) -> M.Map KId Fun
          withFn fns m (n,fn) =
            let d = M.lookup fn fns
            in case d of
                Nothing -> m
                Just b  -> M.insert n b m

          tr :: M.Map KId Fun -> Closure -> Maybe Closure
          tr fns (CApplCls n args) =
            M.lookup n fns >>= \fb@(Fun n _ _ _) -> if hasFree fb
                                                        then error "JOPA!"
                                                        else Just $ CApplDir n args
          tr fns x = Nothing

--    in rewriteBiM
--    let (c, s) = runState (rewriteBiM tr k) init
--    in trace (show s) $ c
--    where tr :: Closure -> ElimM (Maybe Closure)
--          tr (CFun f) = withFun f >> return Nothing
--          tr (CLetR binds _) = forM_ binds withBind >> return Nothing 
--          tr (CLet n c@(CMakeCls fn _) _) = withBind (n, c) >> return Nothing
--          tr (CApplCls fi args) = withAppl fi args
--          tr x = return Nothing

--          withFun f@(Fun n _ _ _) = modify (\x -> x { efuncs = M.insert n f (efuncs x)})

--          withBind (n, CMakeCls fn _) = trace ("withBind " ++ n) $ do -- modify (\x -> x { ebinds = M.insert n fn (ebinds x)})
--            st@(Elim { ebinds = eb }) <- get
--            trace ("STATE: " ++ show eb) $ do
--                put $ st { ebinds = M.insert n fn eb }

--          withBind (n, _) = return ()

--          withAppl :: KId -> [KId] -> ElimM (Maybe Closure)
--          withAppl fi args = trace ("withAppl " ++ fi) $ do
--            s@(Elim { ebinds = eb, efuncs = ef } ) <- get
--            trace ("\n\n --- FUNCS " ++ fi ++ " "  ++ show eb ++ " " ++ show ef) $ do
--                fn <- gets (M.lookup fi . ebinds)
--                fb <- look fn
--                return $ maybe Nothing (\f@(Fun n _ _ _) -> if not (hasFree f) then Just (CApplDir n args) else Nothing) fb

--          look :: Maybe KId -> ElimM (Maybe Fun)
--          look Nothing   = return $ Nothing
--          look (Just n)  = do
--            fb <- gets (M.lookup n . efuncs)
--            return fb

--          init = Elim { efuncs = M.empty, ebinds = M.empty }

hasFree (Fun _ _ free _) = free /= []

conv KUnit = return CUnit 
conv (KInt n) = return $ CInt n
conv (KStr s) = return $ CStr s

conv (KVar n) = return $ CVar n

conv (KLet n e1 e2) = do
    (n', e1') <- convBind (n, e1)
    e2' <- conv e2
    return $ CLet n e1' e2'

conv (KLetR binds e2) = do
    binds' <- forM binds convBind
    e2' <- conv e2
    return $ CLetR binds' e2'

conv (KApp n args) = do
    g <- isGlobal n
    return $ if g then CApplDir n args else CApplCls n args

conv wtf = error $ "WTF? " ++ show wtf

convBind (n, e@(KLambda argz eb)) = do
    globs <- gs
    let (l, r) = partitionEithers $ para fn eb
    let fset = S.difference (S.fromList r) (S.fromList (n : l ++ argz) `S.union` globs)
    let free = filter (flip S.member fset) r
    eb' <- conv eb
    addFun n argz free eb' 
    return $ (n, CMakeCls (fname n) free)
    where fn (KLambda _ _) r = [] 
          fn (KVar n )     r = concat r ++ [Right n]
          fn (KApp n _)    r = concat r ++ [Right n]
          fn (KLet n _ _)  r = concat r ++ [Left n]
          fn (KLetR bs _)  r = concat r ++ map (Left . fst) bs
          fn x r             = concat r

convBind (n, e) = do
    e' <- conv e
    return $ (n, e')

convInit g = Conv [] (S.fromList g)

gs :: ConvM (S.Set KId)
gs = gets glob 

isGlobal n = gs >>= (return . S.member n)

addFun :: KId -> [KId] -> [KId] -> Closure -> ConvM ()
addFun n args free bdy = do
    s@(Conv { fns = fs }) <- get
    put $ s { fns = fs ++ [(n, funOf n args free bdy)] }

funOf n args free bdy = (Fun (fname n) args free bdy)

fname n = "fun_" ++ n

instance Pretty Closure where
    pPrintPrec _ _ (CUnit)  = text "()" 
    pPrintPrec _ _ (CInt n) = integer n 
    pPrintPrec _ _ (CStr s) = (text.show) s
    pPrintPrec _ _ (CVar v) = text v
    pPrintPrec l p (CFun (Fun n args free e)) = prettyParen True $ text "func"
                                                <+> (prettyParen True (fsep $ map text (args++free) ))
                                                <+> pPrintPrec l p e
    pPrintPrec l p (CApplCls n a) = prettyParen True $ text "apply-closure" <+> text n <+> ( fsep $ map text a )
    pPrintPrec l p (CApplDir n a) = prettyParen True $ text "apply-direct" <+> text n <+> ( fsep $ map text a )
    pPrintPrec l p (CMakeCls s f) = prettyParen True $ text "make-closure" <+> text s <+> ( fsep $ map text f )
    pPrintPrec l p (CLet i e1 e2) = prettyParen True $ text "let"
                                    <+> (prettyParen True $ text i <+> pPrintPrec l p e1)
                                    $$ nest 2 (pPrintPrec l p e2)
    pPrintPrec l p (CLetR binds e) = prettyParen True $ text "letrec"
                                     <+> prettyParen True ( fsep $ map (\(n, e1) -> prettyParen True (text n <+> pPrintPrec l p e1)) binds )
                                     $$ nest 2 (pPrintPrec l p e)

