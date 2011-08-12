{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
module Compilers.Hopc.Frontend.Closure where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Either
--import Control.Monad.Maybe
import Control.Monad.State
import Control.Monad.Trans
import Data.Data
import Data.Typeable
import Data.Generics.PlateData
import Text.PrettyPrint.HughesPJClass

import Text.Printf
import Debug.Trace

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.KTree
--import qualified Compilers.Hopc.Frontend.Eliminate as E

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


data Conv = Conv { fns :: [(KId, Fun)], cbind :: Maybe KId } deriving (Show)

type ConvM = StateT Conv CompileM

convert :: KTree -> CompileM Closure
convert k = do
    (cls, s) <- runStateT (conv k) convInit
    let binds = bindsOfCls cls
    return $ convDirectCls $ CLetR (map bindsOfFn (fns s) ++ binds) (cOfCls cls)
    where bindsOfCls (CLet n e1 e2) = [(n, e1)]
          bindsOfCls (CLetR binds e2) = binds
          bindsOfCls x = []
          cOfCls (CLet _ _ e) = e
          cOfCls (CLetR _ e) = e
          cOfCls e = e
          bindsOfFn (n, f@(Fun nm args free c)) = (fname n, CFun f)

          conv :: KTree -> ConvM Closure

          conv KUnit = return CUnit 
          conv (KInt n) = return $ CInt n
          conv (KStr s) = return $ CStr s

          conv (KVar n) = return $ CVar n

          conv (KLet n e1 e2) = do
              (n', e1') <- convBind (n, e1)
              e2' <- conv e2
              return $ CLet n e1' e2'

          conv (KLetR binds e2) = do
              st@(Conv{fns=fs1}) <- get
              (Conv {fns=fs}) <- execStateT (lift $ forM_ binds convBind) st
              put st { fns = fs1 ++ filter (not.(flip elem fs1)) fs }
              binds' <- forM binds convBind
              e2' <- conv e2
              return $ CLetR binds' e2'

          conv (KApp n args) = do
              g <- isGlobal n
              fs <- gets fns
 
              mb <- getbind

              let fn = lookup n fs

              trace (printf "TRACE: KApp %s %s --- has entry %s (bind: %s)" n (show args) (show fn) (show mb)) $ return ()

              let nofree = not $ if isJust fn then hasFree (fromJust fn) else True --False -- error $ "call of unknown function: " ++ fn --False
              let self = maybe False (== n) mb
              let free = maybe [] getFree fn
              let fn = if g then n else (fname n)
              return $ if g || nofree || self then CApplDir fn (args++free) else CApplCls n args

          conv wtf = error $ "WTF? " ++ show wtf

          convBind (n, e@(KLambda argz eb)) = trace (printf "TRACE: convBind %s" n) $ do
              setbind n
              globs <- gs

              let (l, r) = partitionEithers $ para fn eb

--              trace ("TRACE: para " ++ n ++ " " ++ " " ++ (show l) ++ (show r)) $ return () 

              let fset = S.difference (S.fromList r) (S.fromList (n : l ++ argz) `S.union` globs)
              let rset = S.fromList r
             
              addFun n argz [] (CUnit) -- FIXME: function's dummy. what a perversion...

              eb' <- conv eb -- >>= eliminate

              let fv c  = do
                  let live = S.fromList $ concat $ [ n:ns | CApplCls n ns <- universe c ] ++ [ [n] | CVar n <- universe c] ++ [ ns | CMakeCls _ ns <- universe c]
                  trace ("TRACE LIVE/FSET : " ++ n ++ " " ++ (show live) ++ (show fset)) $ return ()
                  return $ S.toList $ S.intersection live fset
             
              free <- fv eb'
                
              trace ("TRACE: convBind STEP 1 " ++ n ++ " "  ++ (show free)) $ return ()

              addFun n argz free eb'

              when (free /= []) $ do --- FIXME: real perversion: fix function body
                eb'' <- conv eb -- >>= eliminate
                free <- fv eb''
                trace ("TRACE: convBind STEP 2 " ++ n ++ " "  ++ (show free)) $ return ()
                addFun n argz free eb''

              clrbind

              trace (printf "convBind KLambda %s (%s) free %s" n (show argz) (show free)) $ do
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

          convInit = Conv [] Nothing

          clrbind :: ConvM ()
          clrbind = modify (\s -> s { cbind = Nothing })

          setbind :: KId -> ConvM ()
          setbind n = modify (\s -> s { cbind = Just n})

          getbind :: ConvM (Maybe KId)
          getbind = gets (cbind)

data ConvDir = ConvDir { efuncs :: (M.Map KId Fun), ebinds :: (M.Map KId (KId, [KId])), evars :: S.Set KId } deriving (Show)
type ConvDirM = State ConvDir 

convDirectCls :: Closure -> Closure
convDirectCls k = evalState (descendBiM tr k) init 
    where tr (CFun f@(Fun fn args free e)) = trace (printf "TRACE: CFun (Fun %s _ _ _)" fn) $ do
            fnDecl fn f 
            e' <- tr e
            return $ CFun (Fun fn args free e')
 
          tr (CLetR binds b) = trace "TRACE: tr (CLetR binds b)" $ do
            binds' <- mapM trB binds
            b'     <- tr b
            return $ CLetR binds b'

          tr (CLet n eb@(CMakeCls fn args) e) = trace "TRACE: tr (CLet _ (CMakeCls _ _) _)" $ do
            (_, eb') <- trB (n, eb)
            e' <- tr e
            return $ CLet n eb' e'

          tr (CLet n eb e) = trace "TRACE: tr (CLet _ _ _)" $ do
            bindVar n
            eb' <- tr eb
            e'  <- tr e
            return $ CLet n eb' e'

          tr x@(CApplCls n args) = do
            fn <- getBindFun n 
            trace (printf "TRACE: APPLY-CLOSURE %s %s" n (show fn)) $ do
                elimAppl fn x

          tr x = return x

          trB (n, x@(CMakeCls fn args)) = trace (printf "TRACE: trB %s" n) $ 
            bindCls n (fn, args) >> return (n, x)

          trB (n, x) = do
            bindVar n
            x' <- tr x
            return (n, x')

          elimAppl (Just (Fun fn _ [] _)) (CApplCls f args) = return $ CApplDir fn args
          
          elimAppl (Just (Fun fn _ free _)) x@(CApplCls f args) =
            trace (printf "TRACE: call-closure %s %s {%s}" f (show args) (show free)) $ do
                cls <- getBindCls f
                elimAppl2 cls x

          elimAppl f x = return x

          elimAppl2 (Just (fn, vars)) x@(CApplCls f args) = trace ( printf "TRACE: elimAppl2 %s {%s} %s" f (show vars) (show args) ) $ do
            ev <- gets evars
            let avail = foldl (&&) True $ map (flip S.member ev) vars
            trace ("TRACE: ConvDir " ++ show ev ++ " " ++ show avail) $ do
                if avail
                    then return $ CApplDir fn (args ++ vars)
                    else return x

          elimAppl2 Nothing x = return x
          elimAppl2 (Just _) x = return x

          bindCls n fn = modify (\s@(ConvDir{ebinds=eb}) -> s{ebinds=M.insert n fn eb})
           
          bindVar n = modify (\s@(ConvDir{evars=ev}) -> s{evars=S.insert n ev})

          fnDecl n f = modify (\s@(ConvDir{efuncs=ef}) -> s{efuncs=M.insert n f ef})

          lookBind n = gets (M.lookup n . ebinds)

          lookFun Nothing = return Nothing
          lookFun (Just (s, _)) = gets (M.lookup s . efuncs)

          lookCls Nothing  = return Nothing
          lookCls (Just x) = return (Just x)
 
          getBindFun n = lookBind n >>= lookFun
          getBindCls n = lookBind n >>= lookCls

          init = ConvDir M.empty M.empty S.empty


gs :: ConvM (S.Set KId)
gs = lift names

hasFree (Fun _ _ free _) = free /= []
getFree (Fun _ _ free _) = free

isGlobal :: KId -> ConvM Bool
isGlobal n = gs >>= (return . S.member n)
--isGlobal n = return $ False -- TODO: use CompileM monad to check globals 

addFun :: KId -> [KId] -> [KId] -> Closure -> ConvM ()
addFun n args free bdy = do
    s@(Conv { fns = fs }) <- get
    let fs' = filter (not.(== n).fst) fs
    put $ s { fns = fs' ++ [(n, funOf n args free bdy)] }

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
                                    <+> prettyParen True (prettyParen True $ text i <+> pPrintPrec l p e1)
                                    $$ nest 2 (pPrintPrec l p e2)
    pPrintPrec l p (CLetR binds e) = prettyParen True $ text "letrec"
                                     <+> prettyParen True ( fsep $ map (\(n, e1) -> prettyParen True (text n <+> pPrintPrec l p e1)) binds )
                                     $$ nest 2 (pPrintPrec l p e)

