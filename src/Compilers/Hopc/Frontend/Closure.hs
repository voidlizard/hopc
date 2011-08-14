{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
module Compilers.Hopc.Frontend.Closure (convert, Closure(..), Fun(..), eliminate, addTopLevelFunctions)
                                        where

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
import Compilers.Hopc.Frontend.Types
--import qualified Compilers.Hopc.Frontend.Eliminate as E

data Fun = Fun KId [KId] [KId] Closure deriving (Show, Eq, Data, Typeable)

data Closure =  CInt Integer
              | CStr String
              | CUnit
              | CLet  KId Closure Closure
              | CLetR [(KId, Closure)] Closure
              | CCond KId Closure Closure
              | CVar KId
              | CFun Fun
              | CMakeCls KId [KId]
              | CApplCls KId [KId]
              | CApplDir KId [KId] 
             deriving (Show, Eq, Data, Typeable)


data Conv = Conv { fns :: [(KId, Fun)], cbind :: Maybe KId } deriving (Show)

type ConvM = StateT Conv CompileM


addTopLevelFunctions :: Closure -> CompileM ()

addTopLevelFunctions (CLetR binds _) = mapM_ addFnEntry $ foldl bind [] binds
    where bind acc (_,(CFun f)) = f:acc
          bind acc _            = acc

addTopLevelFunctions (CLet n (CFun f) _) = addFnEntry f

addTopLevelFunctions x = return ()

addFnEntry (Fun fn args free _) = do
    -- TODO: type inference ?? Where?? When?
    let vars = map TVar (args ++ free)
    let func = TFun TFunLocal vars (TVar ("tret_" ++ fn))
    addEntry fn func

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

--              trace (printf "TRACE: KApp %s %s --- has entry %s (bind: %s) (fs: %s) " n (show args) (show fn) (show mb) (show fs)) $ return ()

              let nofree = not $ if isJust fn
                                    then hasFree (fromJust fn)
                                    else True 
--                                         error $ "call of unknown function: " ++ (show n) ++ " " ++ (show fn)  ++ " <<>>>>" ++ (show fs)--False
--              let nofree = not $ if isJust fn then hasFree (fromJust fn) else True --False -- error $ "call of unknown function: " ++ fn --False
              let self = maybe False (== n) mb
              let free = maybe [] getFree fn
              let fn = if g then n else (fname n)
              return $ if g || nofree || self then CApplDir fn (args++free) else CApplCls n args

          conv (KCond t e1 e2) = do
            e1' <- conv e1
            e2' <- conv e2
            return $ CCond t e1' e2'

          conv wtf = error $ "WTF? " ++ show wtf

          convBind (n, e@(KLambda argz eb)) = trace (printf "TRACE: convBind %s" n) $ do
              setbind n
              globs <- gs

              let (l, r) = partitionEithers $ para fn eb

              let fset = S.difference (S.fromList r) (S.fromList (n : l ++ argz) `S.union` globs)
              let rset = S.fromList r
             
              addFun n argz [] (CUnit) -- FIXME: function's dummy. what a perversion...

              eb'  <- conv eb -- >>= lift . eliminate
--              eb'  <- conv eb

              let fv c  = do
                  let live = S.fromList $  para alive c
                  return $ S.toList $ S.intersection live fset
             
              free <- fv eb'

              addFun n argz free eb'

              when (free /= []) $ do --- FIXME: real perversion: fix function body
                eb'' <- conv eb -- >>= lift . eliminate
                free <- fv eb''
                addFun n argz free eb''

              clrbind

              trace (printf "convBind KLambda %s (%s) free %s" n (show argz) (show free)) $ do
                  return $ (n, CMakeCls (fname n) free)

              where fn (KLambda _ _) r = [] 
                    fn (KVar n )     r = concat r ++ [Right n]
                    fn (KApp n _)    r = concat r ++ [Right n]
                    fn (KLet n _ _)  r = concat r ++ [Left n]
                    fn (KCond n _ _) r = concat r ++ [Right n]
                    fn (KLetR bs _)  r = concat r ++ map (Left . fst) bs
                    fn x r             = concat r

                    alive (CVar n) r = n : concat r
                    alive (CMakeCls n args) r = n:args ++ concat r
                    alive (CApplCls n args) r = n:args ++ concat r
                    alive (CCond n _ _) r = n : concat r
                    alive x r = concat r

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

          tr x@(CApplDir n args) = do
            fn <- getBindFun n 
            trace (printf "TRACE: APPLY-DIRECT %s %s" n (show fn)) $ do
                return $ maybe x (\(Fun fn _ _ _) -> CApplDir fn args) fn

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
    pPrintPrec l p (CCond c e1 e2) = prettyParen True $ text "if" <+> text c <+> (pPrintPrec l p e1) <+> (pPrintPrec l p e2)


data Elim = Elim { elenv :: S.Set KId } 
type ElimM = StateT Elim CompileM

eliminate :: Closure -> CompileM Closure
eliminate k = trace "TRACE: eliminate" $
    evalStateT (descendBiM tr k) init
    where tr :: Closure -> ElimM Closure

          tr x@(CLet n e1 e2) = do
            e1' <- tr e1
            e2' <- tr e2
            let live = S.fromList $ usage e2
            if S.member n live || effect e1'
                then return $ CLet n e1' e2'
                else return e2'

          tr x@(CLetR binds e) = do
            e'     <- tr e
            binds' <- elim e [] (reverse binds) --mapM trB binds
            return $ CLetR binds' e'

          tr (CFun (Fun fn args free e)) = do
            e' <- tr e
            return $ CFun (Fun fn args free e')

          tr x@(CCond n e1 e2) = do
            e1' <- tr e1
            e2' <- tr e2
            return $ CCond n e1' e2'

          tr x = return x

          trB (n, e)  = do
            e' <- tr e
            return (n, e')

          flt live (n, (CFun _)) = S.member n live
          flt live (n, e) = S.member n live || effect e

          usage x = para u x
          u (CApplCls n args) r = concat r ++ n:args
          u (CApplDir n args) r = concat r ++ n:args
          u (CVar n) r = concat r ++ [n]
          u (CMakeCls n args) r = concat r ++ n:args
          u (CCond n _ _) r = concat r ++ [n]
          u x r = concat r

          init = Elim S.empty

          elim e l (r:rs) = do
            let ebs = e : map snd l
            let rbs = map snd rs
            let live = S.fromList $ concat $ map usage (ebs ++ rbs)
--            trace ("TRACE: eliminating " ++ (show $ fst r) ++ " " ++ (show live)) $ return ()
            r' <- trB r
            if flt live r'
              then elim e (r':l) rs
              else elim e l rs

          elim e l [] = return l

effect :: Closure -> Bool
effect k = foldl (||) False $ para eff k
    where eff (CVar n) r = False : concat r
          eff (CMakeCls n args) r = False : concat r
          eff (CFun (Fun n _ _ e)) r = False : concat r
          eff (CInt _) r = False : concat r
          eff (CStr _) r = False : concat r
          eff (CCond _ _ _) r = False : concat r
          eff CUnit r = False : concat r
          eff x r = True : concat r


