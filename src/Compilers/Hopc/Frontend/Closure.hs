{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
module Compilers.Hopc.Frontend.Closure (Closure(..), Fun(..), eliminate, conv2, updateClosures)
                                        where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Writer
import Data.Data
import Data.Typeable
import Data.Generics.PlateData
import Text.PrettyPrint.HughesPJClass

import Text.Printf
import Debug.Trace

import Compilers.Hopc.Compile
import Compilers.Hopc.Error
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
              | CMakeCls KId (Maybe [KId])
              | CApplCls KId [KId]
              | CApplDir KId [KId]
             deriving (Show, Eq, Data, Typeable)

data C2 = C2 { cfn :: M.Map KId KTree, cseen :: M.Map KId (S.Set KId), cfree :: M.Map KId [KId] }
data C3 = C3 { fv :: KId -> [KId],
               isglob :: KId -> Bool,
               fbind :: KId -> Maybe Fun, 
               rn :: KId -> KId,
               fv2 :: KId -> [KId] -> [KId]
             }

type C2M = StateT C2 CompileM
type C4M = StateT (M.Map KId KId) CompileM

entryPoint = "ENTRY" --- TODO: options?

conv2 :: KTree -> CompileM Closure
conv2 k' = do

--    k'' <- rewriteKTree k'

    addEntry False entryPoint (TFun TFunLocal [] TUnit) -- FIXME: hack?
    let k = KLet entryPoint  (KLambda [] k') (KApp entryPoint [])
    setEntryPoint (fname entryPoint)

    (v,s) <- runStateT (p k) init
    entries <- entryList 
    let e = [ x | x@(n, (Entry t tp)) <- entries, tp == True]
    let glob = S.fromList (map fst e)

    fv <- forM (M.toList (cfn s)) $ \(n, l@(KLambda args b)) -> do
        let f = S.toList $ S.difference (S.fromList (free n l)) glob
        return (n, f)

    let finit@(C2{cfree = fs}) = initf init (M.fromList fv)

    let f n = maybe [] id (M.lookup n fs)
    let fv2 n k = k 
    let g n = S.member n glob

    q <- forM (M.toList (cfn s)) $ \(n, l@(KLambda args b)) -> do
        (b, s) <- runStateT (p b) $ finit
        let fn = CFun (Fun (fname n) args (f n) b)
        return (fname n, fn)

    let bmap = M.fromList q

    let fb n = maybe Nothing (\(CFun f) -> Just f) (M.lookup n bmap)

    let cl = addFns v q

    let bs = [n | (CMakeCls n args) <- universe cl]
    let rl = M.fromList $ map (\a -> (a, fname a)) $ S.toList $ S.fromList bs `S.difference` glob
    let rl' = M.fromList $ map (\(a,b) -> (b,a)) $ M.toList rl

    let rn  n = maybe n id (M.lookup n rl)

    (cl', s) <- runStateT (rewriteBiM (r (C3 f g fb rn fv2 )) cl) (M.empty)

    let fs2 = M.fromList $ [(f, p) | CFun p@(Fun f args free bdy) <- universe cl']
    let f2  n = maybe [] (\(Fun _ _ f _) -> f) (M.lookup n fs2)
    let fb2 n = M.lookup n fs2
    let flt n k = f2 n

    let nr n = M.lookup n rl'
--    let fss n = maybe [] id (f2 n)
    let ct = rewriteBi (rself flt nr f2) cl'

    (cl'', s) <- runStateT (rewriteBiM (r (C3 f2 g fb2 rn flt)) ct) (M.empty)

    let bs = [(fn, p) | (CFun p@(Fun fn args free b)) <- universe cl'']

    entries <- getEntries

    forM_ bs $ \(fn, (Fun _ args free b)) -> do
        let nm =  M.lookup fn rl'
        when ((not.isJust) nm) $ error "COMPILER ERROR" -- FIXME
        let n = fromJust nm

        let tn' = M.lookup n entries
        let tf' = map (flip M.lookup entries) free
        let tf = catMaybes tf'

        when ((not.isJust) tn') $ error "COMPILER ERROR / TYPE ERROR" -- FIXME

        let tn = fromJust tn'

        when (length tf /= length free) $ error "COMPILER ERROR / TYPE ERROR" -- FIXME

        nf <- case tn of
               (TFun spec args rt) -> return $ TFun spec (args ++ tf) rt
               _                   -> error $ "COMPILER ERROR / TYPE ERROR [CALL OF NOT APPLICABLE] " ++ n

        addEntry False fn nf

    withWrappers cl''

    where 
          p :: KTree -> C2M Closure
          
          p (KLet  n e e1) = liftM2 (CLet n) (liftM snd (pb (n,e))) (p e1) -- FIXME: BUG -- POSSIBLE make-closure at tail position
          p (KLetR b e1)   = liftM2 CLetR (mapM pb b) (p e1)
          p (KVar n) = lift (getEntryType n) >>= cvar n
          p (KInt v) = return $ CInt v
          p (KStr s) = return $ CStr s
          p (KCond n e1 e2) = liftM2 (CCond n) (p e1) (p e2)
          p (KUnit) = return $ CUnit
          p (KApp n e) = return $ CApplCls n e
          p x = error $ "unexpected " ++ (show x) -- FIXME: compiler error
          pb (n, l@(KLambda a e)) = pl n l >> p e >> clos n >>= return . ((,) n)
          pb (n, x) = p x >>= return . ((,) n)
          pl n l = do
            st@(C2 {cfn = f}) <- get
            put st {cfn = M.insert n l f}

          cvar :: KId -> Maybe HType -> C2M Closure 
          cvar n (Just (TFun _ _ _)) = return $ CMakeCls n Nothing
          cvar n (Just _) = return $ CVar n
          cvar n Nothing = lift $ throwError TypingError -- FIXME: more information

          clos n = do
            fv <- gets (M.lookup n . cfree)
            case fv of
                Nothing -> return $ CMakeCls n Nothing
                Just [] -> return $ CMakeCls n Nothing
                Just x  -> return $ CMakeCls n (Just x)

          fl (KLambda _ _) r = [] 
          fl (KVar n )     r = concat r ++ [Right n]
          fl (KApp n _)    r = concat r ++ [Right n]
          fl (KLet n _ _)  r = concat r ++ [Left n]
          fl (KCond n _ _) r = concat r ++ [Right n]
          fl (KLetR bs _)  r = concat r ++ map (Left . fst) bs
          fl x r             = concat r

          free n (KLambda args b) = 
             let (l, r) = partitionEithers $ para fl b
                 fset = S.fromList r `S.difference` S.fromList (n:l ++ args)
             in S.toList fset

          free n x = []

          r :: C3 -> Closure -> C4M (Maybe Closure)

          r c (CApplCls n args) | ((isglob c) n) = do
            let args' = (fv2 c) n args
            return $ Just $ CApplDir n args'
 
          r c (CApplCls n args) | (not.(isglob c)) n = do
            let args' = (fv2 c) n args
            let nm = ((rn c) n)
            let b = ((fbind c) nm)
            case b of
                Nothing -> return Nothing
                (Just (Fun n _ [] _)) -> return $ Just $ CApplDir ((rn c) n) args'
                (Just (Fun n _ xs _)) -> return Nothing

          r c (CMakeCls n Nothing) = return $ Just $ CMakeCls ((rn c) n) (Just ((fv c) n))

          r c (CMakeCls n (Just f)) = do -- return $ Just $ CMakeCls 
            let f' = (fv2 c) n f
            if f' == f
                then return Nothing
                else return $ Just $ CMakeCls ((rn c) n) (Just f')

          r c (CFun (Fun fn a f b)) = do
--            b <- eliminate b'
            let aset = S.fromList $ alive b
            
--            trace ("ALIVE " ++ fn ++ " f:" ++ show f) $ trace (show aset) $ return ()

            let f' = filter (flip S.member aset) f

            if f' == f 
                then return Nothing
                else return $ Just $ CFun (Fun fn a f' b)

          r c x = return Nothing

          rself :: (KId -> [KId] -> [KId]) -> (KId -> Maybe KId) -> (KId -> [KId]) -> Closure -> Maybe Closure
          rself flt nr fv (CApplCls n args) =
            let nm = nr (fname n)
            in case nm of
                Just x | x == n -> Just $ CApplDir (fname n) $ args ++ (flt n (fv (fname n)))
                _ -> Nothing

          rself _ _ _ _ = Nothing

          alive c = para a c

          a (CVar n) r = n : concat r
          a (CMakeCls n (Just args)) r = n:args ++ concat r
          a (CMakeCls n Nothing) r = n : concat r
          a (CApplCls n args) r = n:args ++ concat r
          a (CApplDir n args) r = n:args ++ concat r
          a (CCond n _ _) r = n : concat r
          a x r = concat r

          init = C2 M.empty M.empty M.empty
          initf v@(C2 {cfree = fr}) f = v {cfree = f}


addFns (CLet n c c2) q = CLetR (q ++ [(n, c)]) c2
addFns (CLetR b c2)  q = CLetR (q ++ b) c2

withWrappers :: Closure -> CompileM Closure
withWrappers cl = do
  e <- getEntries
  let bs = M.fromList [(n,n) | (CMakeCls n args) <- universe cl]
  let fs = e `M.intersection` bs
  (_,funcs) <- runWriterT $ 
    forM_ (M.toList fs) $ ifNativeFun $ \(n,t@(TFun (TFunForeign fn) a r)) -> do
      tmps <- lift $ mapM (const nextTmp) a >>= return . map (((++) "ta_").show)
      let wrap = (fname_wrap n)
--      trace (printf "%s %s %s" wrap  (show t) (show tmps)) $ return ()
      tell [(fn, (wrap, CFun (Fun wrap tmps [] (CApplDir fn tmps))))]
      lift $ addEntry False wrap (TFun TFunLocal a r)

  let remap = M.fromList $ map (\x -> (fst x, (fst.snd) x)) funcs

  return $ addFns (rewriteBi (trCls remap) cl) (map snd funcs)

  where 
    ifNativeFun :: ((KId, HType) -> WT ()) -> (KId, HType) -> WT () 
    ifNativeFun m x@(n, TFun (TFunForeign  fn) a r) = m x 
    ifNativeFun m _ = return ()
  
    trCls r (CMakeCls n args) = maybe Nothing (\n -> Just (CMakeCls n args)) (M.lookup n r)
    trCls _ _ = Nothing


type WT = WriterT [(KId, (KId, Closure))] CompileM

fname_wrap n = "fun_wrap_" ++ n

fname n = "fun_" ++ n

instance Pretty Closure where
    pPrintPrec _ _ (CUnit)  = text "()" 
    pPrintPrec _ _ (CInt n) = integer n
    pPrintPrec _ _ (CStr s) = (text.show) s
    pPrintPrec _ _ (CVar v) = text v
    pPrintPrec l p (CFun (Fun n args free e)) = prettyParen True $ text "func" <+> text n
                                                <+> prettyParen True (fsep $ map text (args))
                                                <+> prettyParen True (fsep $ map text (free))
                                                <+> pPrintPrec l p e
    pPrintPrec l p (CApplCls n a) = prettyParen True $ text "apply-closure" <+> text n <+> ( fsep $ map text a )
    pPrintPrec l p (CApplDir n a) = prettyParen True $ text "apply-direct" <+> text n <+> ( fsep $ map text a )
    pPrintPrec l p (CMakeCls s f) = prettyParen True $ text "make-closure" <+> text s <+> maybe empty (fsep.(map text)) f
    pPrintPrec l p (CLet i e1 e2) = prettyParen True $ text "let"
                                    <+> prettyParen True (prettyParen True $ text i <+> pPrintPrec l p e1)
                                    $$ nest 2 (pPrintPrec l p e2)
    pPrintPrec l p (CLetR binds e) = prettyParen True $ text "letrec"
                                     <+> prettyParen True ( fsep $ map (\(n, e1) -> prettyParen True (text n <+> pPrintPrec l p e1)) binds )
                                     $$ nest 2 (pPrintPrec l p e)
    pPrintPrec l p (CCond c e1 e2) = prettyParen True $ text "if" <+> text c <+> (pPrintPrec l p e1) <+> (pPrintPrec l p e2)


updateClosures :: Closure -> CompileM ()
updateClosures c =
  delClosures >> forM_ [n|(CMakeCls n _) <- universe c] addClosure

data Elim = Elim { elenv :: S.Set KId } 
type ElimM = StateT Elim CompileM

eliminate :: Closure -> CompileM Closure
eliminate k = do 
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
          u (CMakeCls n (Just args)) r = concat r ++ n:args
          u (CMakeCls n Nothing) r = n:concat r
          u (CCond n _ _) r = concat r ++ [n]
--          u (CFun (Fun n _ _ _)) r = trace ("oops! got CFun " ++ n ++ show r) $ concat r
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


