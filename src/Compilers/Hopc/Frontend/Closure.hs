{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
module Compilers.Hopc.Frontend.Closure (convert, Closure(..), Fun(..), eliminate, addTopLevelFunctions, conv2)
                                        where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Error
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
              | CMakeCls KId [KId]
              | CApplCls KId [KId]
              | CApplDir KId [KId] 
             deriving (Show, Eq, Data, Typeable)


data Conv = Conv { fns :: [(KId, Fun)], cbind :: Maybe KId, cknown :: S.Set KId } deriving (Show)

type ConvM = StateT Conv CompileM


addTopLevelFunctions :: Closure -> CompileM ()
addTopLevelFunctions = undefined 

--addTopLevelFunctions (CLetR binds _) = mapM_ addFnEntry $ foldl bind [] binds
--    where bind acc (_,(CFun f)) = f:acc
--          bind acc _            = acc

--addTopLevelFunctions (CLet n (CFun f) _) = addFnEntry f

--addTopLevelFunctions x = return ()

--addFnEntry (Fun fn args free _) = do
--     TODO: type inference ?? Where?? When?
--    let vars = map TVar (args ++ free)
--    let func = TFun TFunLocal vars (TVar ("tret_" ++ fn))
--    addEntry fn func

data C2 = C2 { cfn :: M.Map KId KTree, cseen :: M.Map KId (S.Set KId), cfree :: M.Map KId [KId] }
data C3 = C3 { fv :: KId -> [KId], isglob :: KId -> Bool, fbind :: KId -> Maybe Fun, rn :: KId -> KId }

type C2M = StateT C2 CompileM
type C4M = StateT (M.Map KId KId) CompileM

conv2 :: KTree -> CompileM Closure
conv2 k = do
    (v,s) <- runStateT (p k) init
    entries <- entryList 
    let e = [ x | x@(n, (Entry t tp)) <- entries, tp == True]
    let glob = S.fromList (map fst e)

    fv <- forM (M.toList (cfn s)) $ \(n, l@(KLambda args b)) -> do
        let f = S.toList $ S.difference (S.fromList (free n l)) glob
        return (n, f)

    let finit@(C2{cfree = fs}) = initf init (M.fromList fv)

    let f n = maybe [] id (M.lookup n fs)
    let g n = S.member n glob

    q <- forM (M.toList (cfn s)) $ \(n, l@(KLambda args b)) -> do
        (b, s) <- runStateT (p b) $ finit 
        let fn = CFun (Fun (fname n) args (f n) b)
        return (fname n, fn)

    let bmap = M.fromList q

    liftIO $ putStrLn " -== "
    forM_ (M.toList bmap) $ liftIO . print
    liftIO $ putStrLn " -== "
--    trace (show bmap) $ return ()

    let fb n = maybe Nothing (\(CFun f) -> Just f) (M.lookup n bmap)

    let cl = addFns v q

    let bs = [n | (CMakeCls n args) <- universe cl]
    let rl = M.fromList $ map (\a -> (a, fname a)) $ S.toList $ S.fromList bs `S.difference` glob

    let rn  n = maybe n id (M.lookup n rl)

    liftIO $ putStrLn "------ GLOB ----------"
    forM_ (S.toList glob) $ liftIO . print
    liftIO $ putStrLn "------ REPL ----------"
    forM_ (M.toList rl) $ liftIO . print
    liftIO $ putStrLn "----------------------"

    (cl', s) <- runStateT (rewriteBiM (r (C3 f g fb rn)) cl) (M.empty)

    forM_ (M.toList s) $ liftIO . print

    return cl'

    where 
          p :: KTree -> C2M Closure
          p (KLet  n e e1) = liftM2 (CLet n) (liftM snd (pb (n,e))) (p e1)     -- undefined --CLet n ((snd.pb) (n,e)) (p e1)
          p (KLetR b e1)   = liftM2 CLetR (mapM pb b) (p e1)  -- undefined -- mapM_ pb b >>= \x -> CLetR  x >>= undefined --CUnit -- >> undefined --CLetR (map pb b) (p e1)
          p (KVar n) = lift (getEntryType n) >>= cvar n 
          p (KInt v) = return $ CInt v
          p (KStr s) = return $ CStr s
          p (KCond n e1 e2) = liftM2 (CCond n) (p e1) (p e2) -- undefined -- return $ CCond n (p e1) (p e2)
          p (KUnit) = return $ CUnit
          p (KApp n e) = return $ CApplCls n e
          p x = error $ "unexpected " ++ (show x) -- FIXME: compiler error
          pb (n, l@(KLambda a e)) = pl n l >> p e >> clos n >>= return . ((,) n)
          pb (n, x) = p x >>= return . ((,) n)
          pl n l = do
            st@(C2 {cfn = f}) <- get
            put st {cfn = M.insert n l f}

          cvar :: KId -> Maybe HType -> C2M Closure 
          cvar n (Just (TFun _ _ _)) = return $ CMakeCls n []
          cvar n (Just _) = return $ CVar n
          cvar n Nothing = lift $ throwError TypingError -- FIXME: more information

          clos n = do
            fv <- gets (M.lookup n . cfree)
            case fv of
                Nothing -> return $ CMakeCls n []
                Just [] -> return $ CMakeCls n []
                Just x  -> return $ CMakeCls n x 

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

          r c (CApplCls n args) | ((isglob c) n) =
            return $ Just $ CApplDir n args
 
          r c (CApplCls n args) | (not.(isglob c)) n && ((fv c) n == []) =
            return $ Just $ CApplDir ((rn c) n) args

          r c (CMakeCls n args) | ((rn c) n) == n = return Nothing
          r c (CMakeCls n args)  = rbind n ((fbind c) ((rn c) n))

          r c x = return Nothing

          rbind n Nothing = return Nothing 

          rbind n (Just p@(Fun fn a f r)) = do
            return $ Just $ CMakeCls fn f

          addFns (CLet n c c2) q = CLetR (q ++ [(n, c)]) c2
          addFns (CLetR b c2)  q = CLetR (b ++ q) c2

          init = C2 M.empty M.empty M.empty
          initf v@(C2 {cfree = fr}) f = v {cfree = f}


--    [KLetR]

-- evalState ( rewriteBiM tr k ) 0
--    where tr (KLambda n 



convert :: KTree -> CompileM Closure
convert k = do
    (cls, s) <- runStateT (conv k) convInit
    let binds = bindsOfCls cls
    return $ convDirectCls $ CLetR (map bindsOfFn (fns s) ++ binds) (cOfCls cls)
--    return $ CLetR (map bindsOfFn (fns s) ++ binds) (cOfCls cls)
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

          conv (KVar n) = do  --return $ CVar n
            tp <- lift $ getEntryType n
            trace ("TRACE: conv (KVar _) " ++ n ++ " " ++ (show tp)) $ return ()
            convVar n tp

          conv (KLet n e1 e2) = do
              modify ((\s@(Conv {cknown = k}) -> s{cknown = S.insert n k})) -- FIXME
              (n', e1') <- convBind (n, e1)
              e2' <- conv e2
              modify ((\s@(Conv {cknown = k}) -> s{cknown = S.delete n k})) -- FIXME
              return $ CLet n e1' e2'

          conv (KLetR binds e2) = do --- FIXME: OMFG
              st@(Conv{fns=fs1, cknown=k1}) <- get

              let ks = known binds

              put st { cknown = S.union k1 ks }
 
              (Conv {fns=fs}) <- execStateT (lift $ forM_ binds convBind) st
              put st { fns = fs1 ++ filter (not.(flip elem fs1)) fs }

              trace ("TRACE: SHOW fns \n" ++ intercalate "\n" (map show fs1)) $ return ()

              binds' <- forM binds convBind
              e2' <- conv e2

              st <- get --- FIXME
              put st{cknown = S.difference k1 ks} --- FIXME

              return $ CLetR binds' e2'

              where p (KLetR binds _) r = map fst binds ++ concat r
                    p (KLet n _ _) r = n : concat r
                    p _ r = concat r
                    known binds = S.fromList $ map fst binds

          conv (KApp n args) = do
              g <- lift $ getEntryType n

              fs <- gets fns
 
              mb <- getbind

              let fn = lookup n fs

--              trace (printf "TRACE: KApp %s %s --- has entry %s (bind: %s) (fs: %s) " n (show args) (show fn) (show mb) (show fs)) $ return ()

              let nofree = not $ if isJust fn
                                    then hasFree (fromJust fn)
                                    else True

              let self = maybe False (== n) mb
              let free = maybe [] getFree fn
              (direct, fn) <- case g of 
                               Just (TFun TFunLocal _ _) -> return $ (nofree, (fname n))
                               Just (TFun _ _ _)         -> return (True, n)
                               _                         -> lift $ throwError TypingError

              return $ if self || direct then CApplDir fn (args++free) else CApplCls n args

          conv (KCond t e1 e2) = do
            e1' <- conv e1
            e2' <- conv e2
            return $ CCond t e1' e2'

          conv wtf = error $ "WTF? " ++ show wtf

--          convVar n (TFun _ a r)  = 
          convVar n (Just v@(TFun TFunLocal _ _)) | (not.isVarT) v = return $ CMakeCls (fname n) [] --  error "GOT FUNCTION"
          convVar n (Just v@(TFun (TFunForeign _) _ _)) | (not.isVarT) v = return $ CMakeCls n [] --  error "GOT FUNCTION"
          convVar n (Just x) | (not.isVarT) x = return $ CVar n -- error $ "GOT WTF " ++ n ++ " " ++ (show x)
          convVar n x = error $ "GOT VARIABLE OF UNKNOWN TYPE " ++ n ++ " " ++ (show x) -- FIXME
          
--          convVar n (Just (TVar _)  = error "UNDEFINED TYPE"

          convBind (n, e@(KLambda argz eb)) = trace (printf "TRACE: convBind %s" n) $ do
              setbind n
              globs <- gs

              let (l, r) = partitionEithers $ para fn eb

              trace ("TRACE: globs " ++ show globs) $ return ()

--              let fset =  S.fromList (n : l ++ argz) `S.union` globs -- S.difference (S.fromList r) (S.fromList (n : l ++ argz) `S.union` globs)
              let rset = S.fromList r
              let fset = S.difference (S.fromList r) (S.fromList (n : l ++ argz)) -- `S.union` globs)

              known <- gets cknown
              trace ("TRACE: convBind known " ++ n ++ " " ++ (show known)) $ return ()

              addFun n argz [] (CUnit) -- FIXME: function's dummy. what a perversion...

              eb'  <- conv eb -- >>= lift . eliminate
--              eb'  <- conv eb

              let fv c  = do
                  let live = S.fromList $  para alive c
                  trace ("TRACE: fv live " ++ n ++ " "  ++ (show live)) $ return ()
                  trace ("TRACE: fv fset " ++ n ++ " " ++ (show fset)) $ return ()
                  trace ("TRACE: fv rset " ++ n ++ " " ++ (show rset)) $ return ()
                  return $ S.toList $ S.intersection live known 
             
              free <- fv eb'

              addFun n argz free eb'

              ft <- lift $ getEntry n

              trace ("TRACE: addFun : " ++ n ++  " " ++ (show ft) ++ " free vars" ++ (show free)) $ return ()

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
                    alive (CApplDir n args) r = n:args ++ concat r
                    alive (CCond n _ _) r = n : concat r
                    alive x r = concat r

          convBind (n, e) = do
              e' <- conv e
              return $ (n, e')

          convInit = Conv [] Nothing S.empty

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
    pPrintPrec l p (CFun (Fun n args free e)) = prettyParen True $ text "func" <+> prettyParen True (text n <+> (fsep $ map text (args++free)))
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


