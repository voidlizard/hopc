{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.VM (
    module Compilers.Hopc.Backend.TinyC.VM.Types
  , fromIR
  , spillASAP
--  , spillFreeVars
  ) where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S 
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans
import Compiler.Hoopl

import Debug.Trace
import Text.Printf 

import Compilers.Hopc.Compile
import Compilers.Hopc.Id
import Compilers.Hopc.Frontend.Types
import Compilers.Hopc.Backend.TinyC.Live
import Compilers.Hopc.Backend.TinyC.IR (Insn)
import Compilers.Hopc.Backend.TinyC.VM.Types
--import Compilers.Hopc.Backend.TinyC.Regs
import qualified Compilers.Hopc.Backend.TinyC.IR as I

fromIR :: TDict -> FactBase Live -> RegAllocation -> I.Proc -> M Proc

fromIR dict live ra p@(I.Proc {I.entry = e, I.body = g, I.name = n, I.args = as, I.freevarsnum=fvn}) = do
  let (GMany _ bbb _) = g
  let blocks = postorder_dfs_from bbb e

--  mapM_ printBlock blocks
--  trace "---" $ return ()

  l <- freshLabel 
  (vm, st) <- flip runReaderT initR $ flip runStateT (initS l) $
          liftM (wipeSnots . mergeBlocks e) $
            mapM (\b -> foldBlockNodesF (liftVM trNode) b emptyM) blocks

  return $ Proc { name = n
                , arity = length as
                , args = as
                , slotnum = (rsSlotMax st)
                , body = vm
                , entrypoint = e
                , freevarsnum = (I.freevarsnum p)
                , allocation = ra
                }
  where

    trNode :: forall e x. Insn e x -> TrM TOp

    trNode (I.Label n) = do
      label n
      spill <- spillAtBlock n
      nl <- gets rsTLabel
      let afterspill = if n == e then [Label nl] else []
      chunkM $ Label n : spill ++ afterspill

    trNode x@(I.Call l ct _ _) = do
      let callT = varType (callvar ct) dict
      let isTail = analyzeTailCall l ct callT
      return callT >>= callOf (makeTail isTail x)
--      trace ("ANALYZE TAIL CALL " ++ n) $
--        trace (show x ++ " " ++ show isTail) $
--          return ()
--      trace "SUCCESSORS " $ trace (show isTail) $ return ()
 
    trNode (I.Const c n) = do
      r <- reg n
      chunkM [Const c r]

    trNode (I.Assign to from) = do
      u  <- unspill from Nothing
      r1 <- reg from 
      r2 <- reg to
      chunkM $ u ++ [Move r1 r2]

    trNode (I.Branch l) = chunkM [Branch l]

    trNode (I.Cond n l1 l2) = do
      u  <- unspill n Nothing
      r <- reg n
      chunkM $ u ++ [BranchTrue r l1, Branch l2]

    trNode (I.Return n) | notVoid = do
      ur  <- unspill n (Just R1)
      unsp <- unspill activationRecordVariable (Just closureReg)
      rv <- reg n
      let ret = case ur of
                []  -> Move rv R1 : unsp ++ [Return]
                x   -> ur ++ unsp ++ [Return]
      chunkM ret

    trNode (I.Return n) = do
      unsp <- unspill activationRecordVariable (Just closureReg)
      chunkM $ unsp ++ [Return] 

    trNode (I.MkClos fn args var) = do
      uns <- mapM (\x -> unspill x Nothing) args >>= return . concat
      rs <- mapM reg args
      rt <- reg var
      chunkM $ uns ++ [MkClos fn rs rt]

    trNode _ = chunkM [Nop]

    notVoid :: Bool
    notVoid = notVoid' (varType n dict)

    notVoid' (TFun _ _ TUnit) = False
    notVoid' _ = True

    reloadArg :: (R, KId) -> TrM TOp
    reloadArg (r, n) = do
      spills <- gets rsSpill
      if M.member n spills
        then unspill n (Just r)
        else reg n >>= chunkMs . flip Move r

    newLabel :: TrM Label
    newLabel = lift $ lift $ freshLabel

    callOf :: forall e x. Insn e x -> HType -> TrM TOp

    -- got tail call
    callOf call@(I.Call l (I.Direct n True) args r) t@(TFun TFunLocal _ rt) = do
      alc <- asks (fromJust.M.lookup e.alloc.regalloc)
      spills <- gets rsSpill
      curr <- gets rsAlloc
      r0 <- unspill activationRecordVariable (Just R0)
      let regs = map (\n -> fromJust $ M.lookup n alc) as
      areload <- mapM reloadArg (zip regs args) >>= return . concat
      modify(\s -> s{rsMerge=skipOps})
      nl <- gets rsTLabel
      chunkM $ areload ++ [Branch nl]

--    callOf call@(I.Call l (I.Direct n False) args r) (TFun TFunLocal _ rt) = do
    callOf call@(I.Call l ct args r) (TFun TFunLocal _ rt) = do
      (spl, s) <- spillAlive l r
--      trace "SPILL ALIVE" $ trace (show spl) $ return ()
      uns <- mapM (\x -> unspill x Nothing) (callv ct args) >>= return . concat
      rs <- mapM reg args
      lbl <- newLabel
      callfun <- callF ct lbl rs
      call <- callRet rt r (chunkMs . callfun) >>= \x -> chunkM $ spl ++ uns ++ x
      unsp <- mapM (\(n,r) -> unspill n (Just r)) s >>= return . concat
      mapM_ delSpill (map fst s)
      retR <- reg r
--      trace "CALL OF" $ trace (show n) $ trace (show rt) $ return ()
      chunkM $ call ++ [Label lbl] ++ movRet rt retR ++ unsp ++ [Branch l]
      where callF (I.Direct n _)  lbl rs = return $ CallL lbl n rs
            callF (I.Closure n _) lbl rs = do
              r <- reg n
              return $ CallC lbl r rs

            callv (I.Direct n _)  as = as
            callv (I.Closure n _) as = n:as

    callOf (I.Call l (I.Direct n _) args r) (TFun (TFunForeign nm) _ rt) = do
      uns <- mapM (\x -> unspill x Nothing) args >>= return . concat
      rs <- mapM reg args
      callRet rt r (chunkMs . CallF l nm rs) >>= \x -> chunkM $ uns ++ x

    callOf (I.Call l (I.Closure n _) args r) (TFun TFunLocal atypes rt) = do
        chunkM [Nop]
--      error "Local closure call" -- FIXME ASAP

    callOf (I.Call l (I.Closure n _) args r) (TFun (TFunForeign _) _ rt) = do 
      error "Foreign closure call" -- FIXME ASAP

    callOf _ _ = do
      error "Unsupported call type"

    movRet :: HType -> R -> [Op]
    movRet TUnit r = []
    movRet _ r = [Move R1 r]

    callRet :: HType -> KId -> (RT -> TrM TOp) -> TrM TOp
    callRet TUnit v f = f RVoid
    callRet _     v f = reg v >>= f . RReg

    makeTail :: Bool -> forall e x . Insn e x -> Insn e x 
    makeTail True (I.Call l (I.Direct n False) as r) = I.Call l (I.Direct n True) as r
    makeTail _ x = x

    spillAtBlock :: Label -> TrM TOp
    spillAtBlock n = do
      (RegAllocation{spill=sp}) <- asks regalloc
      case M.lookup n sp of
        Nothing -> emptyM
        Just spills -> mapM spillOf (S.toList spills) >>= return.concat
--      where nonFree s (Spill ) = not $ S.member n s

    spillOf  :: (KId, R, Int) -> TrM TOp
    spillOf (n,r,s) = do
--      trace ("SPILL VAR " ++ show (n,r)) $ return ()
      i <- gets rsSlot
      spilled <- gets (M.member n . rsSpill)

      if spilled
        then emptyM
        else do let sp = Spill n r i
                succSlot
                modify(\s -> s{rsFree=r:rsFree s, rsSpill = M.insert n i (rsSpill s)})
                -- free vars must be spilled before closure call (outside the closure)
                if (not (S.member n fvs)) then chunkMs sp else emptyM

    spillReg :: TrM TOp
    spillReg = do
      st@(REnvSt{rsSpill=sp, rsAlloc=ra, rsSlot=n}) <- get
      let alloc = ra `M.difference` sp
      let (n,r) = head $ M.toList alloc -- FIXME
      spillOf (n,r,0)
    
    spillAlive :: Label -> KId -> TrM (TOp, [(KId, R)])
    spillAlive l n = do
      ra <- gets rsAlloc
      rs <- gets rsSpill
      rl <- asks rlive
      let lv = maybe [] (S.toList) (lookupFact l rl)
      let skip = M.singleton n Nothing
      let ls' = (ra `M.intersection` M.fromList (zip lv (repeat Nothing))) `M.difference` rs
      let ls  = ls' `M.difference` skip
      runWriterT $ liftM concat $
        mapM (\(n,r) -> tell [(n,r)] >> (lift $ spillOf (n,r,0))) $ M.toList ls

    delSpill :: KId -> TrM ()
    delSpill n = modify (\s -> s { rsSlot = rsSlot s - 1, rsSpill = M.delete n (rsSpill s)})

    unspill :: KId -> Maybe R -> TrM TOp
    unspill n (Just r) = do
      sp <- gets rsSpill >>= return . M.lookup n
      case sp of
        Nothing -> emptyM
        Just s  -> tmpReg n r >> chunkM [Unspill s r]

    unspill n Nothing = do
      sp <- gets rsSpill >>= return . M.lookup n
      unspill' sp n

    unspill' :: Maybe Int -> KId -> TrM TOp
    unspill' (Just s) n = do
      rf <- gets rsFree
      l <- gets rsLabel
--      trace ("UNSPILL " ++ show l) $ trace (show n) $ trace (show rf) $ return ()
      mt <- gets (null.rsFree)
      sp <- if mt then spillReg else emptyM
      rfree <- gets rsFree
      when (null rfree) $ error "COMPILER ERROR / UNABLE TO SPILL"
      let r = head rfree
      tmpReg n r
      modify (\s -> s{rsFree=tail rfree})
      chunkM $ sp ++ [Unspill s r]

    unspill' Nothing _ = emptyM

    fvs = S.fromList $ freevars fvn as

    succSlot :: TrM ()
    succSlot = do
      slot <- gets rsSlot >>= return . succ
      slotMx <- gets rsSlotMax
      modify (\s -> s{rsSlot=slot, rsSlotMax=max slot slotMx})

    tmpReg :: KId -> R -> TrM ()
    tmpReg n r = modify (\s -> s{rsAlloc = M.insert n r (rsAlloc s)})

    label :: Label -> TrM ()
    label l = do
      ra  <- asks (alloc.regalloc) >>= return . M.lookup l
      rf  <- asks (free.regalloc) >>= return . M.lookup l
      free' <- gets rsFree
      ma' <- gets rsAlloc
      let ma = maybe M.empty id ra
      let rsAlloc' = ma' `M.union` ma
--      trace ("LABEL " ++ show l) $ trace ("FREE REGS ") $ trace (show rf) $ trace "<<<" $
      modify (\st -> st { rsMerge = mergeOp
                        , rsLabel = l
                        , rsAlloc = rsAlloc'
                        , rsFree = (maybe free' id rf)
                        , rsAllocTrack = M.insert l rsAlloc' (rsAllocTrack st)
                        })

    reg :: KId -> TrM R
    reg n = do
      ra  <- gets rsAlloc 
--      trace "REG " $ 
--        trace (show ra) $
--          return ()
      case M.lookup n ra of
        Just r  -> return r
        Nothing -> error $ "INTERNAL COMPILER ERROR / NO REG ALLOCATED " ++ " " ++ (show n) -- FIXME

    liftVM :: forall e x. (Insn e x -> TrM TOp) -> Insn e x -> TrM TOp -> TrM TOp
    liftVM f n z = do
      merge <- gets rsMerge
      liftM2 merge z (f n)

    chunkM :: [Op] -> TrM TOp
    chunkM x = return x

    chunkMs :: Op -> TrM TOp
    chunkMs x = return [x]

    emptyM :: TrM TOp
    emptyM = return $ []

    initR :: REnv
    initR = REnv ra dict live

    initS :: Label -> REnvSt
    initS l = REnvSt e M.empty M.empty [] 0 0 mergeOp l M.empty 

    mergeOp :: TOp -> TOp -> TOp
    mergeOp x y = x ++ y

    skipOps :: TOp -> TOp -> TOp
    skipOps x y = x

    wipeSnots :: [Op] -> [Op]
    wipeSnots x = filter voidOp x
      where voidOp (Move r1 r2) | r1 == r2 = False
            voidOp _ = True

    mergeBlocks :: Label -> [[Op]] -> [Op]
    mergeBlocks e bs =
        let blk = catMaybes $ map wl bs
            (r, nr) = partition ret blk
            bro = sort $ foldl bra [] $ concatMap snd nr
            brefs = S.fromList $ e : [head i | i <- group bro, length i > 1]
            bmap  = M.fromList $ map (blockOf brefs) nr
            flat' = withBlocks bmap nr ++ concatMap revert r
            br = (S.fromList $ foldl bra [] flat') `S.union` S.singleton e
            flat = rmBranches $ filter (skipLabels br) flat'
        in flat
      where wl ((Label l):xs) = Just (l, xs)
            wl _                = Nothing
            ret (l,[]) = False
            ret (l,xs) = isret (last xs)
            isret (Return) = True
            isret _ = False
            bra acc (Branch l) = l:acc
            bra acc (BranchTrue _ l) = l:acc
            bra acc (BranchFalse _ l) = l:acc
            bra acc (CallC l _ _ _) = l:acc
            bra acc (CallL l _ _ _) = l:acc
            bra acc (CallF l _ _ _) = l:acc
            bra acc _ = acc
            revert (l,x) = (Label l) : x
            blockOf refs (l,xs) | l `S.member` refs = (l, Left xs)
            blockOf refs (l,xs) = (l, Right xs)
            skipLabels brs (Label l) = S.member l brs
            skipLabels _ _ = True


            rmBranches :: [Op] -> [Op]
            rmBranches ((Branch l1):x@(Label l2):xs) | l1 == l2 = x : rmBranches xs
            rmBranches (a:b:xs) = a : rmBranches (b:xs)
            rmBranches (a:[]) = a : []
            rmBranches [] = []

            withBlocks bmap bs =
              concat $ evalState (mapM merge bs) (C3 bmap)

            merge :: (Label, [Op]) -> C3M [Op]
            merge (l, xs) = do
              avail <- gets ( M.lookup l . blocks )
              r <- withMaybe avail (return []) $ anyBlock $ foldM (op l) []
              modify (\c3 -> c3{ blocks = M.delete l (blocks c3)})
              return $ (Label l) : r

            op :: Label -> [Op] -> Op -> C3M [Op]
            op l0 acc x@(Branch l) = do
              avail <- gets (M.lookup l . blocks)
              let def = return $ acc ++ [x]
              withMaybe avail def $ withRight def $ \b -> do
                ops <- merge (l,b)
                return $ acc ++ ops

            op l0 acc x = return $ acc ++ [x]

            anyBlock f (Left  bl) = f bl
            anyBlock f (Right bl) = f bl

            withRight df f (Right b) = f b
            withRight df f (Left _)  = df

            withMaybe (Just a) d f = f a
            withMaybe Nothing d f = d

    analyzeTailCall :: Label -> I.CallT -> HType -> Bool
    analyzeTailCall l (I.Direct fn _) (TFun TFunLocal _ _) | n == fn =
      let (GMany _ b _ ) = g
      in foldl (\a b -> foldBlockNodesF node b a) True (postorder_dfs_from b l)
      where
        node :: forall e x . Insn e x -> Bool -> Bool
        node (I.Label _)  a = a && True
        node (I.Branch _) a = a && True
        node (I.Assign v _) a | v == retvalVariable = a && True
        node (I.Return _) a = a && True
        node _ a = a && False
    
    analyzeTailCall _ _ _ = False

    -- debug
--    printBlock b = foldBlockNodesF (printNode) b (return ())
--    printNode :: forall e x . Insn e x -> I.M () -> I.M ()
--    printNode x@(I.Label l) s = printN x
--    printNode x s = s >> printN x
--    printN :: forall e x . Insn e x -> I.M ()
--    printN x = do
--        trace (printf "%-60s ;" (show x)) $ return ()


--isFreeVar :: KId -> Proc -> Bool
--isFreeVar n (Proc{args=as, freevarsnum=fvn}) = undefined 

freevars :: Int -> [KId] -> [KId]
freevars n as | n > 0  = drop (length as - n) as
              | otherwise = []

spillASAP :: TDict -> FactBase Live -> I.Proc -> S.Set KId 
spillASAP dict live (I.Proc{I.body=g, I.args=as, I.freevarsnum=fvn}) = ofProc $ foldGraphNodes node g S.empty
  where 
    node :: forall e x . Insn e x -> S.Set KId -> S.Set KId 
    node (I.Call l ct _ _ ) acc = varsOf l (varType (callvar ct) dict) `S.union` acc
    node x acc = acc

    varsOf :: Label -> HType -> S.Set KId
    varsOf l (TFun TFunLocal _ _) = maybe S.empty id $ lookupFact l live
    varsOf l _ = S.empty

    ofProc :: S.Set KId -> S.Set KId
--    ofProc asap = arv `S.union` fvs `S.union` (spills `S.intersection` asap)
    ofProc asap = spills `S.intersection` ( arv `S.union` fvs `S.union` asap )

    spills = S.fromList $ activationRecordVariable:as

    fvs = S.fromList $ freevars fvn as

    arv | fvn > 0 = S.singleton activationRecordVariable
        | otherwise = S.empty

varType :: KId -> TDict -> HType 
varType n rdict =
  let tp = M.lookup n rdict
  in if ((not.isJust) tp)
     then error $ "COMPILER ERROR NO TYPE FOR VAR " ++ n --- FIXME
     else fromJust tp

callvar :: I.CallT -> KId
callvar (I.Closure n _) = n
callvar (I.Direct n _)  = n

data REnv = REnv { regalloc :: RegAllocation
                 , rdict :: TDict
                 , rlive :: FactBase Live 
                 }

data REnvSt  = REnvSt { rsLabel :: Label
                      , rsAlloc :: M.Map KId R
                      , rsSpill :: M.Map KId Int
                      , rsFree  :: [R]
                      , rsSlot  :: Int
                      , rsSlotMax :: Int
                      , rsMerge :: TOp -> TOp -> TOp
                      , rsTLabel :: Label
                      , rsAllocTrack :: M.Map Label (M.Map KId R)
                      }

type TrM = StateT REnvSt (ReaderT REnv M)
type TOp = [Op]

data C3 = C3 { blocks :: M.Map Label (Either [Op] [Op]) }
type C3M = State C3 

