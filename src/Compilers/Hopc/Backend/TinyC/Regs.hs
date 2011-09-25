{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.Regs (RegAllocation(..), allocateLinearScan) where

import Compilers.Hopc.Id
import Compilers.Hopc.Compile

import Compilers.Hopc.Frontend.Types
import Compilers.Hopc.Backend.TinyC.IR (Insn)
import qualified Compilers.Hopc.Backend.TinyC.IR as I
import Compilers.Hopc.Backend.TinyC.Live
import qualified Compilers.Hopc.Backend.TinyC.VM as V
import qualified Compilers.Hopc.Backend.TinyC.R as R
import Compilers.Hopc.Backend.TinyC.VM

import qualified Data.Vector.Fusion.Stream as F 

import qualified Data.Set as S
import Data.Maybe
import Data.List
import Data.Either
import Data.Monoid
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.Trans
import Control.Monad.Trans as T
import Control.Monad

import Compiler.Hoopl

import Debug.Trace
import Text.Printf 

allocateLinearScan :: TDict -> FactBase Live -> S.Set KId -> I.Proc -> I.M RegAllocation
allocateLinearScan dict live asap p@(I.Proc {I.entry = e, I.body = g, I.name = n, I.args = as}) = do
    let (GMany _ bbb _) = g
    let args = activationRecordVariable:as
    let prep = M.fromList $ zip args (repeat e)
    let asap = M.keysSet prep 
    let blocks = postorder_dfs_from bbb e
    let labels  = concatMap (\b -> foldBlockNodesF labelOf b []) blocks
    let labelsOrd = M.fromList $ zip labels [0..] :: M.Map Label Int
    let labelCmp = \a b -> compare (labelKey a labelsOrd) (labelKey b labelsOrd)
    let starts  = snd $ foldl (\v b -> foldBlockNodesF useOf b v) (e, prep) blocks
    let ends    = foldl factIns M.empty (reverse labels)
    let rest = starts `M.difference` ends
--    trace "WTF:" $ trace (show wtf) $ return () 
    let intervals' = M.intersectionWith (\a b -> (a, b)) starts (ends `M.union` rest)
    let intervals  = sortBy (intervalCmpS labelCmp) $ M.toList intervals'
    let preintervals = catMaybes $ map (\n -> maybe Nothing (Just . ((,) n)) (lookup n intervals)) args

--    trace "\n-- INTERVALS --" $ trace (show starts)  $ trace (show ends) $ return ()

    (RegAllocSt{ regAlloc=ra
               , location=nl
               , locations=ls 
               , regTrack=t 
               , spillTrack=s
               , freeTrack=f}) <- do
      flip execStateT (initRegAlloc labelCmp) $ do
        preallocate (zip preintervals R.allRegs)
        forM_ intervals $ \i -> do
            expireOldIntervals i
            trackFree i
            an <- activeN
            if an >= regsN
              then spillAtInterval i
              else allocate i >> addActive i
            spillASAP i

--    trace "-- LABELS --" $
--      trace ( intercalate "\n" $ map show $ M.toList labelsOrd ) $ do
--        return ()

--    trace "\n-- REGISTER ALLOCATION --" $
--      trace (intercalate "\n" $ map show $ M.toList t) $
--        trace (intercalate "\n" $ map show $ M.toList s) $
--          trace "PRE-INTERVALS" $
--            trace (intercalate "\n" $ map show preintervals) $
--              return ()
--          trace (intercalate "\n" $ map show $ M.toList f) $

    return $ RegAllocation t s f nl 

    where
      initRegAlloc f = RegAllocSt f [] R.allRegs M.empty 0 M.empty M.empty M.empty M.empty

      activeN :: StateT RegAllocSt I.M Int
      activeN = gets actives >>= return . length

      regsN = (length $ (R.allRegs :: [R]))

      addActive :: Interval -> StateT RegAllocSt I.M ()
      addActive i =
        modify (\st -> st {actives = sortBy (intervalCmpE (lcmp st)) (i:(actives st))})

      preallocate :: [(Interval, R)] -> StateT RegAllocSt I.M ()
      preallocate a = mapM_ (allocate.fst) a

      allocate :: Interval -> StateT RegAllocSt I.M ()
      allocate i@(n,_) = do
        already <- gets (M.member i . regAlloc)
        when (not already) $ do
          st@(RegAllocSt{regPool=(r:rs), regAlloc = ra}) <- get -- FIXME ?
          put st {regPool=rs, regAlloc=M.insert i r ra}
          trackReg i r

      deallocate :: Interval -> StateT RegAllocSt I.M ()
      deallocate i = do
        st@(RegAllocSt{regPool=rp, regAlloc = ra}) <- get
        when (M.member i ra) $ do
            let r = fromJust $ M.lookup i ra
            put st {regPool=rp++[r], regAlloc=M.delete i ra}

      delActive :: Interval -> StateT RegAllocSt I.M ()
      delActive i = do
        st@(RegAllocSt{actives=a}) <- get
        let a' = delete i a
        put st{actives=a'}

      updateReg :: Interval -> Maybe R -> StateT RegAllocSt I.M ()
      updateReg i (Just r) = do
        modify (\st -> st{ regAlloc = M.insert i r (regAlloc st)})
        trackReg i r
--        trace ("REG. ALLOC " ++ (show (fst i)) ++ " " ++ show r) $ return ()

      updateReg _ Nothing = return ()

      trackFree :: Interval -> StateT RegAllocSt I.M ()
      trackFree (_, (l1, _)) = do
        r <- gets regPool
        modify (\s -> s {freeTrack = M.insert l1 r (freeTrack s)})

      trackReg :: Interval -> R -> StateT RegAllocSt I.M ()
      trackReg (n, (l1, _)) r = do
        m1 <- gets (M.lookup l1 . regTrack) >>= return . maybe (M.empty) id
        let m2 = M.insert n r m1
        r <- gets regPool
--        trace ("TRACK REG / FREE POOL " ++ show l1) $ trace ( show r ) $ return ()
        modify (\s -> s {regTrack = M.insert l1 m2 (regTrack s), freeTrack = M.insert l1 r (freeTrack s)})

      trackSpill :: Interval -> Interval -> R -> StateT RegAllocSt I.M ()
      trackSpill (n, (i,_)) spill@(k, _) r = do
        st@(RegAllocSt{spillTrack=mt, locations=ls}) <- get
        let l = fromJust $ M.lookup spill ls
        let s' = maybe (S.singleton (k,r,l)) (\s'' -> S.insert (k,r,l) s'') (M.lookup i mt)
        put st {spillTrack = M.insert i s' mt}

      spillASAP :: Interval -> StateT RegAllocSt I.M ()
      spillASAP i@(n, (l1, _)) = do
        when (S.member n asap) $ do
          allocated <- gets (M.member i . regAlloc)
          when allocated $ do
--            trace ("SPILL-ASAP! " ++ show i) $ return ()
            r <- gets (M.lookup i . regAlloc)
            newLocation i
            delActive i
            deallocate i
            trackSpill i i (fromJust r)

      spillAtInterval :: Interval -> StateT RegAllocSt I.M ()
      spillAtInterval i = do
        cmp <- gets lcmp
        spill <- gets (last.actives)
        a <- gets actives
--        trace ("ACTIVE : " ++ show a) $
--          trace ("SPILL : " ++ show i ++ " " ++ show spill ++ " " ++ (show $ cmp (endp spill) (endp i))) $ do

--        when (cmp (endp spill) (endp i) == GT) $ do
        reg <- gets (M.lookup spill . regAlloc)
        updateReg i reg
        newLocation spill
        delActive spill
        addActive i
        when (isJust reg) $
          trackSpill i spill (fromJust reg)

--        if cmp (endp spill) (endp i) == GT
--          then do
--            reg <- gets (M.lookup spill . regAlloc)
--            updateReg i reg
--            newLocation spill
--            delActive spill
--            addActive i
--          else do
--            newLocation i 
 
      newLocation i = do
        st@(RegAllocSt{location=n, locations=ls}) <- get
        put st {location=(n+1), locations = M.insert i n ls}

      expireOldIntervals :: Interval -> StateT RegAllocSt I.M ()
      expireOldIntervals i = do
        st@(RegAllocSt{lcmp=cmp, actives=a, regAlloc=ra, regPool=rp}) <- get
        let (expired, rest) = partition (\j -> cmp (endp j) (startp i) == LT) (sortBy (intervalCmpE cmp) a)
        let regs = catMaybes $ map (flip M.lookup ra) expired
        let ra' = ra `M.difference` M.fromList (zip expired (repeat Nothing))
        put st {actives = rest, regAlloc = ra', regPool = rp ++ regs}

      startp (_, (l, _)) = l
      endp  (_, (_, l)) = l

      intervalCmpS f (n1, (l1, l2)) (n2, (ll1, ll2)) = f l1 ll1
      intervalCmpE f (n1, (l1, l2)) (n2, (ll1, ll2)) = f l2 ll2

      labelKey :: Label -> M.Map Label Int -> Int
      labelKey a v = maybe (-1) id (M.lookup a v)

      labelOf :: forall e x . Insn e x -> [Label] -> [Label]
      labelOf (I.Label n) l = l ++ [n]
      labelOf _ l = l

      useOf :: forall e x . Insn e x
            -> (Label, M.Map KId Label)
            -> (Label, M.Map KId Label)

      useOf (I.Assign n n1) (l, v) = (l, factIns' n l v `M.union` factIns' n1 l v)
      useOf (I.Const _ n) (l, v)  = (l, factIns' n l v)
      useOf (I.Call _ (I.Closure n1 _) _ n) (l, v) = (l, factIns' n l v `M.union` factIns' n1 l v) -- FIXME: also count the args
      useOf (I.Call _ _ _ n) (l, v) = (l, factIns' n l v) -- FIXME: also count the args ?
      useOf (I.Label n) (_, v) = (n, v)
      useOf _ x = x

      factIns' :: KId -> Label -> M.Map KId Label -> M.Map KId Label
      factIns' n l v = if isUnit n || M.member n v then v else M.insert n l v

      factIns v l =
        let fs' = maybe [] (S.toList) (lookupFact l live)
            fs  = zip (filter (not.isUnit) (filter (not.(flip M.member v)) fs')) (repeat l)
        in v `M.union` M.fromList fs

      isUnit :: KId -> Bool
--      isUnit n = maybe False (== TUnit) $ M.lookup n dict
      isUnit n = False 

data RegAllocSt = RegAllocSt { lcmp :: Label -> Label -> Ordering
                             , actives :: [Interval]
                             , regPool :: [R]
                             , regAlloc :: M.Map Interval R
                             , location :: Int
                             , locations :: M.Map Interval Int
                             , regTrack :: M.Map Label (M.Map KId R)
                             , spillTrack :: M.Map Label (S.Set (KId, R, Int))
                             , freeTrack :: M.Map Label [R]
                             }

type Interval = (KId, (Label, Label))

