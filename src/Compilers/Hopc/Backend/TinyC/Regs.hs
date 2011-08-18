{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.Regs where

import Compilers.Hopc.Id
import Compilers.Hopc.Compile

import Compilers.Hopc.Frontend.Types
import Compilers.Hopc.Backend.TinyC.IR (Insn)
import qualified Compilers.Hopc.Backend.TinyC.IR as I
import Compilers.Hopc.Backend.TinyC.Live
import qualified Compilers.Hopc.Backend.TinyC.VM as V
import qualified Compilers.Hopc.Backend.TinyC.R as R
import Compilers.Hopc.Backend.TinyC.VM

import qualified Data.Set as S
import Data.Maybe
import Data.List
import Data.Either
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans as T
import Control.Monad

import Compiler.Hoopl

import Debug.Trace
import Text.Printf 

data Scan = Scan { s_seenby  :: M.Map Label (S.Set Label)
                 , s_rets    :: S.Set Label
                 , s_blocksL :: [(Label, Block Insn C C)]
                 , s_blockM  :: M.Map Label (Block Insn C C)
                 , s_graph   :: Graph Insn C C
                 }

data RegEnv  = RegEnv { label :: Label
                      , liveFacts :: FactBase Live 
                      , regAlloc :: M.Map KId R
                      , regFree :: S.Set R
                      , regSpilled :: M.Map KId (R, Slot)
                      , spillsFree :: S.Set Slot
                      , spill :: Int
                      , rdict :: TDict
                      , rgraph :: Graph Insn C C
                      }
type RegEnvM = StateT RegEnv I.M

initRegEnv :: [(KId, R)] -> Label -> FactBase Live -> TDict -> Graph Insn C C -> RegEnv
initRegEnv pre e lf tdict g = 
    let free = (S.fromList R.avail) `S.difference` S.fromList (map snd pre)
    in RegEnv e lf (M.fromList pre) free M.empty S.empty 0 tdict g

regenvLabel :: Label -> RegEnv -> RegEnv
regenvLabel l fb = fb{label = l}

regenvReg :: KId -> RegEnv -> Maybe R
regenvReg n re = M.lookup n (regAlloc re)

regenvAlloc :: KId -> RegEnv -> (Maybe R, RegEnv)
regenvAlloc n re = case (M.lookup n (regAlloc re)) of
    Just r  -> (Just r, re)
    Nothing -> withFree re $ let (r, free) = S.deleteFindMin (regFree re)
                                 ra = M.insert n r (regAlloc re)
                             in (Just r, re {regAlloc = ra, regFree = free })
    where withFree re _ | S.null (regFree re) = (Nothing, re)
          withFree re v = v

regenvSpill :: KId -> RegEnv -> (R, Slot, RegEnv)

regenvSpill s re@(RegEnv{regSpilled=rs}) | s `M.member` rs =
  let (r,sl) = fromJust $ M.lookup s rs in (r, sl, re)

regenvSpill s re@(RegEnv{regAlloc=ra}) =
    withReg ra $ let ((n,r), ra') = M.deleteFindMax ra
                     ra'' = M.insert s r ra'
                 in withSpills n r (re{regAlloc=ra''})

    where withReg ra f | M.null ra = error "COMPILER ERROR --- NO REG FOR SPILLS IS AVAIL --- PROBABLY THE COMPILER BUG" --- FIXME
          withReg ra f = f

withSpills n r re | S.null (spillsFree re) =
  let spn = (spill re)
      spl = M.insert n (r, spn) (regSpilled re)
  in (r, spn, re{regSpilled = spl, spill = spn + 1})

withSpills n r re =
  let (spn, sf) = S.deleteFindMin (spillsFree re)
  in (r, spn, re{regSpilled = M.insert n (r, spn) (regSpilled re), spillsFree = sf})

regenvSpill' :: KId -> RegEnv -> Maybe (R, Slot, RegEnv)
regenvSpill' n re@(RegEnv{regAlloc=ra}) = withR (M.lookup n ra) $ \r ->
  let rv = withSpills n r re
  in Just rv
  where withR Nothing  f = Nothing
        withR (Just r) f = f r 

regenvUnspill' :: KId -> RegEnv -> RegEnv
regenvUnspill' v = snd . regenvUnspill v

regenvUnspill :: KId -> RegEnv -> (R, RegEnv)
regenvUnspill v re@(RegEnv{regSpilled=rs}) = withSpill v re (M.lookup v rs)
    where withSpill n re Nothing = error "COMPILER ERROR --- NO SPILL FOUND" -- FIXME
          withSpill n re@(RegEnv{regSpilled=rs, spillsFree=sf}) (Just (r,sn)) =
            (r, re{regSpilled = M.delete n rs, spillsFree = S.insert sn sf })

regenvFree :: S.Set KId -> RegEnv -> RegEnv
regenvFree ls re@(RegEnv{regAlloc=ra, regFree=rf, regSpilled=rs, spillsFree = sf}) =
    let ra' = M.filterWithKey (\k _ -> (S.member k ls)) ra
        rf' = S.fromList (R.avail :: [R]) `S.difference` S.fromList (M.elems ra')
        rs' = M.filterWithKey (\k _ -> (S.member k ls)) rs
        sf'  = S.fromList $ map snd $ M.elems $ M.filterWithKey (\k _ -> not (S.member k ls)) rs
    in re {regAlloc = ra', regFree = rf', regSpilled = rs', spillsFree = sf `S.union` sf' }

regenvUpdateFact :: Label -> (Live -> Live) -> RegEnv -> RegEnv
regenvUpdateFact l fn re@(RegEnv{liveFacts = lf}) =
    withFact re (lookupFact l lf) $ \f0 -> re{liveFacts = mapInsert l (fn f0) lf}
    where withFact _ (Just f) fn = fn f
          withFact re Nothing fn  = re

regenvLiveRegs :: Label -> RegEnv -> [(KId, R)]
regenvLiveRegs l  re@(RegEnv{regAlloc=ra, liveFacts=lf}) =
  let rn = M.keysSet
      lives = maybe S.empty id (lookupFact l lf)
  in M.toList $ M.filterWithKey (\k _ -> k `S.member` lives && k /= retvalVariable) ra

data C3 = C3 { blocks :: M.Map Label (Either [V.Op] [V.Op]) }
type C3M = State C3 

allocateAndFlatten :: TDict -> FactBase Live -> I.Proc -> I.M V.Proc 
allocateAndFlatten dict live p@(I.Proc {I.entry = e, I.body = g, I.name = n, I.args = as}) = do

    let (GMany _ bbb _) = g

--    execStateT (mapM_ (printBlock (return ()) live) (postorder_dfs_from bbb e)) e

    let pre = zip (activationRecordVariable:retvalVariable:as) (R.allRegs :: [R])

    (ops, rnv) <- runStateT (mapM transBlock (postorder_dfs_from bbb e))
                            (initRegEnv pre e live dict g)

--    trace "JOPA KITA PECHEN TRESKI" $!
--        forM_ ops $! \op -> do
--            trace (intercalate "\n" $ map show op) $ return ()

    return $ V.Proc {V.name = n, V.arity = length as, V.slotnum = (spill rnv), V.body = mergeBlocks ops}

    where   printBlock init l b = foldBlockNodesF (printNode live) b init 

            printNode :: forall e x . FactBase Live -> Insn e x -> StateT Label I.M () -> StateT Label I.M ()
            printNode fb x@(I.Label l) s = put l >> s >> printN fb x
            printNode fb x s = s >> printN fb x 
 
            printN :: forall e x . FactBase Live -> Insn e x -> StateT Label I.M ()
            printN fb x = do
                l <- get
                trace (printf "%-60s ; %s" (show x) (show $ (S.toList.fromJust) (lookupFact l fb) ) ) $ return ()

            transBlock b = foldBlockNodesF (liftRM transNode) b (return [])

            transNode :: forall e x . Insn e x -> RegEnvM [V.Op]
            transNode b@(I.Label l)    = enterBlock b l >> return (V.Label l) >>= list1M
            transNode (I.Return _)   = list1M $ V.Return
 
            transNode (I.Const c n)  = allocate n $ list1M . V.Const c
 
            transNode (I.Assign to from) = withRegs [from] $ do
                fromR <- reg from
                allocate to $ list1M . V.Move fromR
 
            transNode x@(I.Call _ ct rs _) = varType (callvar ct) >>= withRegs rs . callOf x

            transNode (I.Branch l)   = list1M $ V.Branch l
 
            transNode (I.Cond n l1 l2) = withRegs [n] $ do
                mergeFacts l1 l2
                r <- reg n
                return $ V.BranchTrue r l1 : V.Branch l2 : []

            transNode (I.MkClos _ _ _ ) = error "CLOSURES ARE NOT SUPPORTED YET" -- FIXME ASAP!!!

            callOf :: forall e x. Insn e x -> HType -> RegEnvM [V.Op]
            callOf (I.Call l (I.Direct n) args r) (TFun TFunLocal _ rt) = do
                rs <- mapM reg args
                
                ra <- gets regAlloc
                lr <- gets (regenvLiveRegs l)
--                trace ("\n\nSPILL BEFORE CALL " ++ show ra ++ " " ++ show lr) $ return ()

--                call <- callRet rt r (list1M . V.CallL l n rs)
                call <- callRet rt r (spillAlive l . list1 . V.CallL l n rs)
                return call
--                re <- get
--                ra <- gets regAlloc
--                lr <- gets (regenvLiveRegs l)
--                let restore = [x|(x, _) <- lr, x /= activationRecordVariable]

--                lf <- gets liveFacts
--                rs <- gets regSpilled
--                sf <- gets spillsFree
--                sn <- gets spill

--                trace ("SPILLED " ++ show rs) $ return ()
--                trace ("SPILLED " ++ show rs) $ return ()
--                trace ("SPILLS " ++ show sn ++ " " ++ show sf) $ return ()

--                let live = maybe S.empty id (lookupFact l lf)
--                modify (regenvFree live)

--                unspill <- withRegs restore (return [])

--                rs <- gets regSpilled
--                sf <- gets spillsFree
--                sn <- gets spill

--                trace ("\nAFTER CALL: " ++ n ++ " " ++ show l ++ " "  ++ show ra ++ " " ++ show lr ) $ return ()
--                trace ("SPILLED " ++ show rs) $ return ()
--                trace ("SPILLED " ++ show rs) $ return ()
--                trace ("LIVE" ++ show lr) $ return ()
--                return $ call ++ unspill

            callOf (I.Call l (I.Direct n) args r) (TFun (TFunForeign nm) _ rt) = do
                rs <- mapM reg args
                callRet rt r (list1M . V.CallF l nm rs)

            callOf (I.Call l (I.Closure n) args _) (TFun TFunLocal _ rt) =
                error "CALLING CLOSURE  --- NOT SUPPORTED YET" -- FIXME
            --    list1M $ V.CallC R2 (map (const R2) args)

            callOf (I.Call l (I.Closure n) args _) (TFun (TFunForeign _) _ _) =
                error "CALLING CLOSURE OF FOREIGN FUNCTION --- NOT SUPPORTED YET" -- FIXME
            --    list1M $ V.CallL R2 (map (const R2) args)

            callOf x _ = error $ "CAN'T CALL " ++ show x -- FIXME

            callRet :: HType -> KId -> (RT -> RegEnvM [V.Op]) -> RegEnvM [V.Op]
            callRet TUnit v f = f RVoid
            callRet _     v f = allocate v $ f . RReg

            spillAlive :: Label -> [V.Op] -> RegEnvM [V.Op]
            spillAlive l v = do
              liveR  <- gets (regenvLiveRegs l)
              spills <- gets regSpilled >>= return . M.map fst

              ra <- gets regAlloc

              let toSpill = (M.fromList liveR) `M.difference` spills

--              trace ("SPILL ALIVE " ++ show l ++ (show liveR)) $ return ()
--              trace ("SPILLED " ++ show spills) $ return ()
--              trace ("TO SPILL: " ++ show toSpill) $ return ()
--              trace ("RA: " ++ show ra) $ return ()
--              trace ("\n") $ return ()
              spills <- forM (M.toList toSpill) $ \(n, r) -> do
--                          trace ("SPILL VAR " ++ n ++ " " ++ show r) $ return ()
                          re <- get
                          withM (regenvSpill' n re) $ \(r,sl,re') -> do
                            put re'
                            list1M $ V.Spill r sl
                  
              return $ concat spills ++ v
              where withM :: Maybe (R, Slot, RegEnv) -> ((R, Slot, RegEnv) -> RegEnvM [V.Op]) -> RegEnvM [V.Op]
                    withM Nothing f = return []
                    withM (Just v) f = f v 

            enterBlock :: forall e x . Insn e x -> Label -> RegEnvM ()
            enterBlock b l = do
                modify $ regenvLabel l
                lf <- gets liveFacts
                ra <- gets regAlloc
                rf <- gets regFree
                sp <- gets regSpilled
                sn <- gets spill
 
                let live = maybe S.empty id (lookupFact l lf)
                let dead = M.keysSet ra `S.difference` live
 
                modify (regenvFree live)

--                trace ("\n\nTRACE: ENTER BLOCK " ++ show l) $ return ()
--                trace ("LIVES " ++ show live) $ return ()
--                trace ("ALLOCATED: " ++ show (M.toList ra)) $ return ()
--                trace ("FREE: " ++ show (S.toList rf)) $ return ()
--                trace ("SPILL: " ++ show sn) $ return ()
--                trace ("SPILLED: " ++ show sp) $ return ()
--                trace ("DEAD VALUES: " ++ show (S.toList dead) ) $ return ()


            -- TODO: check for spills
            -- if reg spilled  --- check for free reg
            --    if free reg -> unspill to free reg, return reg
            --    if no free reg --> spill a reg, unspill to the reg, return reg
            -- free vars also remove spills
            withRegs :: [KId] -> RegEnvM [V.Op] -> RegEnvM [V.Op]
            withRegs rs m = do
--                trace ("TRACE: withRegs " ++ show rs) $ return  ()
                spills <- gets regSpilled
                unspill <- forM (filter (flip M.member spills) rs) $ \n -> do
                            let (r, sn) = fromJust $ M.lookup n spills
                            modify (regenvUnspill' n)
                            allocate n $ list1M . V.Unspill sn
                liftM2 (++) ((return.concat) unspill) m

            reg :: KId -> RegEnvM R
            reg n = do
                r <- gets (regenvReg n)
                maybe (error $ "ERROR: NO REG FOR " ++ n) (return.id) r -- FIXME

            allocate :: KId -> (R -> RegEnvM [V.Op]) -> RegEnvM [V.Op]
            allocate n f = do
                ra <- gets regAlloc -- FIXME DEBUG
                rf <- gets regFree  -- FIXME DEBUG
                l  <- gets label
--                sucs <- successors l
                (mr, re) <- gets (regenvAlloc n)
--                trace ("REG. ALLOC " ++ n ++ "\nRA " ++ show (M.toList ra) ++ "\nFREE " ++ show (S.toList rf)) $ return ()
                withReg n (mr, re)

                where withReg :: KId -> (Maybe R, RegEnv) -> RegEnvM [V.Op]
                      withReg var ((Just r), re) = put re >> f r
                      withReg var (Nothing, re)  = spillR var f
  
            spillR var f = do
              (r,n,re) <- gets (regenvSpill var)
              trace ("SPILLR " ++ show (var,r,n)) $ return ()
              put re
              liftM2 (++) (list1M $ V.Spill r n) (f r)

            fact l = gets liveFacts >>= return . lookupFact l
                                    >>= return . maybe S.empty id

            mergeFacts :: Label -> Label -> RegEnvM ()
            mergeFacts l1 l2 = do
                l  <- gets label
                fct <- liftM2 (S.union) (fact l1) (fact l2)
                ra' <- gets regAlloc
                ra <- gets regAlloc >>= return . M.keysSet
                                    >>= return . S.intersection fct

                (GMany _ b _) <- gets rgraph
                let ls1 = S.fromList $ collectLabels l1 b
                let ls2 = S.fromList $ collectLabels l2 b
                let lsi = ls1 `S.intersection` ls2
                let ll = ls2 `S.difference` lsi
                let lr = ls1 `S.difference` lsi

                fl <- fact l1 
                fr <- fact l2

                forM_ (S.toList $ ll) $ \l ->
                    modify (regenvUpdateFact l (S.union fl))

                forM_ (S.toList $ lr) $ \l ->
                    modify (regenvUpdateFact l (S.union fr))

--                forM_ (S.toList $ ll `S.union` lr) $ \l ->
--                    modify (regenvUpdateFact l (S.union fact))
 
--                trace ("MERGE FACTS: " ++ show l1 ++ " " ++ show l2 ++ " " ++ (show fact)) $ return ()
--                trace ("LIVE REGS: " ++ show ra) $ return ()
--                trace ("REGS: " ++ show ra') $ return ()
--                trace ("SUCC L1: " ++ show ll) $ return ()
--                trace ("SUCC L2: " ++ show lr) $ return ()
--                trace ("SUCC L2: " ++ show sucL) $ return ()
--                trace ("SUCC L1: " ++ show sucR) $ return ()

                where 
                      collectLabels l b  = 
                        concatMap (\b -> foldBlockNodesF collectLabel b [])
                                  (postorder_dfs_from b l)

                      collectLabel :: forall e x. Insn e x -> [Label] -> [Label]
                      collectLabel (I.Label n) acc = n:acc
                      collectLabel _ acc = acc

--                let factF = S.union fact
--                modify (regenvUpdateFact l1 factF . regenvUpdateFact l2 factF)

            varType :: KId -> RegEnvM HType 
            varType n = do
                tp <- gets (M.lookup n . rdict)
                when ((not.isJust) tp) $ error $ "COMPILER ERROR NO TYPE FOR VAR " ++ n --- FIXME
                return $ fromJust tp

            liftRM :: forall e x y . (Insn e x -> RegEnvM [y]) -> Insn e x -> RegEnvM [y] -> RegEnvM [y]
            liftRM f n z = liftM2 (++) z (f n)

            mergeBlocks :: [[V.Op]] -> [V.Op]
            mergeBlocks bs =
                let blk = catMaybes $ map wl bs
                    (r, nr) = partition ret blk
                    bro = sort $ foldl bra [] $ concatMap snd nr
                    brefs = S.fromList [head i | i <- group bro, length i > 1]
                    bmap  = M.fromList $ map (blockOf brefs) nr
                    flat' = withBlocks bmap nr ++ concatMap revert r
                    br = S.fromList $ foldl bra [] flat' 
                    flat = rmBranches $ filter (skipLabels br) flat'

                in flat 
              where wl ((V.Label l):xs) = Just (l, xs)
                    wl _                = Nothing
                    ret (l,[]) = False
                    ret (l,xs) = isret (last xs)
                    isret (V.Return) = True
                    isret _ = False
                    bra acc (V.Branch l) = l:acc
                    bra acc (V.BranchTrue _ l) = l:acc
                    bra acc (V.BranchFalse _ l) = l:acc
                    bra acc (V.CallC l _ _ _) = l:acc
                    bra acc (V.CallL l _ _ _) = l:acc
                    bra acc (V.CallF l _ _ _) = l:acc
                    bra acc _ = acc
                    revert (l,x) = (V.Label l) : x
                    blockOf refs (l,xs) | l `S.member` refs = (l, Left xs)
                    blockOf refs (l,xs) = (l, Right xs)
                    skipLabels brs (V.Label l) = S.member l brs
                    skipLabels _ _ = True


                    rmBranches :: [V.Op] -> [V.Op]
                    rmBranches ((V.Branch l1):x@(V.Label l2):xs) | l1 == l2 = x : rmBranches xs
                    rmBranches (a:b:xs) = a : rmBranches (b:xs)
                    rmBranches (a:[]) = a : []
                    rmBranches [] = []

                    withBlocks bmap bs =
                      concat $ evalState (mapM merge bs) (C3 bmap)

                    merge :: (Label, [V.Op]) -> C3M [V.Op]
                    merge (l, xs) = do
                      avail <- gets ( M.lookup l . blocks )
                      r <- withMaybe avail (return []) $ anyBlock $ foldM (op l) []
                      modify (\c3 -> c3{ blocks = M.delete l (blocks c3)})
                      return $ (V.Label l) : r

                    op :: Label -> [V.Op] -> V.Op -> C3M [V.Op]
                    op l0 acc x@(V.Branch l) = do
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

list1M x = return $ x : []
list1 x = x : []

callvar (I.Closure n) = n
callvar (I.Direct n) = n

--withMaybeM :: (Monad m) => Maybe a -> b -> (a -> m b) -> m b
--withMaybeM (Just 



