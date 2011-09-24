{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.VM (
    module Compilers.Hopc.Backend.TinyC.VM.Types
  , fromIR
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

fromIR :: TDict -> FactBase Live -> RegAllocation -> I.Proc -> I.M Proc

fromIR dict live ra p@(I.Proc {I.entry = e, I.body = g, I.name = n, I.args = as}) = do
  let (GMany _ bbb _) = g
  let blocks = postorder_dfs_from bbb e

--  mapM_ printBlock blocks
--  trace "---" $ return ()

  (vm, st) <- flip runReaderT initR $ flip runStateT initS $
--          liftM (mergeBlocks e) $
                liftM concat $ mapM (\b -> foldBlockNodesF (liftVM trNode) b emptyM) blocks

  return $ Proc {name = n, arity = length as, slotnum = (rsSlotMax st), body = vm}

  where

    trNode :: forall e x. Insn e x -> TrM TOp

    trNode (I.Label n) = do
      label n
      spill <- spillAtBlock n
      chunkM $ Label n : spill

    trNode x@(I.Call _ ct _ _) = varType (callvar ct) >>= callOf x
 
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

    trNode (I.Return n) = do
      ur  <- unspill n (Just R1)
      unsp <- unspill activationRecordVariable (Just closureReg)
      rv <- reg n
      let ret = case ur of
                []  -> unsp ++ [Move rv R1, Return]
                x   -> ur ++ unsp ++ [Return]
      chunkM ret

    trNode (I.MkClos _ _ _) = error "CLOSURES ARE NOT SUPPORTED YET" -- FIXME

    trNode _ = chunkM [Nop]

    callOf :: forall e x. Insn e x -> HType -> TrM TOp
 
    callOf (I.Call l (I.Direct n) args r) (TFun TFunLocal _ rt) = do
      (spl, s) <- spillAlive l r
--      trace "SPILL ALIVE" $ trace (show spl) $ return () 
      uns <- mapM (\x -> unspill x Nothing) args >>= return . concat
      rs <- mapM reg args
      call <- callRet rt r (chunkMs . CallL l n rs) >>= \x -> chunkM $ spl ++ uns ++ x
      unsp <- mapM (\(n,r) -> unspill n (Just r)) s >>= return . concat
      mapM_ delSpill (map fst s)
      chunkM $ call ++ unsp

    callOf (I.Call l (I.Direct n) args r) (TFun (TFunForeign nm) _ rt) = do
      uns <- mapM (\x -> unspill x Nothing) args >>= return . concat
      rs <- mapM reg args
      callRet rt r (chunkMs . CallF l nm rs) >>= \x -> chunkM $ uns ++ x

    callOf _ _ = error "Unsupported call type" -- FIXME ASAP

    callRet :: HType -> KId -> (RT -> TrM TOp) -> TrM TOp
    callRet TUnit v f = f RVoid
    callRet _     v f = reg v >>= f . RReg

    spillAtBlock :: Label -> TrM TOp
    spillAtBlock n = do
      (RegAllocation{spill=sp}) <- asks regalloc
      case M.lookup n sp of
        Nothing -> emptyM
        Just spills -> mapM spillOf (S.toList spills) >>= return.concat

    spillOf  :: (KId, R, Int) -> TrM TOp
    spillOf (n,r,s) = do
--      trace ("SPILL VAR " ++ show (n,r)) $ return ()
      i <- gets rsSlot
      spilled <- gets (M.member n . rsSpill)
      if spilled 
        then emptyM
        else do let sp = Spill r i
                succSlot
                modify(\s -> s{rsFree=r:rsFree s, rsSpill = M.insert n i (rsSpill s)})
                chunkMs sp


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

    spillFrom :: Op -> Maybe (R, Int)
    spillFrom (Spill r n) = Just (r, n)
    spillFrom _ = Nothing

    delSpill :: KId -> TrM ()
    delSpill n = modify (\s -> s { rsSlot = rsSlot s - 1, rsSpill = M.delete n (rsSpill s)})

--    killSpill :: Int -> R -> TrM TOp
--    killSpill n r = do
--      modify (\s -> s{rsSpill = M.delete n }
--      chunkM [Unspill n r]
--      sp <- gets rsSpill >>= return . M.lookup n
--      case sp of
--        Nothing -> emptyM
--        Just s  -> chunkM [Unspill   r]

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
      ma' <- gets rsAlloc
      let ma = maybe M.empty id ra
--      trace ("LABEL " ++ show l) $ trace ("FREE REGS ") $ trace (show rf) $ trace "<<<" $
      modify (\st -> st {rsLabel = l, rsAlloc = ma' `M.union` ma, rsFree = (maybe [] id rf)})

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
    liftVM f n z = liftM2 (++) z (f n)

    chunkM :: [Op] -> TrM TOp
    chunkM x = return x

    chunkMs :: Op -> TrM TOp
    chunkMs x = return [x]

    emptyM :: TrM TOp
    emptyM = return $ []

    varType :: KId -> TrM HType 
    varType n = do
      tp <- asks (M.lookup n . rdict)
      when ((not.isJust) tp) $ error $ "COMPILER ERROR NO TYPE FOR VAR " ++ n --- FIXME
      return $ fromJust tp

    callvar (I.Closure n) = n
    callvar (I.Direct n) = n

    initR :: REnv
    initR = REnv ra dict live

    initS :: REnvSt
    initS = REnvSt e M.empty M.empty [] 0 0

    mergeBlocks :: Label -> [[Op]] -> [Op]
    mergeBlocks e bs =
        let blk = catMaybes $ map wl bs
            (r, nr) = partition ret blk
            bro = sort $ foldl bra [] $ concatMap snd nr
            brefs = S.fromList [head i | i <- group bro, length i > 1]
            bmap  = M.fromList $ map (blockOf brefs) nr
            flat' = withBlocks bmap nr ++ concatMap revert r
            br = S.fromList $ foldl bra [] flat' 
            flat = rmBranches $ filter (skipLabels br) flat'
--                    flat = flat' 

        in Label e : flat
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



    -- debug
--    printBlock b = foldBlockNodesF (printNode) b (return ())
--    printNode :: forall e x . Insn e x -> I.M () -> I.M ()
--    printNode x@(I.Label l) s = printN x
--    printNode x s = s >> printN x
--    printN :: forall e x . Insn e x -> I.M ()
--    printN x = do
--        trace (printf "%-60s ;" (show x)) $ return ()

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
                      }

type TrM = StateT REnvSt (ReaderT REnv I.M)
type TOp = [Op]

data C3 = C3 { blocks :: M.Map Label (Either [Op] [Op]) }
type C3M = State C3 

