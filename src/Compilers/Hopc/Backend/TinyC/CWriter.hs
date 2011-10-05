{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}

module Compilers.Hopc.Backend.TinyC.CWriter where

import Compilers.Hopc.Compile
import Compilers.Hopc.Id
import Compilers.Hopc.Frontend.Types
import qualified Compilers.Hopc.Backend.TinyC.R as R
import Compilers.Hopc.Backend.TinyC.VM
import qualified Compilers.Hopc.Backend.TinyC.VM as V
import Compilers.Hopc.Backend.TinyC.Lit

import Compiler.Hoopl
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

import Text.Printf
import Debug.Trace

write :: KId -> [Proc] -> CompileM [String]
write ep p = do
      (tl, tref) <- uniqSpillTagMap (closureTag+1)
      runCWriterM (envInit tl tref) $ do
        prologue
        entrypoint $ forM_ p $ \p -> do
          comment (name p)
          comment $ printf "arity: %d  freevars: %d" (arity p) (freevarsnum p)
          forM_ (body p) (opcode p)
          empty
        epilogue

  where

    closureTag = 1

    prologue :: CWriterM ()
    prologue = do

      indent "#include <hopcruntime.h>"
      indent "#include \"hopcstubs.h\""

      empty

      indent $ printf "#define HOPCCLOSURETAG %d" (closureTag)

      comment "FIXME: more general way to allocate the heap"
      indent $ "#define HOPCINITIALHEAPSIZE 8192 // words"
      indent $ stmt $ "hcell heap[HOPCINITIALHEAPSIZE] = { { .w = 0 } }"

      empty

      cp <- asks checkpoints
      forM_ (M.toList cp) $ \(l, n) -> do
        noindent $ printf "#define %s %d" (decorateCaseLbl (show l)) n


      empty

      sc <- asks strings 
      forM_ (M.toList sc) $ \(s, n) -> do
        comment $ show s
        noindent $ stmt $ printf "const hword_t %s[] = %s" n (sencode s)

      empty

      indent $ "const hopc_tagdata tagdata[] = {"
      shift $ " {CELLS(sizeof(hopc_task)), {0}}"
      shift $ ",{CELLS(sizeof(hopc_closure)), {0}} // HOPCCLOSURETAG " 

      -- insert spill's tags
      tl <- asks spillTags
      comment $ "spill tags"
      forM_ tl $ \(tagn, tagr) -> do
        shift $ printf ",{%d, {0}} // %d" (tagSize tagr) tagn
--      forM_ (M.toList spillmap) $ \(n, len) -> $ do
--        indent $ "{}"

      indent $ "}"
      indent $ stmt ""

      indent $ stmt "static hcell localcallregs[HOPCREGNUM] = {{ .w = 0 }}"

      empty
 
      forM_ (zip (R.avail :: [R]) ([1..] :: [Int])) $ \(r,i) -> do
        let args = [printf "x%d" m | m <- take i ([1..]::[Int])]
        indent $ printf "#define LOCALCALLARGS%d(%s) \\" i (intercalate "," args)
        forM_  (zip args [1..])$ \(s,j) -> do
          shift $ printf "localcallregs[%d] = (%s) ; \\" (j :: Int) s
        forM_  (zip (R.avail :: [R]) [1..i]) $ \(r,j) -> do
          shift $ printf "%s = localcallregs[%d]; \\" (show r) (j::Int)
        empty

      empty

    epilogue :: CWriterM ()
    epilogue = do
      empty
      empty
      indent $ "int main() {"
      pushIndent
      indent $ stmt $ "static hopc_runtime runtime = { .tagdata = tagdata }"

      empty

      indent $ stmt $ "hopc_init_runtime(&runtime, heap, HOPCINITIALHEAPSIZE)"

      empty

      comment "root task"
      indent $ stmt "hopc_insert_task(&runtime)";

      indent $ stmt $ "hopc_entrypoint(&runtime)"
      indent $ stmt $ "return 0"
      popIndent 
      indent $ "}"

    entrypoint :: CWriterM () -> CWriterM () 
    entrypoint m = do
      indent "void hopc_entrypoint(hopc_runtime *runtime) {"

      shift $ stmt $ regType ++ " " ++ intercalate ", " (map reg R.allRegs)
      empty
      lep <- funEntry ep
      shift $ goto lep
      empty
      gotoLabel entrypointLabel
      pushIndent
      indent $ printf "switch(W(%s)) {" (reg retReg)
      pushIndent
      m
      gotoLabel exitpointLabel
      indent $ stmt $ "default: break"
      popIndent
      indent "}"
      popIndent
      indent "}"

    gotoLabel :: String -> CWriterM ()
    gotoLabel s = tell [s ++ ":"]

    noindent :: String -> CWriterM ()
    noindent s = tell [s]

    indent :: String -> CWriterM ()
    indent s = do
      i <- get
      let shift = concat $ replicate i "    "
      tell [shift ++ s]

    empty = tell [""]

    nothing :: CWriterM ()
    nothing = return ()

    comment :: String -> CWriterM ()
    comment s = indent $ "// " ++ s

    entrypointLabel = "entrypoint"
    exitpointLabel = "exitpoint"

    regType = "register hcell"

    retReg = R0

    iconst = show

    reg = show

    keepReturn :: Label -> CWriterM ()
    keepReturn l = shift $ stmt $ printf "W(%s) = %s" (reg retReg) (decorateCaseLbl (show l))

    goto l = stmt $ "goto " ++ show l

    goto' :: String -> String 
    goto' s = stmt $ "goto " ++ s

    branch :: Label -> CWriterM ()
    branch l = do 
      shift $ goto l 
      empty

    decorateCaseLbl :: String -> String
    decorateCaseLbl s = "C" ++ s

    caseLabel :: Label -> CWriterM ()
    caseLabel n = do
      cp <- asks checkpoints >>= return . M.member n
      when cp $ indent $ "case " ++ decorateCaseLbl (show n) ++ ":"

    args :: [R] -> String
    args rs = intercalate ", " $ map reg rs
    
    args' = map reg

    foreign :: KId -> RT -> [R] -> CWriterM ()
    foreign n (RReg r) rs =
      shift $ stmt $ printf "W(%s) = HOPC_CALLFFI(%s)" (reg r) (intercalate "," (n:args' rs))

    foreign n (RVoid) rs =
      shift $ stmt $ printf "HOPC_CALLFFI(%s)" (intercalate "," (n:args' rs))

    activationRecord :: Proc -> CWriterM ()
    activationRecord p | (slotnum p) > 0 = do
      tag <- asks (fromJust . M.lookup (name p) . spillTagRefs)
      shift $ stmt $ printf "hopc_push_activation_record(runtime, %d)" tag
      let alloc = fromJust $ M.lookup (name p) funAllocMap
      let spl = M.fromList $
                  map varname $
                    maybe [] S.toList $ M.lookup (V.entrypoint p) (spill alloc)
      let av = M.lookup activationRecordVariable spl
      case av of
        Nothing -> nothing
        Just (k,r) -> -- spill R0 first if it must be spilled
          shift $ printf "hopc_spill(runtime, %d, %s);  /* %s */" k (reg r) activationRecordVariable
      empty
      where varname (n, r, k) = (n,(k,r))

    activationRecord _ = nothing

    deallocateActivationRecord :: Proc -> CWriterM ()
    deallocateActivationRecord p | (slotnum p) > 0 =
      shift $ stmt $ printf "hopc_pop_activation_record(runtime)"

    deallocateActivationRecord _ = nothing

    opcode :: Proc -> Op -> CWriterM ()
    opcode p (Label n) = do
      gotoLabel (show n)
      when (n == V.entrypoint p) $ activationRecord p
      caseLabel n

    opcode _ (Branch n) = branch n

    opcode p (Return) | (name p) == ep = do
      deallocateActivationRecord p
      shiftIndent $ comment $ "return from " ++ name p
      shift $ goto' exitpointLabel

    opcode p (Return) = do
      deallocateActivationRecord p
      shiftIndent $ comment $ "return from " ++ name p
      shift $ goto' entrypointLabel

    opcode _ (Const (LInt v) r) = shift $ stmt $ printf "W(%s) = %s" (reg r) (iconst v)

    opcode _ (Const (LStr s) r) = do
      sname <- asks (fromJust . M.lookup s . strings) -- FIXME: must mork
      shift $ stmt $ printf "P(%s) = S(%s)" (reg r) sname

    opcode _ (Move r1 r2) = shift $ stmt $ reg r2 ++ " = " ++ reg r1

    opcode _ (CallL l n rs _) = do
      keepReturn l
      shiftIndent $ comment $ "local call " ++ n
      lbl <- funEntry n
      when (not (null rs)) $
        shift $ printf "LOCALCALLARGS%d(%s)" (length rs) (intercalate "," (map reg rs))
 
      shift $ goto lbl 
      empty

    opcode _ (CallF _ n  rs r) = do
      shiftIndent $ comment $ "foreign call " ++ n 
      foreign n r rs

    opcode _ (CallC l rc rs _) = do
      let cl = decorateCaseLbl (show l)
      shiftIndent $ comment $ "closure call " ++ (reg rc)
      shiftIndent $ comment "critical section?"
      shift $ stmt $ printf "hopc_push_activation_record2(runtime, CLOSURE_AR(runtime, %s))" (reg rc)
      shift $ stmt $ printf "hopc_spill(runtime, 0, ((hcell)((hword_t)%s)))" cl -- FIXME: hardcode of activationRecordVariable slot
      shift $ stmt $ printf "R0 = CLOSURE_CP(runtime, %s)" (reg rc)
      when (not (null rs)) $
        shift $ printf "LOCALCALLARGS%d(%s)" (length rs) (intercalate "," (map reg rs))
      shift $ goto' entrypointLabel
      empty

    opcode _ (Spill v r n) = do    
--      shiftIndent $ comment $ "spill " ++ reg r ++ " " ++ show n
      if v /= activationRecordVariable
        then shift $ printf "hopc_spill(runtime, %d, %s);  /* %s */" n (reg r) v
        else nothing -- R0 is spilled in prologue

    opcode _ (Unspill n r) = do
--      shiftIndent $ comment $ "unspill " ++ reg r ++ " " ++ show n
      shift $ stmt $ printf "%s = hopc_unspill(runtime, %d)" (reg r) n

    opcode _ (BranchTrue r l) = do
      shift $ stmt $ printf "if(%s) %s" (reg r) (goto l)

    opcode _ (BranchFalse r l) = do
      shift $ stmt $ printf "if(!(%s)) %s" (reg r) (goto l)

    opcode p (MkClos n rs rt) = do
      lself <- funEntry (name p)
      l <- funEntry n
      let alloc = maybe S.empty id $ M.lookup l $ spill $ fromJust $ M.lookup n funAllocMap -- FIXME: must work but ugly
      let slots = M.fromList $ map (\(n,_,sn) -> (n,sn))$ S.toList $ alloc
--      let fv = slots `M.intersection` (M.fromList $ (map (\a -> (a,a))) $ funFreeVars n)
      let fv = catMaybes $ map (flip M.lookup slots) $ funFreeVars n
      let spl = zip rs fv
      when (length spl /= (length.funFreeVars) n) $ error "INTERNAL ERROR / COMPILER ERROR: make-closure free vars mismatched"  -- FIXME
      tag <- asks (fromJust . M.lookup n . spillTagRefs)
      shift $ stmt $ printf "P(%s) = (hword_t*)hopc_make_activation_record(runtime, %d)" (reg rt) tag
      forM_ spl $ \(r,i) -> do
        shift $ stmt $ printf "hopc_spill_ar(runtime, (hcell*)P(%s), %d, %s)" (reg rt) i (reg r)
      let cl = decorateCaseLbl (show l)
      shift $ stmt $ printf "P(%s) = (hword_t*)hopc_make_closure(runtime, %s, (hcell*)P(%s), HOPCCLOSURETAG)" (reg rt) cl (reg rt)
      empty

    opcode _ x = shiftIndent $ comment $ show x

    shiftIndent :: CWriterM () -> CWriterM ()
    shiftIndent m = pushIndent >> m >> popIndent

    shift = shiftIndent . indent

    stmt :: String -> String 
    stmt s = s ++ ";"

    pushIndent :: CWriterM ()
    pushIndent = modify succ

    popIndent :: CWriterM ()
    popIndent = modify pred

    sencode :: String -> String 
    sencode s = "{" ++ enc ++ "}"
      where enc = intercalate "," $ map (printf "0x%02X") $ length s : map ord s ++ [0]

    funMap :: M.Map KId Proc
    funMap = M.fromList $ map (\(fp@(Proc{name=n})) -> (n,fp)) p

    funFreeVars :: KId -> [KId]
    funFreeVars n = fv (fromJust (M.lookup n funMap))
      where fv (Proc{freevarsnum=fvn, args=as}) | fvn > 0 = drop (length as - fvn) as
                                                | otherwise = []

    funAllocMap :: M.Map KId RegAllocation
    funAllocMap = M.fromList $ map (\(Proc{name=n, allocation=a})  -> (n,a)) p

    funEntry :: KId -> CWriterM Label
    funEntry n =
      asks (M.lookup n.entrypoints) >>= return.fromJust --- FIXME: must always work. but code is dirty

    funVars :: KId -> [KId]
    funVars = undefined

    entrypointsMap =
--      trace "entrypointsMap" $ trace (intercalate "\n" (map (const "QQ") p)) $
      M.fromList $ map (\p@(Proc{name=fn, entrypoint=l}) -> (fn, l)) p

    envInit r t = CWriterEnv entrypointsMap (checkpointsMap entrypointsMap) sconsts r t

    checkpointsMap eps =
      let labels = foldl labelOf [] $ concatMap body p
          refs   = S.fromList $ foldl ref [] $ concatMap body p
      in M.fromList $ zip (filter (flip S.member refs) labels) [0..]
      where labelOf acc (Label l) = acc ++ [l]
            labelOf acc _ = acc
            ref acc (MkClos n _ _) = acc ++ [fromJust $ M.lookup n eps]
            ref acc (CallL l _ _ _) = acc ++ [l]
            ref acc (CallC l _ _ _) = acc ++ [l]
--            ref acc (CallF l _ _ _) = acc ++ [l]
            ref acc _ = acc

    sconsts = 
      let ss = foldl s [] $ concatMap body p
      in M.fromList $ zip (S.toList $ S.fromList ss) ["sConst" ++ show x |x <- [0..]]
      where s acc (Const (LStr s) _) = s : acc
            s acc _ = acc

    uniqSpillTagMap :: Int -> CompileM ([(Int, TagRecord)], M.Map KId Int)
    uniqSpillTagMap ns = do
      let tags = map tagOf p
      let tagn = M.fromList $ zip (S.toList (S.fromList (map snd tags))) [ns..]
      let tagl = map swap $ M.toList tagn
      let tagref = M.fromList $ map (\(n,t) -> (n,fromJust (M.lookup t tagn))) tags
      return (sortBy cmp tagl, tagref)
      where tagOf (Proc{name=nm, slotnum=sn}) = (nm, TagRecord (2+sn)) -- FIXME: constant hardcoding!
            cmp a b = compare (fst a) (fst b)

type CWriterM = StateT Int (WriterT [String] (ReaderT CWriterEnv CompileM))
data CWriterEnv = CWriterEnv { entrypoints :: M.Map KId Label
                             , checkpoints :: M.Map Label Int
                             , strings :: M.Map String String
                             , spillTags :: [(Int, TagRecord)]
                             , spillTagRefs :: M.Map KId Int
                             }

data TagRecord = TagRecord { tagSize :: Int } deriving (Eq, Ord)

runCWriterM :: CWriterEnv -> CWriterM a -> CompileM [String]
runCWriterM env m = runReaderT (execWriterT (evalStateT m 0)) env

