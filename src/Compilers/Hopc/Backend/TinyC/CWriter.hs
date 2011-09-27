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
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

import Text.Printf

write :: KId -> [Proc] -> CompileM [String]
write ep p = runCWriterM  envInit $ do

      prologue
      entrypoint $ forM_ p $ \p -> do
        comment (name p)
        forM_ (body p) (opcode p)
        empty
      epilogue

  where
    prologue :: CWriterM ()
    prologue = do
      indent "#include <hopcruntime.h>"
      indent "#include \"hopcstubs.h\""
      empty
      cp <- asks checkpoints
      forM_ (M.toList cp) $ \(l, n) -> do
        noindent $ printf "#define %s %d" (decorateCaseLbl (show l)) n
      empty

    epilogue :: CWriterM ()
    epilogue = nothing

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
      indent $ printf "switch(%s) {" (reg retReg)
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

    regType = "unsigned register"

    retReg = R0

    iconst = show

    reg = show

    keepReturn :: Label -> CWriterM ()
    keepReturn l = shift $ stmt $ (reg retReg) ++ " = " ++ decorateCaseLbl (show l)

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
      shift $ stmt $ printf "%s = HOPC_CALLFFI(%s)" (reg r) (intercalate "," (n:args' rs))

    foreign n (RVoid) rs =
      shift $ stmt $ printf "HOPC_CALLFFI(%s)" (intercalate "," (n:args' rs))

    activationRecord :: Proc -> CWriterM ()
    activationRecord p | (slotnum p) > 0 = 
      shift $ stmt $ printf "hopc_allocate_activation_record(runtime, %d)" (slotnum p)

    activationRecord _ = nothing

    deallocateActivationRecord :: Proc -> CWriterM ()
    deallocateActivationRecord p | (slotnum p) > 0 = 
      shift $ stmt $ printf "hopc_deallocate_activation_record(runtime)"

    deallocateActivationRecord _ = nothing

    opcode :: Proc -> Op -> CWriterM ()
    opcode p (Label n) = do
      gotoLabel (show n) >> caseLabel n
      when (n == V.entrypoint p) $ activationRecord p 

    opcode _ (Branch n) = branch n

    opcode p (Return) | (name p) == ep = do
      shiftIndent $ comment $ "return from " ++ name p
      deallocateActivationRecord p
      shift $ goto' exitpointLabel

    opcode p (Return) = do
      shiftIndent $ comment $ "return from " ++ name p
      shift $ goto' entrypointLabel

    opcode _ (Const (LInt v) r) = shift $ stmt $ reg r ++ " = " ++ iconst v

    opcode _ (Move r1 r2) = shift $ stmt $ reg r2 ++ " = " ++ reg r1

    opcode _ (CallL l n _ _) = do
      keepReturn l
      shiftIndent $ comment $ "local call " ++ n
      lbl <- funEntry n
      shift $ goto lbl 
      empty

    opcode _ (CallF _ n  rs r) = do
--      shiftIndent $ comment $ "foreign call " ++ n 
      foreign n r rs
      empty

    opcode _ (CallC _ r _ _) = do
      shiftIndent $ comment $ "closure call " ++ reg r
      empty

    opcode _ (Spill r n) = do
      shiftIndent $ comment $ "spill " ++ reg r ++ " " ++ show n

    opcode _ (Unspill n r) = do
      shiftIndent $ comment $ "unspill " ++ reg r ++ " " ++ show n

    opcode _ (BranchTrue r l) = do
      shift $ stmt $ printf "if(%s) %s" (reg r) (goto l)

    opcode _ (BranchFalse r l) = do
      shift $ stmt $ printf "if(!(%s)) %s" (reg r) (goto l)

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

    funEntry :: KId -> CWriterM Label
    funEntry n =
      asks (M.lookup n.entrypoints) >>= return.fromJust --- FIXME: must always work. but code is dirty

    entrypointsMap =
      M.fromList $ map (\p@(Proc{name=fn, entrypoint=l}) -> (fn, l)) p

    envInit = CWriterEnv entrypointsMap (checkpointsMap entrypointsMap)

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

type CWriterM = StateT Int (WriterT [String] (ReaderT CWriterEnv CompileM))
data CWriterEnv = CWriterEnv {entrypoints :: M.Map KId Label, checkpoints :: M.Map Label Int}

runCWriterM :: CWriterEnv -> CWriterM a -> CompileM [String]
runCWriterM env m = runReaderT (execWriterT (evalStateT m 0)) env

