module Compilers.Hopc.Backend.TinyC.FromClosure where

import Prelude hiding (init, last)

import Compilers.Hopc.Compile
import Compilers.Hopc.Frontend.Types
import Compilers.Hopc.Frontend.KTree (KId)
import Compilers.Hopc.Frontend.Closure
import Compilers.Hopc.Backend.TinyC.VM

import Data.List hiding (last)
import Data.Maybe

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Generics.Biplate


import Text.PrettyPrint.HughesPJClass (prettyShow)

import Text.Printf
import Debug.Trace

data Conv = Conv { regno :: Int, lbl :: Int, regmap :: M.Map KId R, funlbl :: M.Map KId LabelId}

type ConvM = StateT Conv CompileM

retvalReg = (R 1)

convertVM :: Closure -> CompileM VM
convertVM c = do
    (procs, _) <- convert c
    let v = concatMap (\(Proc _ b) -> b) procs
    return $ VM v

convert  :: Closure -> CompileM ([Proc], Conv)
convert k = trace "TRACE: FromClosure :: convert " $ do

    (l0, s) <- runStateT ( getFunLbl "" ) init

    let procs = [(n,b) | b@(CFun (Fun n _ _ _)) <- universe k]

    (v,  s) <- flip runStateT s $ mapM trProc procs

    return (v, s)

--    ep  <- getEntryPoint
--    lbl <- evalStateT ( maybe (return Nothing) getFunLbl' ep ) s

--    undefined

--    let value = maybe v (\l -> [op (LABEL l0), opc (JUMP l) "entry point"] ++ (skipEntry l v)) lbl


--    let optimized = adhocMov2 $ adhocMov1 value
--    let optimized = adhocMov1 value

--    let optimized = value

--    return $ VM value 

    where 

          trProc :: (KId, Closure) -> ConvM Proc
          trProc (n,b) = do
            e <- trB (n,b)
            l <- getFunLbl n
            return $ Proc l e 

          tr :: R -> Closure -> ConvM [Instr]

          tr r (CLet n e e1) = do
            a <- trB (n,e)
            b <- tr r e1
            return $ a ++ b

          tr r (CLetR binds e)   = do
            binds' <- mapM trB binds
            e' <- tr r e
            return $ concat binds' ++ e'

          tr r (CApplCls n args) = do
            rs <- mapM getReg' args >>= return . unwords . ((:) n) .  map (maybe "r?" prettyShow)
--            l <- getFunLbl n

            regs <- getRegList args 

            -- TODO: regs <> args -> unknown var check
            -- TODO: regs <> at   -> bad function call

            rc' <- getReg' n  -- TODO: CALL-CLOSURE
            trace ("TRACE: CALL CLOSURE " ++ n ++ " " ++ (show rc')) $ return ()

            when ((not.isJust) rc') $ error $ "UNKNOWN VARIABLE " ++ n

            let rc = fromJust rc'

            l <- newlbl

            return $ opc (CALL_CLOSURE rc regs l) ("call-closure " ++ n ++ " " ++ l) : op (LABEL l) : mov retvalReg r "ret. val."
        
          tr r (CApplDir n args) = do
            entry <- lift $ getEntry n
            applDir r n args entry

          tr r (CMakeCls n args) = do
            let a = maybe [] id args
            regs <- getRegList a 
            l <- getFunLbl n
            rr <- newreg
            return $ [op (MAKE_CLOSURE l regs), opc (MOV rr r) n]

          tr r (CVar n) = do
            r2 <- getReg n
--            trace ("TRACE: WTF? " ++ n ++ " " ++ (show r2)) $ return () --- FIXME: CHECK FOR CLOSURE?
--            error "I'm so sorry, but u need typing here"
            return $ mov r2 r (printf "%s -> %s" n (prettyShow r))

          tr r (CCond n e1 e2) = do
            c1 <- tr r e1
            c2 <- tr r e2
            rc  <- getReg n

            l0 <- newlbl
            l1 <- newlbl
            l2 <- newlbl

            let ll0 = LABEL l0
            let ll1 = LABEL l1
            let ll2 = LABEL l2

            return $ op (CJUMP (JumpFake rc) l0 l1) : op ll0 : c1 ++ op (JUMP l2) : op ll1 : c2 ++ op ll2 : []   -- ++ op ll2 : op ll1 : c2 ++ op ll2 : []

          tr r (CInt v)= do
              return $ [op (CONST (show v) r)]

          tr r (CStr s) = do
            return $ [op (CONST "Sx" r)]

          tr r1 (CVar k) = do
            r2' <- getReg' k

            when ((not.isJust) r2') $ error $ "UNKNOWN VARIABLE " ++ k

            let r2 = fromJust r2'

            return $ mov r2 r1 ""

          tr r x = return $ [opc NOP $"unsupported " ++ (prettyShow x)]

          trB :: (KId, Closure) -> ConvM [Instr]

          trB (n, e@(CFun (Fun fn args free k))) = do

            fstart <- getFunLbl n

            trace ("TRACE: convert fstart " ++ n ++ " " ++ (show fstart)) $ return ()

            st@(Conv{lbl=l, funlbl=fl}) <- get

            let newst = init{lbl = l, funlbl = fl}

            (code, (Conv{lbl=nl, funlbl=nfl})) <- lift $ flip runStateT newst $ do
                mapM_ addReg (args ++ free)
                tr retvalReg k

--            let optimized = adhocMov2 $ adhocMov1 code
            let optimized = code
 
            modify (\x -> x{lbl=nl, funlbl=nfl})

            return $ opc (LABEL fstart) fn : optimized ++ [opc RET "ret"]


          trB (n, e) = addReg n >>= flip tr e

          applDir :: R -> KId -> [KId] -> Maybe Entry -> ConvM [Instr]

          applDir r n args (Just (Entry (TFun (TFunForeign ffn) at rt) tp)) = do
            regs <- getRegList args 

            -- TODO: regs <> args -> unknown var check
            -- TODO: regs <> at   -> bad function call

            l <- newlbl

            return $ opc (CALL_FOREIGN ffn regs l) l : op (LABEL l) : mov retvalReg r (printf "retval -> %s" (prettyShow r)) -- TODO: remove boilerplat

          applDir r n args (Just (Entry (TFun (TFunLocal) at rt) tp)) = do
            regs <- getRegList args
            l <- getFunLbl n

            trace ("TRACE: appDir  " ++ (show n) ++ " " ++ (show l)) $ return ()

            -- TODO: regs <> args -> unknown var check
            -- TODO: regs <> at   -> bad function call

            l1 <- newlbl

            return $ opc (CALL_LOCAL l regs l1) (n ++ " " ++ l1) : op (LABEL l1) :  mov retvalReg r (printf "retval -> %s" (prettyShow r)) -- TODO: remove boilerplate

          applDir r n args (Just x) = do
            error $ "CALLING NOT-APPLICABLE ENTITY" ++ n --TODO: error handling

          applDir r n args Nothing = do
            trace ("TRACE: appDir  NOT FOUND" ++ (show n)) $ return ()
            error "oops"

          addReg :: KId -> ConvM R
          addReg n = do
            reg <- newreg
            modify (\x@(Conv {regmap=r}) -> x{regmap=M.insert n reg r})
            return reg

          getRegList args = do 
            regs <- mapM getReg' args
            return $ catMaybes regs

          getReg n = do
            r <- gets (M.lookup n . regmap)
            if isJust r then (return.fromJust) r else addReg n

          getReg' n = do
            gets (M.lookup n . regmap) >>= return

          newreg :: ConvM R
          newreg = do
            s@(Conv {regno = n}) <- get
            let r = R (n+1)
            put s{regno = n+1}
            return r

          addFunLbl :: KId -> LabelId -> ConvM LabelId 
          addFunLbl n l = do
            modify (\x@(Conv {funlbl=fl}) -> x{funlbl=M.insert n l fl})
            return l 

          getFunLbl' :: KId -> ConvM (Maybe LabelId)
          getFunLbl' n = gets (M.lookup n . funlbl)

          getFunLbl :: KId -> ConvM LabelId -- TODO: error handling (unknown label)
          getFunLbl n = do
            l <- gets (M.lookup n . funlbl)
            if isJust l then (return.fromJust) l else newlbl >>= addFunLbl n

          init  = Conv {regno = 3, lbl = 0, regmap = M.empty, funlbl = M.empty} -- FIXME: remove the hardcode

          skipEntry l v = filter entr v
            where entr (I (CALL_LOCAL x _ _) _) | x == l = False
                  entr _ = True

          mov (R a) (R b) dsc | a == b = []
          mov a b dsc = [opc (MOV a b) dsc]


newlbl :: ConvM LabelId
newlbl = do
  s@(Conv {lbl = n}) <- get
  put s{lbl = n+1}
  return $ "L" ++ (show n)

data Block = Block { first :: Maybe Op
                    ,middle :: [Op]
                    ,last :: Maybe Op }
             deriving Show

split :: Conv -> [Instr] -> CompileM [Block]
split s ins = do
    (b, _) <- evalStateT (foldM withIns ([], newbl) ins) s
    return b

    where withIns acc (I x _) = wop acc x
          wop acc x@(JUMP l) = trans acc x
          wop acc x@(CALL_LOCAL _ _ _) = trans acc x
          wop acc x@(CALL_CLOSURE _ _ _) = trans acc x
          wop acc x@(CALL_FOREIGN _ _ _) = trans acc x
          wop acc x@(JUMP l) = trans acc x
          wop acc x@(CJUMP _ _ _) = trans acc x
          wop acc x@(RET) = trans acc x
          wop acc x@(LABEL n) = label acc x
          wop acc x = nontrans acc x

          label acc@(bs, (Block Nothing  _ _)) x = nontrans acc x

          label (bs, b@(Block f@(Just _) (m:ms) _) ) lbl@(LABEL n) = do
            let b' = b { last = Just (JUMP n) }
--            trace ("TRACE: label " ++ (show lbl)) $ return ()
            return (bs ++ [b'], newbl { first = (Just lbl)  })

          label (bs, (Block f@(Just _) (m:ms) _)) _ =
            error "COMPILER INTERNAL ERROR / ASSERTION FAIL" -- FIXME

          trans (bs, (Block f@(Just _) m Nothing)) x =
            return (bs ++ [Block f m (Just x)], newbl)

          trans (bs, (Block Nothing m Nothing)) x = do
            l <- newlbl
            return (bs ++ [Block (Just (LABEL l)) m (Just x)], newbl)

          trans b x =
            error $ "COMPILER ERROR / INVALID CODE STRUCTURE I " ++ (show b) ++ " " ++ (show x) -- FIXME

          nontrans (bs, (Block Nothing [] Nothing)) x =
            return (bs, Block (Just x) [] Nothing) -- first op

          nontrans (bs, (Block f@(Just _) m Nothing)) x =
            return (bs, Block f (m++[x]) Nothing) -- next op
 
          nontrans b x = 
            error $ "COMPILER ERROR / INVALID CODE STRUCTURE II " ++ show (b) ++ " " ++ (show x) -- FIXME
          
          newbl = Block Nothing [] Nothing

{-

adhocMov1 :: [Instr] -> [Instr]
adhocMov1 a = scan a

    where scan ((I x1@(MOV (R a1) (R b1)) _) : x@(I (MOV (R a2) (R b2)) _)  : xs )  | b1 == a2  = 
            scan ((I (MOV (R a1) (R b2)) ""):xs)

          scan (x:xs) = x : scan xs

          scan [] = []


adhocMov2 :: [Instr] -> [Instr]
adhocMov2 a = scanMov a

    where scanMov ((x@(I (MOV (R a1) (R b1)) _)):xs) = 
            let (b, rs) = scan (a1,b1) ([], xs)
            in x : b ++ scanMov rs

          scanMov (x:xs) = x : scanMov xs

          scanMov [] = []

          scan :: (RId,RId) -> ([Instr], [Instr]) ->  ([Instr], [Instr])
          
          scan rr (a, x@(I (CALL_FOREIGN _ _ _) _ ):xs) = (a ++ [repl rr x], xs)

          scan rr (a, x@(I (CALL_LOCAL _ _ _) _):xs) = (a ++ [repl rr x], xs)

          scan rr (a, x@(I (CALL_CLOSURE _ _ _) _ ):xs) = (a ++ [repl rr x], xs)

          scan rr (a, x@(I (MOV (R a1) (R b1)) _):xs) | b1 /= (fst rr) = scan rr (a++[x], xs)

          scan rr (a, x@(I (CJUMP _  _) _):xs)  = (a ++ [repl rr x], xs)
       
          scan rr (a, x@(I (CONST _  _) _):xs)  = scan rr (a ++ [x], xs)

          scan rr (a, x:xs) = (a, x:xs)

          repl rr (I (CALL_LOCAL l regs lr) d) =   I (CALL_LOCAL l (map (reg rr) regs) lr ) d
          repl rr (I (CALL_FOREIGN n regs lr) d) = I (CALL_FOREIGN n (map (reg rr) regs) lr ) d
          repl rr (I (CALL_CLOSURE rc regs lr) d) = I (CALL_CLOSURE (reg rr rc) (map (reg rr) regs) lr) d
          repl rr (I (CJUMP (JumpFake r) l) d) = (I (CJUMP (JumpFake (reg rr r)) l) d)
          repl rr x = x
--          repl rr x = trace ("TRACE: repl " ++ (show rr) ++ " " ++ (show x)) x 

          reg (r1,r2) (R x) | r2 == x = (R r1)
                            | otherwise = (R x)


-}

