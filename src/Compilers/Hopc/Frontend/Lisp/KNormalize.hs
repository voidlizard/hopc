module Compilers.Hopc.Frontend.Lisp.KNormalize where

import qualified Data.ByteString.Char8 as BS
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Abs
import Compilers.Hopc.Frontend.Lisp.BNFC.Lisp.Par
import Compilers.Hopc.Frontend.KTree

toString :: BS.ByteString -> String
toString = BS.unpack

kNormalize :: Exp -> KTree

kNormalize (EInt i) = KInt i

kNormalize (EStr s) = KStr s

kNormalize (EAtom (AtomT (p,bs))) = KVar bs

kNormalize (EList _ _ _ _) = error "List literals are not supported yet"

kNormalize (ELet p1 p2 n eb p3 e p4) = error "let is not supported yet"

kNormalize (EApply p1 n args p2) = error "Apply is not supported yet"


