module Compilers.Hopc.Frontend.KTyped where

import Data.Data hiding (typeOf)
import Data.Typeable hiding (typeOf)
import Data.Generics.PlateData

import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Frontend.Types

constraints :: KTree -> [(HType, HType)]
constraints k = para tp k
    where 
          tp (KLet n x _) r = tpB (n, x) : concat r
          tp (KLetR bs _) r = map tpB bs ++ concat r
          tp (KApp n args) r = concat r -- undefined
          tp x r = concat r

          tpB (n, x) = (TVar n, typeOf x)

typeOf :: KTree -> HType

typeOf (KLet _ _ e) = typeOf e

typeOf (KLetR _ e)  = typeOf e

typeOf (KLambda args e) = TFun TFunLocal (map TVar args) (typeOf e)

typeOf (KInt _) = TInt

typeOf (KStr _) = TStr

typeOf (KVar n) = TVar n

typeOf (KUnit) = TUnit

typeOf (KApp n _) = TVar "unknown" 

typeOf x = error (show x)

