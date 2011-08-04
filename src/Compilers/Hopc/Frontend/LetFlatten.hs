module Compilers.Hopc.Frontend.LetFlatten where

import Data.Generics.Biplate
import Control.Monad.State

import Compilers.Hopc.Frontend.KTree

{-

(let (x (let (y e1) e2)) e3) => (let (y e1) (let (x e2) e3))

-}

flatten :: KTree -> KTree
flatten k = rewriteBi tr k
    where tr (KLet x (KLet y e1 e2) e3) = Just $ (KLet y e1 (KLet x e2 e3))
--          tr (KLetR b e) = undefined
          tr x = Nothing
