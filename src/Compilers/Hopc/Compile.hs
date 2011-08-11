module Compilers.Hopc.Compile where

import qualified Data.Map as M

import Compilers.Hopc.Frontend.KTree
import Compilers.Hopc.Frontend.Types

data Entry = Entry { eType :: HType } 

type Dict = M.Map KId Entry 

data Compile = Compile Dict 

