module Ocram.Transformation.Inline.CriticalFunction
(
	traverse
) where

-- imports {{{1
import Ocram.Visitor
import Ocram.Types

import Language.C.Syntax.AST

import Data.Monoid
import qualified Data.Set as Set

type SymbolTable = Set.Set Symbol

type HandleVar state = CExpr -> state -> (state, CExpr)

data DownState state = DownState {
	  dsUserState :: state
		, dsHandleVar :: HandleVar state
	}

instance DownVisitor (DownState state)

instance (Monoid state) => UpVisitor (DownState state) state where
	


traverse :: (Monoid state) => HandleVar state -> CFunDef -> state -> (state, CFunDef)
traverse h fd st = 
	case traverseCFunDef fd (DownState st h) of
		(Nothing, st') -> (st', fd)
		(Just fd', st') -> (st', fd')
