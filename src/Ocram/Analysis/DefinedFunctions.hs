module Ocram.Analysis.DefinedFunctions 
-- exports {{{!
(
	defined_functions
) where

-- imports {{{1
import Ocram.Types
import Data.Set (fromList)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit, ListVisitor)
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CFunDef)

-- defined_functions :: Ast -> ER DefinedFunctions {{{1
defined_functions :: Ast -> ER DefinedFunctions
defined_functions ast = return $ fromList $ snd $ traverseCTranslUnit ast emptyDownState

type UpState = [Symbol]

instance UpVisitor EmptyDownState UpState where
	upCFunDef o _ _ = (o, [symbol o])

instance ListVisitor EmptyDownState UpState
