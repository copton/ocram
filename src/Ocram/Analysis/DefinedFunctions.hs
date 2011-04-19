module Ocram.Analysis.DefinedFunctions 
-- exports {{{!
(
	collectDefinedFunctions
) where

-- imports {{{1
import Ocram.Types
import Data.Set (fromList)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CFunDef)

-- collectDefinedFunctions :: Context -> Result DefinedFunctions {{{1
collectDefinedFunctions :: Context -> Result DefinedFunctions
collectDefinedFunctions ctx = do
	ast <- getSaneAst ctx
	return $ fromList $ snd $ traverseCTranslUnit (getAst ast) emptyDownState

type UpState = [Symbol]

instance UpVisitor EmptyDownState UpState where
	upCFunDef fd _ _ = [symbol fd]
