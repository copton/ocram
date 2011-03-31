module Ocram.Types where

import Control.Monad.Error
import Data.Either
import Language.C.Syntax.AST (CTranslUnit)

type Result a = Either String a

type Ast = CTranslUnit

class AstC a where
	getAst :: a -> Ast

newtype RawAst = RawAst Ast
instance AstC RawAst where
	getAst (RawAst ast) = ast

newtype SaneAst = SaneAst Ast
instance AstC SaneAst where
	getAst (SaneAst ast) = ast

newtype CyclefreeAst = CyclefreeAst Ast
instance AstC CyclefreeAst where
	getAst (CyclefreeAst ast) = ast

newtype ValidAst = ValidAst Ast
instance AstC ValidAst where
	getAst (ValidAst ast) = ast

newtype OutputAst = OutputAst Ast
instance AstC OutputAst where
	getAst (OutputAst ast) = ast

