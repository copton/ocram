module Ocram.Types where

import Control.Monad.Error
import Data.Either
import Language.C.Syntax.AST (CTranslUnit, CDecl, CBlockItem, CTypeSpec, CExtDecl, CFunDef)
import qualified Data.Map as Map
import qualified Data.Set as Set

-------------
-- generic
type Symbol = String

type Result a = Either String a

-------------
-- options
data Options = Options { 
	  optInput :: String
	, optOutput :: String
	, optCppOptions :: String
	, optHelp :: Bool
} deriving Show

-------------
-- AST

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

newtype RevisedAst = RevisedAst Ast
instance AstC RevisedAst where
	getAst (RevisedAst ast) = ast

newtype StacklessAst = StacklessAst Ast
instance AstC StacklessAst where
	getAst (StacklessAst ast) = ast

newtype OutputAst = OutputAst Ast
instance AstC OutputAst where
	getAst (OutputAst ast) = ast

-------------
-- analysis
-- map of all blocking function declarations
type BlockingFunctions = Map.Map Symbol CExtDecl

-- caller: any function definition, callee: any function definition or any blocking function declaration

type Callers = Set.Set Symbol
type Callees = Set.Set Symbol
data Entry = Entry {cgCallers :: Callers, cgCallees :: Callees} deriving Show

type CallGraph = Map.Map Symbol Entry

-- set of all critical functions
type CriticalFunctions = Set.Set Symbol

-- map of all function definitions
type FunctionMap = Map.Map Symbol CFunDef

-- list of all start routine names
type StartRoutines = [Symbol]

-------------
-- Transformation
type Frame = [CDecl]

data FunctionInfo = FunctionInfo {
	getFunctionName :: Symbol,
	getResultType :: CTypeSpec,
	getTStackFrame :: Frame,
	getBody :: [CBlockItem]
}

type FunctionInfos = Map.Map Symbol FunctionInfo

type TStackFrame = CExtDecl
