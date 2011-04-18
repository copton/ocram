module Ocram.Types where

import Control.Monad.Error
import Data.Either
import Language.C.Syntax.AST (CTranslUnit, CDecl, CBlockItem, CTypeSpec, CExtDecl, CFunDef)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- generic {{{1
type Symbol = String

type Result a = Either String a

type FunMap a = Map.Map Symbol a

-- options {{{1
data Options = Options { 
	  optInput :: String
	, optOutput :: String
	, optCppOptions :: String
	, optHelp :: Bool
} deriving Show

-- AST {{{1

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

-- analysis {{{1
-- map of all blocking function declarations
type BlockingFunctions = Set.Set Symbol

-- caller: any function definition, callee: any function definition or any blocking function declaration

type Callers = Set.Set Symbol
type Callees = Set.Set Symbol
data Entry = Entry {cgCallers :: Callers, cgCallees :: Callees} deriving Show

type CallGraph = Map.Map Symbol Entry

-- set of all critical functions
type CriticalFunctions = Set.Set Symbol

-- map of all function definitions
type DefinedFunctions = Set.Set Symbol 

-- list of all start routine names
type StartRoutines = Set.Set Symbol

-- transformation {{{1
type Frame = [CDecl]

data FunctionInfo = FunctionInfo {
	getFunctionName :: Symbol,
	getResultType :: CTypeSpec,
	getTStackFrame :: Frame,
	getBody :: [CBlockItem]
}

type FunctionInfos = Map.Map Symbol FunctionInfo

type TStackFrame = CExtDecl

--- context {{{1
data Context = Context {
	getOptions :: Options,
	getRawAst :: RawAst,
	getSaneAst :: Result SaneAst,
	getBlockingFunctions :: Result BlockingFunctions,
	getDefinedFunctions :: Result DefinedFunctions,
	getStartRoutines :: Result StartRoutines,
	getCallGraph :: Result CallGraph,
	getCyclefreeAst :: Result CyclefreeAst,
	getCriticalFunctions :: Result CriticalFunctions,
	getValidAst :: Result ValidAst,
	getRevisedAst :: Result RevisedAst,
	getFunctionInfos :: Result FunctionInfos,
	getStacklessAst :: Result StacklessAst,
	getOutputAst :: Result OutputAst
	}
