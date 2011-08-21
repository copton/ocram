module Ocram.Transformation.Inline.Types where

import Control.Monad.Reader
import Control.Monad.Writer

import Language.C.Syntax.AST
import qualified Data.Map as Map
import Ocram.Types

type SymTab = Map.Map Symbol CDecl

data FunctionInfo = FunctionInfo {
		  fiResultType :: CTypeSpec
		, fiParams :: [CDecl]
		, fiVariables :: SymTab
		, fiBody :: Maybe CStat
	} deriving (Show)

type FunctionInfos = Map.Map Symbol FunctionInfo

newtype WR a = WR {
		runWR :: WriterT DebugSymbols (Reader Analysis) a
 	} deriving (
		Monad, 
		MonadWriter DebugSymbols, 
		MonadReader Analysis
	)

execWR :: Analysis -> WR a -> (a, DebugSymbols)
execWR ana f = runReader (runWriterT (runWR f)) ana

