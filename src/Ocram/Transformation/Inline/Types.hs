module Ocram.Transformation.Inline.Types where

import Control.Monad.Reader
import Control.Monad.Writer
import Language.C.Syntax.AST
import Ocram.Symbols (Symbol)
import Ocram.Types
import Ocram.Analysis (CallGraph)
import qualified Data.Map as Map

type SymTab = Map.Map Symbol CDecl

data FunctionInfo = FunctionInfo {
      fiResultType :: CTypeSpec
    , fiParams :: [CDecl]
    , fiVariables :: SymTab
    , fiBody :: Maybe CStat
  } deriving (Show)

newtype WR a = WR {
    runWR :: WriterT DebugSymbols (Reader CallGraph) a
  } deriving (
    Monad, 
    MonadWriter DebugSymbols, 
    MonadReader CallGraph 
  )

execWR :: CallGraph -> WR a -> (a, DebugSymbols)
execWR cg f = runReader (runWriterT (runWR f)) cg
