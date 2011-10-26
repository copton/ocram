module Ocram.Transformation.Inline.Types where

import Control.Monad.Reader
import Control.Monad.Writer
import Ocram.Types (DebugSymbols)
import Ocram.Analysis (CallGraph)

newtype WR a = WR {
    runWR :: WriterT DebugSymbols (Reader CallGraph) a
  } deriving (
    Monad, 
    MonadWriter DebugSymbols, 
    MonadReader CallGraph 
  )

execWR :: CallGraph -> WR a -> (a, DebugSymbols)
execWR cg f = runReader (runWriterT (runWR f)) cg
