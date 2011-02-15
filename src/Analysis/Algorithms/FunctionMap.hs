module Analysis.Algorithms.FunctionMap (
    getFunctions
) where

import Data.Map as Map
import Visitor
import Util.Names (functionName)
import Context
import Analysis.Types.FunctionMap

data State = State {
    stMap :: FunctionMap
}

emptyState = State $ Map.empty

instance Visitor State where
    handleCFunDef fd st = st { stMap = Map.insert (functionId' fd) fd (stMap st)}

getFunctions :: Context -> FunctionMap
getFunctions ctx = stMap $ execTrav traverseCTranslUnit (ctxAst ctx) emptyState
