module Analysis.Types.FunctionMap (
    FunctionMap, FunctionId, functionId, functionId'
) where

import Data.Map as Map
import Language.C.Syntax.AST (CFunDef)
import Util.Names (functionName)

newtype FunctionId = FunctionId { funId :: String } deriving (Show, Eq, Ord)

functionId :: String -> FunctionId
functionId name = FunctionId name

functionId' :: CFunDef -> FunctionId
functionId' fd = functionId $ functionName fd

type FunctionMap = Map.Map FunctionId CFunDef
