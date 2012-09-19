module Ocram.Backend
-- exports {{{1
(
  tcode_2_ecode
) where

-- imports {{{1
import Language.C.Syntax.AST
import Language.C.Data.Node (undefNode)
import Ocram.Analysis (Analysis(..))
import Ocram.Backend.TStack
import Ocram.Backend.EStack
import Ocram.Backend.BlockingFunctionDeclaration
import Ocram.Backend.ThreadExecutionFunction
import Ocram.Intermediate (Function)
import Ocram.Symbols (Symbol)

import qualified Data.Map as M
 
tcode_2_ecode :: Analysis -> M.Map Symbol Function -> CTranslUnit -- {{{1
tcode_2_ecode ana cfs =
  let
    (tframes, tstacks) = create_tstacks (anaCallgraph ana) (anaBlocking ana) cfs
    (eframes, estacks) = create_estacks (anaCallgraph ana) cfs
    bfds               = blocking_function_declarations (anaBlocking ana)
    tefs               = thread_execution_functions (anaCallgraph ana) (anaBlocking ana) cfs estacks

    decls              = anaNonCritical ana ++ map CDeclExt (tframes ++ tstacks ++ eframes ++ bfds)
    fundefs            = map CFDefExt tefs
  in
    CTranslUnit (decls ++ fundefs) undefNode
