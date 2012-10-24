{-# LANGUAGE TemplateHaskell #-}
module Ocram.Debug.StepMap
-- exports {{{1
(
  step_map
) where

-- imports {{{1
import Data.Maybe (mapMaybe)
import Language.C.Data.Position (posRow)
import Language.C.Data.Node (posOfNode, NodeInfo, nodeInfo)
import Language.C.Syntax.AST (CExternalDeclaration(CFDefExt))
import Ocram.Analysis (Analysis(..), critical_functions, get_all_callees)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Intermediate (Function(..))
import Ocram.Ruab (MapTP, StepMap, t2p_row, PRow, TRow(..))
import Ocram.Util (fromJust_s)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Compiler.Hoopl as H

step_map :: MapTP -> Analysis -> M.Map Symbol Function -> StepMap -- {{{1
step_map _ _ _ = M.empty
{-
step_map mtp ana cfs = M.unionsWith (++) $ map perFunction (M.elems cfs)
  where
    perFunction fun = M.unionsWith (++) (functionCalls 
      where
        name = fun_name fun

        functionCalls = -- {{{2
          map calling . get_callees (anaCallgraph ana) $ name

        calling (ni, callee) =
          let entry = $lookup_s entries callee in
          M.singleton ((t2p . posRow . posOfNode) ni) [(entry, True)]

    functionEntries :: M.Map Symbol PRow -- {{{2
    functionEntries = M.map (t2p . posRow . posOfNode . fun_def) cfs
            `M.union` M.fromList (mapMaybe nonCriticalEntry (anaNonCritical ana))


        m1 = H.postorder_dfs_from (fun_body fun) (fun_entry fun)
        m1 = foldl ctrlflow (M.empty, Nothing) . H.foldGraphNodes . fun_body $ fun
        m2 = map (calling name) . get_callees (anaCallgraph ana) $ name
        

    -- control flow {{{2
    ctrlflow (sm, mprow) (Label _)   = (sm, mprow)
    ctrlflow (sm, mprow) (Cont _ _)  = (sm, mprow)
    ctrlflow (sm, mprow) (Stmt expr) = 
      let
        prow = frommJust_s . t2p_row mtp . enTRow . annotation $ expr
    -- utils {{{2
    t2p = $fromJust_s . t2p_row mtp

data CtrlFact = CtrlFact {
    ctrlNextRow :: Maybe TRow
    ctrlStepMap :: Map TRow [TRow]
  }

ctrlflowLattice :: H.DataflowLattice CtrlFact
ctrlflowLattice = H.DataflowLattice {
    fact_name = "control flow"
  , fact_bot  = CtrlFact Nothing M.empty
  , fact_join = join
  } 
  where
    join _ (OldFact old) (NewFact new) = case ctrlNextRow new of
      Nothing -> $abort "unexpected case"
      Just nextNewRow -> 
      | isJust (ctrlNextRow new) && fromJust (ctrlNextRow new) 
    where
      c = changeIf (M.size
-}

functionCalls :: MapTP -> Analysis -> StepMap -- {{{1
functionCalls mtp ana = M.unionsWith (++) $ concatMap perFunction (critical_functions cg)
  where
    cg = anaCallgraph ana

    perFunction :: Symbol -> [StepMap]
    perFunction caller = mapMaybe (perCall caller) (get_all_callees cg caller)

    perCall :: Symbol -> (Symbol, NodeInfo) -> Maybe StepMap
    perCall caller (callee, ni) = do
      entry <- M.lookup callee functionEntries
      return $ M.singleton ((t2p mtp . TRow . posRow . posOfNode) ni) [(entry, True)]

    functionEntries :: M.Map Symbol PRow -- {{{2
    functionEntries = M.map (t2p mtp . TRow . posRow . posOfNode . nodeInfo) (anaCritical ana)
            `M.union` M.fromList (mapMaybe nonCriticalEntry (anaNonCritical ana))

    nonCriticalEntry (CFDefExt fd) = Just (symbol fd, t2p mtp . TRow . posRow . posOfNode . nodeInfo $ fd)
    nonCriticalEntry _             = Nothing

-- utils {{{1
t2p :: MapTP -> TRow -> PRow -- {{{2
t2p mtp = $fromJust_s . t2p_row mtp
