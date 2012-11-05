{-# LANGUAGE TemplateHaskell #-}
module Ocram.Analysis 
-- export {{{1
(
    analysis, Analysis(..)
  , module Ocram.Analysis.CallGraph
  , module Ocram.Analysis.Filter
  , CallGraph
) where

-- import {{{1
import Language.C.Syntax.AST
import Ocram.Analysis.CallGraph
import Ocram.Analysis.Filter
import Ocram.Analysis.Types (CallGraph)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Text (OcramError)
import Ocram.Query (is_function_declaration)
import Ocram.Util (abort, unexp)

import qualified Data.Map as M

data Analysis = Analysis { -- {{{1
    anaNonCritical :: [CExtDecl]
  , anaCritical    :: M.Map Symbol CFunDef
  , anaBlocking    :: M.Map Symbol CDecl
  , anaCallgraph   :: CallGraph 
  }

analysis :: CTranslUnit -> Either [OcramError] Analysis -- {{{1
analysis ast@(CTranslUnit eds _) = do
  global_constraints ast
  let cg = call_graph ast
  critical_constraints ast cg
  return $ populate cg

  where
    populate cg = foldr seperate (Analysis [] M.empty M.empty cg) eds

    seperate o@(CDeclExt cd) ana
      | is_function_declaration cd && is_blocking (anaCallgraph ana) (symbol cd) =
          addBlocking ana cd
      | otherwise =
          addNonCritical ana o

    seperate o@(CFDefExt fd) ana
      | is_critical (anaCallgraph ana) (symbol fd) = addCritical ana fd
      | otherwise = addNonCritical ana o
  
    seperate x _ = $abort $ unexp x

    addNonCritical a x =
      a {anaNonCritical = x : (anaNonCritical a)}

    addCritical a x =
      a {anaCritical = M.insert (symbol x) x (anaCritical a)}

    addBlocking a x =
      a {anaBlocking = M.insert (symbol x) x (anaBlocking a)}

    
