{-# LANGUAGE GADTs #-}
module Ocram.Intermediate.Optimize
(
  optimize_ir
) where

import Ocram.Intermediate.Representation
import Control.Monad (mplus)
import Compiler.Hoopl (blockMapNodes3, mapFold, mapDelete, mapMap, O, C)

optimize_ir :: (Label, Body) -> (Label, Body)
optimize_ir = dissolveMiniBlocks

dissolveMiniBlocks :: (Label, Body) -> (Label, Body)
dissolveMiniBlocks (lbl, body) =
  let
    bm = block_map body
    (lbl', bm') = dissolve (lbl, bm)
  in
    (lbl', form_body bm')
  where
    dissolve :: (Label, BlockMap) -> (Label, BlockMap)
    dissolve (entry, bm) = case mapFold scan Nothing bm of
      Nothing -> (entry, bm)
      Just (oldLabel, newLabel) ->
        let
          entry' = replaceLabel oldLabel newLabel entry
          bm' = mapMap (blockMapNodes3 (id, id, replace oldLabel newLabel))
              $ mapDelete (hLabel oldLabel) bm 
        in dissolve (entry', bm')

    scan block s = s `mplus` isMiniBlock (block_components block)

    isMiniBlock (Label oldLbl  , [], Goto newLbl) = Just (oldLbl, newLbl)
    isMiniBlock (Cont oldLblb (FirstNormalForm _ _), [], Goto newLbl) = Just (oldLblb, newLbl)
    isMiniBlock _ = Nothing

    replace :: Label -> Label -> Node O C -> Node O C
    replace ol nl o = case o of
      Goto t      -> Goto (repl t)
      If cond t e -> If cond (repl t) (repl e)
      Call nf t   -> Call nf (repl t)
      _           -> o
      where
        repl = replaceLabel ol nl

    replaceLabel oldLabel newLabel currentLabel
      | hLabel currentLabel == hLabel oldLabel = newLabel
      | otherwise                              = currentLabel
