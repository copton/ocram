{-# LANGUAGE GADTs #-}
module Ocram.Intermediate.Optimize
(
  optimize_ir
) where

import Control.Monad (mplus)
import Compiler.Hoopl (graphMapBlocks, blockMapNodes3, mapFold, mapDelete, mapMap, O, C)
import Ocram.Intermediate.Representation

optimize_ir :: (Label, Body) -> (Label, Body)
optimize_ir = dissolveMiniBlocks . ifElseFusion

ifElseFusion :: (Label, Body) -> (Label, Body)
ifElseFusion (lbl, body) = (lbl, graphMapBlocks (blockMapNodes3 (id, id, replace)) body)
  where
    replace o@(If _ tl el)
      | hLabel tl == hLabel el = Goto tl
      | otherwise              = o
    replace o = o

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

    isMiniBlock (Label oldLbl, [], Goto newLblb) = Just (oldLbl, newLblb)
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
