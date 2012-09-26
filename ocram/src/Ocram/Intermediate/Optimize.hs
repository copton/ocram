{-# LANGUAGE GADTs #-}
module Ocram.Intermediate.Optimize
(
  optimize_ir
) where

import Control.Monad (mplus)
import Compiler.Hoopl (graphMapBlocks, blockMapNodes3, mapFold, mapDelete, mapMap, O, C)
import Ocram.Intermediate.Representation

import Debug.Trace (trace)

optimize_ir :: Body -> Body
optimize_ir = dissolveMiniBlocks . ifElseFusion

ifElseFusion :: Body -> Body
ifElseFusion = graphMapBlocks (blockMapNodes3 (id, id, replace))
  where
    replace o@(If _ tl el)
      | hLabel tl == hLabel el = Goto tl
      | otherwise              = o
    replace o = o

dissolveMiniBlocks :: Body -> Body
dissolveMiniBlocks = form_body . dissolve . block_map
  where
    dissolve :: BlockMap -> BlockMap
    dissolve bm = case mapFold scan Nothing bm of
      Nothing -> bm
      Just (oldLabel, newLabel) -> trace ("XXX: " ++ show (oldLabel, newLabel)) $
          mapMap (blockMapNodes3 (id, id, replace oldLabel newLabel))
        $ mapDelete (hLabel oldLabel) bm 

    scan block s = s `mplus` isMiniBlock (block_components block)

    isMiniBlock (Label oldLbl, [], Goto newLblb) = Just (oldLbl, newLblb)
    isMiniBlock _ = Nothing

    replace :: Label -> Label -> Node O C -> Node O C
    replace oldLbl newLbl o = case o of
      Goto lbl    -> Goto (replaceLabel lbl)
      If cond t e -> If cond (replaceLabel t) (replaceLabel e)
      Call nf lbl -> Call nf (replaceLabel lbl)
      _           -> o
      where
        replaceLabel lbl
          | hLabel lbl == hLabel oldLbl = newLbl
          | otherwise                   = lbl
