{-# LANGUAGE TemplateHaskell #-}

module Ruab.Frontend.Infos where

-- imports {{{1
import Control.Arrow (first)
import Data.List (groupBy, sortBy)
import Ruab.Util (abort)

import qualified Ruab.Core as C

type BreakpointNumber = Int

data Info -- {{{1
  = InfThread C.ThreadId
  | InfBreakpoint BreakpointNumber
  | InfHighlight
  deriving Show

type InfoInstance = (C.PRow, Info) -- {{{1

-- setters {{{1
clearHighlight :: [InfoInstance] -> [InfoInstance] -- {{{2
clearHighlight = filter (not . infoIsHighlight . snd)

clearThread :: Int -> [InfoInstance] -> [InfoInstance] -- {{{2
clearThread tid infos = filter (not . infoIsThread tid . snd) infos

clearBreakpoint :: Int -> [InfoInstance] -> [InfoInstance] -- {{{2
clearBreakpoint bid infos = filter (not . infoIsBreakpoint bid . snd) infos

setHighlight :: C.PRow -> [InfoInstance] -> [InfoInstance] -- {{{2
setHighlight prow infos = (prow, InfHighlight) : clearHighlight infos

setThread :: C.PRow -> Int -> [InfoInstance] -> [InfoInstance] -- {{{2
setThread prow tid infos = (prow, InfThread tid) : clearThread tid infos

setBreakpoint :: C.PRow -> Int -> [InfoInstance] -> [InfoInstance] -- {{{2
setBreakpoint prow 0 infos = (prow, InfBreakpoint 0) : infos
setBreakpoint prow bid infos = (prow, InfBreakpoint bid) : clearBreakpoint bid infos

class Row a where -- {{{2
  getRow :: a -> Int

instance Row C.TRow where
  getRow = C.getTRow

instance Row C.PRow where
  getRow = C.getPRow

instance Row C.ERow where
  getRow = C.getERow

render_info :: Row r => [(r, Info)] -> String -- {{{1
render_info = renderAll . map renderRow . groupBy groupf . sortBy sortf . map (first getRow)
  where
    sortf (row, info) (row', info') =
      case row `compare` row' of
        EQ -> orderInfo info `compare` orderInfo info'
        x -> x

    groupf i i' = fst i == fst i'

    renderRow infos@((row, _):_) = (row, snd (foldl go (0, "") infos))
      where
        go (col, txt) (row', info) = if row /= row' then $abort "assertion failed" else
          if orderInfo info < col
            then (col, txt)
            else 
              let spacing = replicate (orderInfo info - col) ' ' in
              (orderInfo info + 1, txt ++ spacing ++ render info)
    renderRow x = $abort $ "unexpected parameter: " ++ show x

    renderAll :: [(Int, String)] -> String
    renderAll rows = ($"") $ snd $ foldl go (1, id) rows
      where
      go (row, f) (row', txt) =
        let spacing = replicate (row' - row) '\n' in
        (row' + 1, f . showString spacing . showString txt . showString "\n")
  
render :: Info -> String -- {{{2
render (InfThread tid) = show tid
render (InfBreakpoint bid) = show bid
render InfHighlight = "#"

-- utils {{{1
orderInfo :: Info -> Int -- {{{2
orderInfo (InfThread _) = 0
orderInfo (InfBreakpoint _) = 1
orderInfo InfHighlight = 2

infoIsThread :: Int -> Info -> Bool -- {{{2
infoIsThread tid (InfThread tid') = tid == tid'
infoIsThread _ _ = False

infoIsBreakpoint :: Int -> Info -> Bool -- {{{2
infoIsBreakpoint bid (InfBreakpoint bid') = bid == bid'
infoIsBreakpoint _ _ = False

infoIsHighlight :: Info -> Bool -- {{{2
infoIsHighlight InfHighlight = True
infoIsHighlight _ = False

