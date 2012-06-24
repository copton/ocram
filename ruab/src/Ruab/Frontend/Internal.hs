{-# LANGUAGE TemplateHaskell #-}
module Ruab.Frontend.Internal where

-- imports {{{1
import Data.List (groupBy, sortBy)
import Ruab.Util (abort)

data Info -- {{{1
  = InfThread Int
  | InfBreakpoint Int
  | InfHighlight

orderInfo :: Info -> Int
orderInfo (InfThread _) = 0
orderInfo (InfBreakpoint _) = 1
orderInfo InfHighlight = 2

instance Show Info where
  show (InfThread tid) = show tid
  show (InfBreakpoint bid) = show bid
  show InfHighlight = "#"

infoIsThread :: Int -> Info -> Bool
infoIsThread tid (InfThread tid') = tid == tid'
infoIsThread _ _ = False

infoIsBreakpoint :: Int -> Info -> Bool
infoIsBreakpoint bid (InfBreakpoint bid') = bid == bid'
infoIsBreakpoint _ _ = False

infoIsHighlight :: Info -> Bool
infoIsHighlight InfHighlight = True
infoIsHighlight _ = False

type InfoInstance = (Int, Info)

clearHighlight :: [InfoInstance] -> [InfoInstance]
clearHighlight = filter (not . infoIsHighlight . snd)

clearThread :: Int -> [InfoInstance] -> [InfoInstance]
clearThread tid = filter (not . infoIsThread tid . snd)

clearBreakpoint :: Int -> [InfoInstance] -> [InfoInstance]
clearBreakpoint bid = filter (not . infoIsBreakpoint bid . snd)

setHighlight :: Int -> [InfoInstance] -> [InfoInstance]
setHighlight row infos = (row, InfHighlight) : clearHighlight infos

setThread :: Int -> Int -> [InfoInstance] -> [InfoInstance]
setThread row tid infos = (row, InfThread tid) : clearThread tid infos

setBreakpoint :: Int -> Int -> [InfoInstance] -> [InfoInstance]
setBreakpoint row bid infos = (row, InfBreakpoint bid) : clearBreakpoint bid infos

render_info :: [InfoInstance] -> String
render_info = renderAll . map renderRow . groupBy groupf . sortBy sortf
  where
    sortf (row, info) (row', info') =
      case row `compare` row' of
        EQ -> orderInfo info `compare` orderInfo info'
        x -> x

    groupf i i' = fst i == fst i'

    renderRow :: [InfoInstance] -> (Int, String)
    renderRow infos@((row, _):_) = (row, snd (foldl go (0, "") infos))
      where
        go (col, txt) (row', info) = if row /= row' then $abort "assertion failed" else
          let spacing = replicate (orderInfo info - col) ' ' in
          (orderInfo info + 1, txt ++ spacing ++ show info)
    renderRow x = $abort $ "unexpected parameter: " ++ show x

    renderAll :: [(Int, String)] -> String
    renderAll rows = ($"") $ snd $ foldl go (1, id) rows
      where
      go (row, f) (row', txt) =
        let spacing = replicate (row' - row) '\n' in
        (row' + 1, f . showString spacing . showString txt . showString "\n")
  
