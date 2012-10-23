{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ocram.Debug.DebugInfo where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Data.List (nub)
import Ocram.Ruab
import Ocram.Analysis (CallGraph, start_functions, call_order)
import Ocram.Debug.Types (Breakpoints, Breakpoint(..), VarMap')
import Ocram.Intermediate (Function)
import Ocram.Util (abort, fromJust_s)
import Ocram.Names (tfunction)
import Ocram.Symbols (Symbol)
import Text.Regex.Posix ((=~))

import qualified Compiler.Hoopl as H
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

t2p_map :: BS.ByteString -> BS.ByteString -> MapTP -- {{{1
t2p_map tcode pcode
  | BS.null pcode = MapTP 0 0 []
  | BS.last pcode /= '\n' = $abort "pre-processed file should be terminated by a newline"
  | otherwise = MapTP (trows) (prows - 1) (reverse ppm)
  where
    trows = TRow $ length $ BS.split '\n' tcode
    (first:rest) = init $ BS.split '\n' pcode
    (ppm, prows) = foldl go ([], 2) rest
    mainFile = case match first of
      (_, (BS.null -> True), _, _) -> $abort $ "unexpected first row in pre-processed file"
      (_, _, _, (_:file:_)) -> file
      x -> $abort $ "unexpected parameter: " ++ show x

    match :: BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])
    match txt = txt =~ "^# ([0-9]*) \"([^\"]+)\".*$" -- prefix, match, postfix, groups

    go (ppm', row) line = case match line of
      (_, (BS.null -> True), _, _ ) -> (ppm', row + 1)
      (_, _, _, (row':file:_)) -> if file == mainFile
        then (((TRow . read . BS.unpack) row', row) : ppm', row + 1)
        else (ppm', row + 1)
      x -> $abort $ "unexpected parameter:" ++ show x

p2e_map :: MapTP -> FilePath -> Breakpoints -> MapPE -- {{{1
p2e_map mtp tfile = M.toList . foldr insert M.empty . nub . filter ((==(Just tfile)) . bpFile)
  where
    insert bp = M.alter (alter (bpERow bp)) (ploc bp)
    alter erow Nothing      = Just [erow]
    alter erow (Just erows) = Just $ erow : erows
    ploc = PLocation <$> bpThread <*> $fromJust_s . t2p_row mtp . bpTRow <*> bpBlocking

var_map :: MapTP -> VarMap' -> VarMap -- {{{1
var_map mtp = M.toList . foldr insert M.empty
  where
    t2p = $fromJust_s . t2p_row mtp
    insert (var, (start, end), fqn) = M.alter (alter var fqn) (Scope (t2p start) (t2p end))
    alter var fqn Nothing    = Just [(var, fqn)]
    alter var fqn (Just rm)  = Just $ (var, fqn) : rm

all_threads :: CallGraph -> [Thread] -- {{{1
all_threads cg = zipWith create [0..] (start_functions cg)
  where
    co = $fromJust_s . call_order cg
    create tid sf = Thread tid sf (tfunction tid) (co sf)

step_map :: MapTP -> CallGraph -> M.Map Symbol Function -> StepMap -- {{{1
step_map _ _ _ = M.empty
{-
step_map mtp cg cfs = M.unions $ map stepmap (M.elems cfs)
  where
    t2p = $fromJust_s . t2p_row mtp

    stepmap fun = 

    callSites name = map (t2p . posRow. posOfNode . snd) (get_gallers cg name)
      
    startOfFunction name = $fromJust_s $ do
      fun   <- M.lookup name cfs
      block <- H.mapLookup (fun_entry fun) (block_map (fun_body fun))

    startOfBlock block = do
      let (_, (Stmt expr:_), _) = block_components block in
      t2p_row mtp . enTRow . annotation $ expr
-}
