{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-} 
module Ocram.Debug
-- export {{{1
(
  VarMap,
  TLocation(..), ELocation(..), Location(..), LocMap,
  ENodeInfo(..), enrichNodeInfo, un, enableBreakpoint, setThread, validBreakpoint, tlocation, eNodeInfo,
  format_debug_info
) where

-- import {{{1
import Data.Data (Data)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Language.C.Data.Node (CNode(..), lengthOfNode, isUndefNode, NodeInfo, undefNode, posOfNode)
import Language.C.Syntax.AST (CExpression(CCall))
import Language.C.Data.Position (posRow, posColumn)
import Ocram.Options
import Ocram.Symbols (Symbol)
import Ocram.Util (abort, unexp, fromJust_s)
import System.FilePath ((</>))
import Text.Regex.Posix ((=~))
import Text.JSON (encodeStrict, toJSObject, toJSString, JSON(..), JSValue(JSString, JSObject))

import qualified Data.ByteString.Char8 as BS

data File = -- {{{1
  File {fileName :: String, fileChecksum :: String}

instance JSON File where
  readJSON _ = undefined
  
  showJSON (File name cs) = JSObject $ toJSObject [
      ("file", (JSString . toJSString) name),
      ("checksum", (JSString . toJSString) cs)
    ]

data TLocation = -- {{{1
  TLocation {tlocRow :: Int, tlocCol :: Int, tlocLen :: Int}

instance Show TLocation where
  show (TLocation r c l) = show (r, c, l)

data ELocation = -- {{{1
  ELocation {elocRow :: Int, elocCol :: Int, elocTidd :: Maybe Int}

instance Show ELocation where
  show (ELocation r c t) = show (r, c, t)

data Location = Location TLocation ELocation -- {{{1

instance JSON Location where
  readJSON _ = undefined

  showJSON (Location (TLocation r c l) (ELocation r' c' t)) =
    case t of
      Nothing -> showJSON [r, c, l, r', c']
      (Just t') -> showJSON [r, c, l, r', c', t']
  
type LocMap = [Location] -- {{{1

type PrepLocation = (Int, Int)

type PrepMap = [PrepLocation] -- {{{1

type Variable = Symbol -- {{{1
type VarMap = [(Variable, Variable)]

data ENodeInfo = ENodeInfo { -- {{{1
  tnodeInfo :: NodeInfo,
  threadId :: Maybe Int,
  isBreakpoint :: Bool
  } deriving (Data, Typeable)

instance CNode ENodeInfo where
  nodeInfo = tnodeInfo

instance Show ENodeInfo where
  show _ = ""

enrichNodeInfo :: NodeInfo -> ENodeInfo -- {{{1
enrichNodeInfo ni = ENodeInfo ni Nothing False

un :: ENodeInfo -- {{{1
un = enrichNodeInfo undefNode

enableBreakpoint :: ENodeInfo -> ENodeInfo -- {{{1
enableBreakpoint eni
  | isUndefNode (tnodeInfo eni) = $abort "enabling breakpoint for undefined node"
  | otherwise = eni {isBreakpoint = True}

validBreakpoint :: ENodeInfo -> Bool -- {{{1
validBreakpoint (ENodeInfo tni _ bp) = bp && not (isUndefNode tni)

setThread :: Int -> ENodeInfo -> ENodeInfo -- {{{1
setThread tid eni = eni {threadId = Just tid}

tlocation :: ENodeInfo -> TLocation -- {{{1
tlocation eni =
  let
    ni = tnodeInfo eni
    pos = posOfNode ni
  in
    TLocation (posRow pos) (posColumn pos) (fromMaybe (-1) (lengthOfNode ni))

format_debug_info :: Options -> FilePath -> BS.ByteString -> BS.ByteString -> BS.ByteString -> LocMap -> VarMap -> BS.ByteString -- {{{1
format_debug_info opt cwd tcode ptcode ecode lm vm =
  (BS.pack . encodeStrict . toJSObject) [
    ("tcode", showJSON (File (cwd </> optInput opt) (md5sum tcode))),
    ("ptcode", showJSON (File (cwd </> $fromJust_s (optPTFile opt)) (md5sum ptcode))),
    ("ecode", showJSON (File (cwd </> optOutput opt) (md5sum ecode))),
    ("prepmap", showJSON (prepMap ptcode)),
    ("locmap", showJSON lm),
    ("varmap", showJSON vm)
  ]

prepMap :: BS.ByteString -> PrepMap
prepMap code = (reverse . fst . foldl go ([], 2)) rest
  where
    (first:rest) = BS.split '\n' code
    match :: BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])
    match txt = txt =~ "^# ([0-9]*) \"([^\"]+)\".*$"
    mainFile = case match first of
      (_, (BS.null -> True), _, _) -> $abort $ "unexpected first row in pre-processed file"
      (_, _, _, (_:file:_)) -> file
      x -> $abort $ "unexpected parameter: " ++ show x
    go (tplm, row) line = case match line of
      (_, (BS.null -> True), _, _ ) -> (tplm, row + 1)
      (_, _, _, (row':file:_)) -> if file == mainFile
        then (((read . BS.unpack) row', row) : tplm, row + 1)
        else (tplm, row + 1)
      x -> $abort $ "unexpected parameter:" ++ show x

class CENode a where
  eNodeInfo :: a -> ENodeInfo

instance CENode (CExpression ENodeInfo) where
  eNodeInfo (CCall _ _ x) = x
  eNodeInfo x = $abort $ unexp x 
