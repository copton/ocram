{-# LANGUAGE DeriveDataTypeable #-}
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
import Ocram.Symbols (Symbol)
import Ocram.Util (abort, unexp)
import Text.JSON (encodeStrict, toJSObject, toJSString, JSON(..), JSValue(JSArray, JSString))

import qualified Data.ByteString.Char8 as BS

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

format_debug_info :: BS.ByteString -> BS.ByteString -> LocMap -> VarMap -> BS.ByteString -- {{{1
format_debug_info tcode ecode lm vm =
  let
    cs = JSString . toJSString . md5sum
    tcodeChecksum = cs tcode
    ecodeChecksum = cs ecode
    checksum = JSArray [tcodeChecksum, ecodeChecksum]
  in
    (BS.pack . encodeStrict . toJSObject) [
      ("checksum", checksum),
      ("varmap", showJSON vm),
      ("locmap", showJSON lm)
    ]

class CENode a where
  eNodeInfo :: a -> ENodeInfo

instance CENode (CExpression ENodeInfo) where
  eNodeInfo (CCall _ _ x) = x
  eNodeInfo x = $abort $ unexp x 
