module Ocram.Debug
-- export {{{1
(
  VarMap,
  Location(..), LocMap,
  format_debug_info
) where

-- import {{{1
import Ocram.Symbols (Symbol)
import Text.JSON (encodeStrict, toJSObject, toJSString, JSON(..), JSValue(JSArray, JSString))
import Data.Digest.OpenSSL.MD5 (md5sum)

import qualified Data.ByteString.Char8 as BS

data Location = -- {{{1
  Location {locRow :: Int, locCol :: Int, locLen :: Int}

instance Show Location where
  show (Location r c l) = show (r, c, l)

type LocMap = [(Location, Location)]

instance JSON Location where
  readJSON value = do
    (r, c, l) <- readJSON value
    return $ Location r c l

  showJSON (Location r c l) = showJSON (r, c, l)

type Variable = Symbol -- {{{1
type VarMap = [(Variable, Variable)]


format_debug_info :: BS.ByteString -> String -> LocMap -> VarMap -> String
format_debug_info tcode ecode lm vm =
  let
    cs = JSString . toJSString . md5sum
    tcodeChecksum = cs tcode
    ecodeChecksum = cs $ BS.pack ecode
    checksum = JSArray [tcodeChecksum, ecodeChecksum]
  in
    encodeStrict $ toJSObject [
      ("checksum", checksum),
      ("varmap", showJSON vm),
      ("locmap", showJSON lm)
    ]
