module Ocram.Debug
-- export {{{1
(
  VarMap,
  TLocation(..), ELocation(..), LocMap,
  format_debug_info
) where

-- import {{{1
import Ocram.Symbols (Symbol)
import Text.JSON (encodeStrict, toJSObject, toJSString, JSON(..), JSValue(JSArray, JSString))
import Data.Digest.OpenSSL.MD5 (md5sum)

import qualified Data.ByteString.Char8 as BS

data TLocation = -- {{{1
  TLocation {tlocRow :: Int, tlocCol :: Int, tlocLen :: Int}

instance Show TLocation where
  show (TLocation r c l) = show (r, c, l)

instance JSON TLocation where
  readJSON value = do
    (r, c, l) <- readJSON value
    return $ TLocation r c l

  showJSON (TLocation r c l) = showJSON (r, c, l)

data ELocation = -- {{{1
  ELocation {elocRow :: Int, elocCol :: Int, elocTidd :: Maybe Int}

instance Show ELocation where
  show (ELocation r c t) = show (r, c, t)

instance JSON ELocation where
  readJSON value = do
    (r, c, t) <- readJSON value
    return $ ELocation r c (if t == -1 then Nothing else Just t)

  showJSON (ELocation r c Nothing)  = showJSON (r, c, (-1)::Int)
  showJSON (ELocation r c (Just t)) = showJSON (r, c, t)  

type LocMap = [(TLocation, ELocation)] -- {{{1

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
