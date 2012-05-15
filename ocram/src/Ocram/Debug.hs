module Ocram.Debug
-- export {{{1
(
  VarMap,
  Location(..), LocMap,
  format_debug_info
) where

-- import {{{1
import Ocram.Symbols (Symbol)
import Text.JSON (encodeStrict, toJSObject, JSON(..))

data Location = -- {{{1
  Location {locRow :: Int, locCol :: Int}

instance Show Location where
  show (Location r c) = "(" ++ show r ++ ", " ++ show c ++ ")"

type LocMap = [(Location, Location)]

instance JSON Location where
  readJSON value = do
    (r, c) <- readJSON value
    return $ Location r c

  showJSON (Location r c) = showJSON (r, c)

type Variable = Symbol -- {{{1
type VarMap = [(Variable, Variable)]


format_debug_info :: LocMap -> VarMap -> String
format_debug_info lm vm = encodeStrict $ toJSObject [("varmap", showJSON vm), ("locmap", showJSON lm)]
