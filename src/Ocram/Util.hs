module Ocram.Util (
	Location(..)
) where

import Language.C.Data.Node (NodeInfo(NodeInfo, OnlyPos))
import Language.C.Data.Position (posFile, posRow)

newtype Location = Location NodeInfo
instance Show Location where
	show (Location (OnlyPos p _)) = showPosition p
	show (Location (NodeInfo p _ _ )) = showPosition p
showPosition p = "row: " ++ (show $ posRow p) ++ " in file: " ++ (posFile p)
