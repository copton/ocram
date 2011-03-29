module Ocram.Filter.Util (
	Filter(Filter), performCheck, performFilter, Error, pass, append
) where

import Language.C.Data.Node (NodeInfo(NodeInfo, OnlyPos))
import Language.C.Data.Position (posFile, posRow)
import Ocram.Types (Result, AST)
import Data.Monoid (mconcat)

data Filter = Filter {
	getDescription :: String,
	getChecker :: AST -> [Error],
	getErrorCodes :: Int -> String
}

data Error = Error {
	getId :: Int,
	getLocation :: NodeInfo
}

performCheck :: Filter -> AST -> [Int]
performCheck f ast = map getId $ getChecker f ast

performFilter :: Filter -> AST -> Result AST
performFilter filter ast = case getChecker filter ast of
	[] -> return ast
	es -> fail $ showErrors filter es

append :: [[Error]] -> Int -> NodeInfo -> [Error]
append ess code location = Error code location : pass ess
pass :: [[Error]] -> [Error]
pass ess = mconcat ess

showErrors :: Filter -> [Error] -> String
showErrors filter es = "input programm failed the following " ++ (getDescription filter) ++ " checks:\n" ++ (showEnum filter $ zip [1..] es)

showEnum :: Filter -> [(Int, Error)] -> String
showEnum _ [] = ""
showEnum filter ((idx,e):es) = sidx ++ ") " ++ sloc ++ "\n" ++ serr ++ " (" ++ sid ++ ")\n\n" ++ next
	where
		sidx = show idx
		sloc = showLocation (getLocation e)
		sid = show (getId e)
		serr = getErrorCodes filter (getId e)
		next = showEnum filter es

showLocation (OnlyPos p _) = showPosition p
showLocation (NodeInfo p _ _ ) = showPosition p

showPosition p = "row: " ++ (show $ posRow p) ++ " in file: " ++ (posFile p)
