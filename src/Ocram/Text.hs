module Ocram.Text 
-- export {{{1
(
	show_errors, OcramError(..), new_error
) where 

-- import {{{1
import Language.C.Data.Node (NodeInfo(OnlyPos, NodeInfo), isUndefNode, undefNode)
import Language.C.Data.Position (Position, posRow, posFile)
import Data.List (intersperse)

-- data OcramError = OcramError { {{{1
data OcramError = OcramError {
		errCode :: Int
	, errWhat :: String 
	, errWhere :: NodeInfo
} deriving (Ord, Eq)

-- new_error :: Int -> String -> Maybe NodeInfo {{{1
new_error :: Int -> String -> Maybe NodeInfo -> OcramError
new_error code what Nothing = OcramError code what undefNode
new_error code what (Just where_) = OcramError code what where_

-- show_errors :: String -> [OcramError] -> String {{{1
show_errors :: String -> [OcramError] -> String
show_errors module_ es = show (length es) ++ " issue(s) reported by the '" ++ module_ ++ "' module:\n" ++ errors
	where
		errors = concat $ intersperse "\n" $ map showError (zip [1..] es)

showError :: (Int, OcramError) -> String
showError (count, e) = "### Error: " ++ show count ++ ") (" ++ scode ++ ") " ++ sloc ++ "\n" ++ serr
	where
		scode = show $ errCode e
		sloc = showLocation $ errWhere e
		serr = errWhat e

showLocation :: NodeInfo -> String
showLocation loc
	| isUndefNode loc = "<<undefined location>>"
	| otherwise = case loc of
		(OnlyPos p _) -> showPosition p
		(NodeInfo p _ _ ) -> showPosition p

showPosition :: Position -> String
showPosition p = "row: " ++ (show $ posRow p) ++ " in file: " ++ (posFile p)
