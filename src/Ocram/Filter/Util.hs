module Ocram.Filter.Util 
--- exports {{{1
(
	Filter(Filter), performFilter, Error(Error)
) where

-- imports {{{1
import Ocram.Types

import Language.C.Data.Node (NodeInfo(NodeInfo, OnlyPos))
import Language.C.Data.Position (posFile, posRow)

import Control.Monad.Error (throwError)

-- data Filter a = Filter { {{{1
data Filter a = Filter {
	getDescription :: String,
	getChecker :: Ast -> [Error a],
	getErrorDescriptions :: a -> String,
	getErrorIds :: a -> Int
}

-- data Error a = Error { {{{1
data Error a = Error {
	getError :: a,
	getLocation :: NodeInfo
}

-- performFilter :: Filter a -> Ast -> ER () {{{1
performFilter :: Filter a -> Ast -> ER ()
performFilter filter ast = case getChecker filter ast of
	[] -> return ()
	es -> throwError $ showErrors filter es

showErrors :: Filter a -> [Error a] -> String
showErrors filter es = "input programm failed the following " ++ (getDescription filter) ++ " checks:\n" ++ (showEnum filter $ zip [1..] es)

showEnum :: Filter a -> [(Int, Error a)] -> String
showEnum _ [] = ""
showEnum filter ((idx,e):es) = sidx ++ ") (" ++ sid ++ ") " ++ sloc ++ "\n" ++ serr ++ "\n\n" ++ next
	where
		sidx = show idx
		sloc = showLocation (getLocation e)
		sid = show $ (getErrorIds filter) $ getError e
		serr = getErrorDescriptions filter (getError e)
		next = showEnum filter es

showLocation (OnlyPos p _) = showPosition p
showLocation (NodeInfo p _ _ ) = showPosition p

showPosition p = "row: " ++ (show $ posRow p) ++ " in file: " ++ (posFile p)
