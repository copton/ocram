module Ocram.Sanity (
	checkSanity
) where

import Ocram.Types (Result, AST)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Language.C.Syntax.AST 
import Language.C.Data.Node (NodeInfo(NodeInfo, OnlyPos))
import Language.C.Data.Position (posFile, posRow)
import Data.Monoid (mconcat)

checkSanity :: AST -> Result AST
checkSanity ast = case traverseCTranslUnit ast emptyDownState of
	(_, []) -> return ast
	(_, es) -> fail $ showErrors es

newtype ErrorId = ErrorId {getErrorId :: Int}
instance Show ErrorId where
	show (ErrorId 1) = "function without parameter list"
	show (ErrorId id) = error $ "unknown error id " ++ show id

newtype Location = Location NodeInfo
instance Show Location where
	show (Location (OnlyPos p _)) = showPosition p
	show (Location (NodeInfo p _ _ )) = showPosition p
showPosition p = "row: " ++ (show $ posRow p) ++ " in file: " ++ (posFile p)

data Error = Error ErrorId Location
instance Show Error where
	show (Error eid@(ErrorId id) location) = show eid ++ " (" ++ show id ++ ")\n" ++ show location

showEnum :: [(Int, Error)] -> String
showEnum [] = ""
showEnum ((idx, e):es) = show  idx ++ ") " ++ show e ++ "\n\n" ++ showEnum es

showErrors :: [Error] -> String
showErrors es = "input program failed the following sanity checks" ++ "\n" ++ (showEnum $ zip [1..] es)


type UpState = [Error]

append :: [[Error]] -> Int -> NodeInfo -> [Error]
append es id location = Error (ErrorId id) (Location location) : pass es

pass :: [[Error]] -> [Error]
pass es = mconcat es

instance UpVisitor EmptyDownState UpState where
	upCExtDecl (CFDefExt (CFunDef _ (CDeclr _ [] _ _ _) _ _ ni)) _ us = append us 1 ni
	upCExtDecl _ _ us = pass us
