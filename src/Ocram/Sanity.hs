module Ocram.Sanity (
	checkSanity, getErrorIds
) where

import Ocram.Types (Result, AST)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Ocram.Util (Location(Location))
import Language.C.Syntax.AST 
import Data.Monoid (mconcat)

checkSanity :: AST -> Result AST
checkSanity ast = case check ast of
	[] -> return ast
	es -> fail $ showErrors es

getErrorIds :: AST -> [Int]
getErrorIds ast = map unpackError $ check ast
	where
		unpackError (Error (ErrorId id) _) = id

check :: AST -> [Error]
check ast = snd $ traverseCTranslUnit ast emptyDownState

newtype ErrorId = ErrorId Int
instance Show ErrorId where
	show (ErrorId 1) = "function without parameter list"
	show (ErrorId id) = error $ "unknown error id " ++ show id

data Error = Error ErrorId Location
instance Show Error where
	show (Error eid@(ErrorId id) location) = show eid ++ " (" ++ show id ++ ")\n" ++ show location

showEnum :: [(Int, Error)] -> String
showEnum [] = ""
showEnum ((idx, e):es) = show  idx ++ ") " ++ show e ++ "\n\n" ++ showEnum es

showErrors :: [Error] -> String
showErrors es = "input program failed the following sanity checks" ++ "\n" ++ (showEnum $ zip [1..] es)


type UpState = [Error]

append es id location = Error (ErrorId id) (Location location) : pass es
pass es = mconcat es

instance UpVisitor EmptyDownState UpState where
	upCExtDecl (CFDefExt (CFunDef _ (CDeclr _ [] _ _ _) _ _ ni)) _ us = append us 1 ni
	upCExtDecl _ _ us = pass us
