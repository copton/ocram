module Ocram.Filter.Sanity (
	checkSanity, getErrorCodes
) where

import Ocram.Filter.Util (Error, pass, append, Filter(Filter), performFilter, performCheck)
import Ocram.Types (AST)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Language.C.Syntax.AST 
import Data.Monoid (mconcat)

checker :: AST -> [Error]
checker ast = snd $ traverseCTranslUnit ast emptyDownState

errorCodes :: Int -> String
errorCodes 1 = "function without parameter list"
errorCodes x = error $ "unknown error code " ++ show x

descriptor = Filter "sanity" checker errorCodes

checkSanity = performFilter descriptor
getErrorCodes = performCheck descriptor

type UpState = [Error]

instance UpVisitor EmptyDownState UpState where
	upCExtDecl (CFDefExt (CFunDef _ (CDeclr _ [] _ _ _) _ _ ni)) _ us = append us 1 ni
	upCExtDecl _ _ us = pass us
