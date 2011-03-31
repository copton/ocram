module Ocram.Filter.Sanity (
	checkSanity, getErrorCodes
) where

import Ocram.Filter.Util (Error, pass, append, Filter(Filter), performFilter, performCheck)
import Ocram.Types (Result, Ast, RawAst, SaneAst(SaneAst), getAst)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Language.C.Syntax.AST 
import Data.Monoid (mconcat)

checkSanity :: RawAst -> Result SaneAst
checkSanity raw_ast = fmap SaneAst $ performFilter descriptor $ getAst raw_ast
getErrorCodes raw_ast = performCheck descriptor $ getAst raw_ast

checker :: Ast -> [Error]
checker ast = snd $ traverseCTranslUnit ast emptyDownState

errorCodes :: Int -> String
errorCodes 1 = "function without parameter list"
errorCodes x = error $ "unknown error code " ++ show x

descriptor = Filter "sanity" checker errorCodes


type UpState = [Error]

instance UpVisitor EmptyDownState UpState where
	upCExtDecl (CFDefExt (CFunDef _ (CDeclr _ [] _ _ _) _ _ ni)) _ us = append us 1 ni
	upCExtDecl _ _ us = pass us
