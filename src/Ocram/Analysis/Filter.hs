module Ocram.Analysis.Filter
--- exports {{{1
(
  check_sanity, check_constraints
) where

-- imports {{{1
import Data.Maybe (fromMaybe, fromJust)
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))
import Ocram.Analysis.Functions (CallGraph, critical_functions, start_functions, CriticalFunctions, is_critical, get_function_definition)
import Ocram.Text (OcramError, new_error)
import Ocram.Types (Ast)
import Ocram.Visitor (traverseCTranslUnit, emptyDownState, EmptyDownState, DownVisitor, UpVisitor(upCExtDecl, upCExpr), ListVisitor)
import qualified Data.Set as Set


-- errors {{{1
newError :: Int -> Maybe String -> Maybe NodeInfo -> OcramError 
newError code extraWhat where_ =
  let
    extraWhat' = fromMaybe "" extraWhat
    what = errorText code ++ extraWhat'
  in
		new_error code what where_

errorText :: Int -> String
errorText 1 = "function without parameter list"
errorText 2 = "taking pointer from critical function"
errorText 3 = "at least one thread must be started"
errorText 4 = "thread does not call any blocking functions."
errorText x = error $ "unknown error code " ++ show x


-- check_sanity :: Ast -> Either OcramError () {{{1
check_sanity :: Ast -> Either [OcramError] ()
check_sanity ast = failOrPass $ snd $ traverseCTranslUnit ast emptyDownState

type CsUpState = [OcramError]

instance UpVisitor EmptyDownState CsUpState where
	upCExtDecl o@(CFDefExt (CFunDef _ (CDeclr _ [] _ _ _) _ _ ni)) _ u =
		(o, (newError 1 Nothing (Just ni)) : u)
	upCExtDecl o _ u = (o, u)

instance ListVisitor EmptyDownState CsUpState


-- check_constraints :: Ast -> CallGraph -> Either [OcramError] () {{{1
check_constraints :: Ast -> CallGraph -> Either [OcramError] ()
check_constraints ast cg = failOrPass $
		 checkFunctionPointer ast cg
	++ checkThreads cg
	++ checkRecursion cg
	++ checkStartFunctions cg


checkFunctionPointer :: Ast -> CallGraph -> [OcramError]
checkFunctionPointer ast cg =
	let
		cf = critical_functions cg
	in
		snd $ traverseCTranslUnit ast $ FpDownState cf


newtype FpDownState = FpDownState CriticalFunctions

instance DownVisitor FpDownState

type FpUpState = [OcramError]

instance UpVisitor FpDownState FpUpState where
	upCExpr o@(CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni) (FpDownState cf) u
		| name `Set.member` cf = (o, newError 2 Nothing (Just ni) : u)
		| otherwise = (o, u)
	upCExpr o _ u = (o, u)

instance ListVisitor FpDownState FpUpState


checkThreads :: CallGraph -> [OcramError]
checkThreads cg
	| Set.null (start_functions cg) = [newError 3 Nothing Nothing]
	| otherwise = []


checkRecursion :: CallGraph -> [OcramError]
checkRecursion = undefined


checkStartFunctions :: CallGraph -> [OcramError]
checkStartFunctions cg =
	let
		sf = start_functions cg
		failures = Set.filter (not . (is_critical cg)) sf
		errors = Set.map (toError . getLocation) failures
		getLocation name = (\(CFunDef _ _ _ _ x) -> x) $ fromJust $ get_function_definition cg name
		toError location = newError 4 Nothing (Just location)
	in
		Set.toList errors


-- util {{{1
failOrPass :: [a] -> Either [a] ()
failOrPass [] = Right ()
failOrPass x = Left x
