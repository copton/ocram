module Ocram.Analysis.Filter
-- exports {{{1
(
  check_sanity, check_constraints, ErrorCode(..)
) where

-- imports {{{1
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))
import Ocram.Analysis.CallGraph (critical_functions, start_functions, CriticalFunctions, is_critical, get_function_definition)
import Ocram.Analysis.Fgl (find_loop, edge_label)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Text (OcramError, new_error)
import Ocram.Types (Ast)
import Ocram.Util (trd, tmap)
import Ocram.Visitor (traverseCTranslUnit, emptyDownState, EmptyDownState, DownVisitor, UpVisitor(upCExtDecl, upCExpr), ListVisitor)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Graph.Inductive.Graph as G

-- errors {{{1
newError :: ErrorCode -> Maybe String -> Maybe NodeInfo -> OcramError 
newError code extraWhat where_ =
  let
    extraWhat' = fromMaybe "" extraWhat
    what = errorText code ++ extraWhat'
  in
    new_error (fromEnum code) what where_

data ErrorCode =
    NoParameterList
  | PointerToCriticalFunction
  | NoThreads
  | ThreadNotBlocking
  | CriticalRecursion
  deriving (Eq, Enum, Show)

errors :: [(ErrorCode, String)]
errors = [
    (NoParameterList, "function without parameter list")
  , (PointerToCriticalFunction, "taking pointer from critical function")
  , (NoThreads, "at least one thread must be started")
  , (ThreadNotBlocking, "thread does not call any blocking functions")
  , (CriticalRecursion, "recursion of critical functions")
  ]

errorText :: ErrorCode -> String
errorText code = fromJust $ List.lookup code errors

-- check_sanity :: Ast -> Either OcramError () {{{1
check_sanity :: Ast -> Either [OcramError] ()
check_sanity ast = failOrPass $ snd $ traverseCTranslUnit ast emptyDownState

type CsUpState = [OcramError]

instance UpVisitor EmptyDownState CsUpState where
  upCExtDecl o@(CFDefExt (CFunDef _ (CDeclr _ [] _ _ _) _ _ ni)) _ u =
    (o, (newError NoParameterList Nothing (Just ni)) : u)
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
    | name `Set.member` cf = (o, newError PointerToCriticalFunction Nothing (Just ni) : u)
    | otherwise = (o, u)
  upCExpr o _ u = (o, u)

instance ListVisitor FpDownState FpUpState


checkThreads :: CallGraph -> [OcramError]
checkThreads cg
  | Set.null (start_functions cg) = [newError NoThreads Nothing Nothing]
  | otherwise = []


checkRecursion :: CallGraph -> [OcramError]
checkRecursion cg@(CallGraph gd gi) = map (createRecError cg) $ catMaybes $ map (find_loop gd) $ map (gi Map.!) $ Set.toList $ start_functions cg


createRecError :: CallGraph -> [G.Node] -> OcramError
createRecError (CallGraph gd gi) call_stack =
  newError CriticalRecursion (Just errText) (Just location)
  where
    errText = concat $ List.intersperse " -> " $
      map (show . lblName . fromJust . G.lab gd) call_stack
    (callee:caller:[]) = take 2 $ List.reverse call_stack 
    location = head $ edge_label gd caller callee
      

checkStartFunctions :: CallGraph -> [OcramError]
checkStartFunctions cg =
  let
    sf = start_functions cg
    failures = Set.filter (not . (is_critical cg)) sf
    errors = Set.map (toError . getLocation) failures
    getLocation name = (\(CFunDef _ _ _ _ x) -> x) $ fromJust $ get_function_definition cg name
    toError location = newError ThreadNotBlocking Nothing (Just location)
  in
    Set.toList errors


-- util {{{1
failOrPass :: [a] -> Either [a] ()
failOrPass [] = Right ()
failOrPass x = Left x
