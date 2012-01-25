{-# LANGUAGE TemplateHaskell #-}
module Ocram.Analysis.Filter
-- exports {{{1
(
  check_sanity, check_constraints, ErrorCode(..)
) where

-- imports {{{1
import Data.Generics (mkQ, everything, extQ)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis.CallGraph (critical_functions, start_functions, is_critical)
import Ocram.Analysis.Fgl (find_loop, edge_label)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Query (function_definition)
import Ocram.Text (OcramError, new_error)
import Ocram.Types (Ast)
import Ocram.Util (fromJust_s, head_s, lookup_s)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.List as List
import qualified Data.Set as Set

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
  | NoReturnType
  | AssemblerCode
  | PointerToCriticalFunction
  | NoThreads
  | ThreadNotBlocking
  | CriticalRecursion
  | StructInitialization
  deriving (Eq, Enum, Show)

errors :: [(ErrorCode, String)]
errors = [
    (NoParameterList, "function without parameter list")
  , (NoReturnType, "function without explicit return type")
  , (AssemblerCode, "transformation of assembler code is not supported")
  , (PointerToCriticalFunction, "taking pointer from critical function")
  , (NoThreads, "at least one thread must be started")
  , (ThreadNotBlocking, "thread does not call any blocking functions")
  , (CriticalRecursion, "recursion of critical functions")
  , (StructInitialization, "Sorry, struct initialization is not supported yet.")
  ]

errorText :: ErrorCode -> String
errorText code = $fromJust_s $ List.lookup code errors

check_sanity :: Ast -> Either [OcramError] () -- {{{1
check_sanity ast = failOrPass $ everything (++) (mkQ [] saneExtDecls `extQ` saneStats `extQ` saneDecls) ast

saneExtDecls :: CExtDecl -> [OcramError]

saneExtDecls (CDeclExt (CDecl ts _ ni))
  | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
  | otherwise = []

saneExtDecls (CFDefExt (CFunDef ts (CDeclr _ ps _ _ _) _ _ ni))
  | null ps = [newError NoParameterList Nothing (Just ni)]
  | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
  | otherwise = []

saneExtDecls (CAsmExt _ ni) = [newError AssemblerCode Nothing (Just ni)]

saneStats :: CStat -> [OcramError]
saneStats (CAsm _ ni) = [newError AssemblerCode Nothing (Just ni)]
saneStats _ = []

saneDecls :: CDecl -> [OcramError]
saneDecls (CDecl _ l ni)
  | any containsInitList l = [newError StructInitialization Nothing (Just ni)]
  | otherwise = []
  where
    containsInitList (_, Just (CInitList _ _), _) = True
    containsInitList _ = False

hasReturnType :: [CDeclSpec] -> Bool
hasReturnType = any isTypeSpec
  where
    isTypeSpec (CTypeSpec _) = True
    isTypeSpec _ = False

check_constraints :: Ast -> CallGraph -> Either [OcramError] () -- {{{1
check_constraints ast cg = failOrPass $
     checkFunctionPointer cg ast
  ++ checkThreads cg
  ++ checkRecursion cg
  ++ checkStartFunctions cg ast

checkFunctionPointer :: CallGraph -> Ast -> [OcramError]
checkFunctionPointer cg ast = everything (++) (mkQ [] check) ast
  where
    cf = critical_functions cg
    check (CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni)
      | name `Set.member` cf = [newError PointerToCriticalFunction Nothing (Just ni)]
      | otherwise = []
    check _ = []

checkThreads :: CallGraph -> [OcramError]
checkThreads cg
  | Set.null (start_functions cg) = [newError NoThreads Nothing Nothing]
  | otherwise = []

checkRecursion :: CallGraph -> [OcramError]
checkRecursion cg@(CallGraph gd gi) = mapMaybe (fmap (createRecError cg) . find_loop gd . $lookup_s gi) $ Set.toList $ start_functions cg

createRecError :: CallGraph -> [G.Node] -> OcramError
createRecError (CallGraph gd _) call_stack =
  newError CriticalRecursion (Just errText) (Just location)
  where
    errText = List.intercalate " -> " $
      map (show . lblName . $fromJust_s . G.lab gd) call_stack
    (callee:caller:[]) = take 2 $ List.reverse call_stack 
    location = $head_s $ edge_label gd caller callee

checkStartFunctions :: CallGraph -> Ast -> [OcramError]
checkStartFunctions cg ast =
  let
    sf = start_functions cg
    failures = Set.filter (not . is_critical cg) sf
    errs = Set.map (toError . getLocation) failures
    getLocation name = (\(CFunDef _ _ _ _ x) -> x) $ $fromJust_s $ function_definition ast name
    toError location = newError ThreadNotBlocking Nothing (Just location)
  in
    Set.toList errs

-- util {{{1
failOrPass :: [a] -> Either [a] ()
failOrPass [] = Right ()
failOrPass x = Left x
