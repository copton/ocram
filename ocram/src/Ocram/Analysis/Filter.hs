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
import Language.C.Data.Node (NodeInfo, nodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis.CallGraph (start_functions, is_start, is_critical, critical_functions)
import Ocram.Analysis.Fgl (find_loop, edge_label)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Query (is_start_function', function_definition, is_blocking_function', function_parameters_cd)
import Ocram.Symbols (symbol)
import Ocram.Text (OcramError, new_error)
import Ocram.Util (fromJust_s, head_s, lookup_s)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.List as List

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
  | NestedFunction
  | CaseRange
  | PointerToCriticalFunction
  | NoThreads
  | ThreadNotBlocking
  | CriticalRecursion
  | InitializerList 
  | NoVarName
  | Ellipses
  deriving (Eq, Enum, Show)

errors :: [(ErrorCode, String)]
errors = [
    (NoParameterList, "function without parameter list")
  , (NoReturnType, "function without explicit return type")
  , (AssemblerCode, "transformation of assembler code is not supported")
  , (NestedFunction, "nested functions are not part of C99 and are thus not supported")
  , (CaseRange, "case ranges are not part of C99 and are thus not supported")
  , (PointerToCriticalFunction, "taking pointer from critical function")
  , (NoThreads, "at least one thread must be started")
  , (ThreadNotBlocking, "thread does not call any blocking functions")
  , (CriticalRecursion, "recursion of critical functions")
  , (InitializerList, "Sorry, initializer list in critical functions is not supported yet.")
  , (NoVarName, "Function parameters of blocking functions must have names.")
  , (Ellipses, "No ellipses for critical functions")
  ]

errorText :: ErrorCode -> String
errorText code = $fromJust_s $ List.lookup code errors

check_sanity :: CTranslUnit -> Either [OcramError] () -- {{{1
check_sanity ast = failOrPass $ everything (++) (mkQ [] saneExtDecls `extQ` saneStats `extQ` saneCompound) ast

saneExtDecls :: CExtDecl -> [OcramError]

saneExtDecls (CDeclExt cd@(CDecl ts _ ni))
  | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
  | is_blocking_function' cd = map (\p -> newError NoVarName Nothing (Just (nodeInfo p))) $ filter noVarName $ function_parameters_cd cd
  | otherwise = []

saneExtDecls (CFDefExt (CFunDef ts (CDeclr _ ps _ _ _) _ _ ni))
  | null ps = [newError NoParameterList Nothing (Just ni)]
  | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
  | otherwise = []

saneExtDecls (CAsmExt _ ni) = [newError AssemblerCode Nothing (Just ni)]

saneStats :: CStat -> [OcramError]
saneStats (CAsm _ ni) = [newError AssemblerCode Nothing (Just ni)]
saneStats (CCases _ _ _ ni) = [newError CaseRange Nothing (Just ni)]
saneStats _ = []

saneCompound :: CCompoundBlockItem NodeInfo -> [OcramError]
saneCompound (CNestedFunDef o) = [newError NestedFunction Nothing (Just (nodeInfo o))]
saneCompound _ = []

hasReturnType :: [CDeclSpec] -> Bool
hasReturnType = any isTypeSpec
  where
    isTypeSpec (CTypeSpec _) = True
    isTypeSpec _ = False

noVarName :: CDecl -> Bool
noVarName (CDecl _ [] _) = True
noVarName _ = False

check_constraints :: CTranslUnit -> CallGraph -> Either [OcramError] () -- {{{1
check_constraints ast cg = failOrPass $
     checkFunctionPointer cg ast
  ++ checkRecursion cg
  ++ checkStartFunctions cg ast
  ++ checkThreads cg
  ++ checkInitList cg ast
  ++ checkEllipses cg ast

checkEllipses :: CallGraph -> CTranslUnit -> [OcramError] -- {{{2
checkEllipses cg ast = everything (++) (mkQ [] ellipses) ast
  where
  ellipses :: CExtDecl -> [OcramError]
  ellipses (CDeclExt cd)
    | is_blocking_function' cd = everything (++) (mkQ [] ellipses') cd
    | otherwise = []
  ellipses (CFDefExt fd)
    | is_critical cg (symbol fd) = everything (++) (mkQ [] ellipses') fd
    | otherwise = []
  ellipses _ = []
  ellipses' :: CDerivedDeclr -> [OcramError]
  ellipses' x@(CFunDeclr (Right (_, True)) _ _) = [newError Ellipses Nothing (Just (nodeInfo x))]
  ellipses' _ = []

checkInitList :: CallGraph -> CTranslUnit -> [OcramError] -- {{{2
checkInitList cg ast = everything (++) (mkQ [] saneDecls) $ mapMaybe (function_definition ast) $ critical_functions cg
  where
  saneDecls (CDecl _ l ni)
    | any containsInitList l = [newError InitializerList Nothing (Just ni)]
    | otherwise = []
  containsInitList (_, Just (CInitList _ _), _) = True
  containsInitList _ = False

checkFunctionPointer :: CallGraph -> CTranslUnit -> [OcramError] -- {{{2
checkFunctionPointer cg ast = everything (++) (mkQ [] check) ast
  where
    check (CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni)
      | is_critical cg name = [newError PointerToCriticalFunction Nothing (Just ni)]
      | otherwise = []
    check _ = []

checkThreads :: CallGraph -> [OcramError] -- {{{2
checkThreads cg
  | null (start_functions cg) = [newError NoThreads Nothing Nothing]
  | otherwise = []

checkRecursion :: CallGraph -> [OcramError] -- {{{2
checkRecursion cg@(CallGraph gd gi) = mapMaybe (fmap (createRecError cg) . find_loop gd . $lookup_s gi) $ start_functions cg

createRecError :: CallGraph -> [G.Node] -> OcramError
createRecError (CallGraph gd _) call_stack =
  newError CriticalRecursion (Just errText) (Just location)
  where
    errText = List.intercalate " -> " $
      map (show . lblName . $fromJust_s . G.lab gd) call_stack
    (callee:caller:[]) = take 2 $ List.reverse call_stack 
    location = $head_s $ edge_label gd caller callee

checkStartFunctions :: CallGraph -> CTranslUnit -> [OcramError] -- {{{2
checkStartFunctions cg (CTranslUnit ds _) = foldr go [] ds
  where
  go (CFDefExt f@(CFunDef _ _ _ _ ni)) es
    | is_start_function' f && not (is_start cg (symbol f)) = 
      newError ThreadNotBlocking Nothing (Just ni) : es
    | otherwise = es
  go _ es = es

-- util {{{1
failOrPass :: [a] -> Either [a] ()
failOrPass [] = Right ()
failOrPass x = Left x
