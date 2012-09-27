{-# LANGUAGE TemplateHaskell #-}
module Ocram.Analysis.Filter
-- exports {{{1
(
  check_sanity, check_constraints, ErrorCode(..)
) where

-- imports {{{1
import Control.Monad (guard)
import Data.Data (Data)
import Data.Generics (mkQ, everything, extQ)
import Data.List (findIndices)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (NodeInfo, nodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis.CallGraph (start_functions, is_start, is_critical)
import Ocram.Analysis.Fgl (find_loop, edge_label)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Query (is_start_function, is_blocking_function, function_parameters_cd)
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
  | StatExpression
  | GotoPtr
  | BuiltinExpr
  | RangeDesignator
  | IllFormedSwitch
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
  , (StatExpression, "GNU C compound statement as expressions are not supported")
  , (GotoPtr, "Computed gotos are not part of C99 and are thus not supported")
  , (BuiltinExpr, "GNU C builtin expressions are not supported")
  , (RangeDesignator, "GNU C array range designators are not supported")
  , (IllFormedSwitch, "ill-formed switch statement")
  ]

errorText :: ErrorCode -> String
errorText code = $fromJust_s $ List.lookup code errors

check_sanity :: CTranslUnit -> Either [OcramError] () -- {{{1
check_sanity ast = failOrPass $ everything (++) (mkQ [] saneExtDecls `extQ` saneStmt) ast
  where
    saneExtDecls (CDeclExt cd@(CDecl ts _ ni))
      | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
      | is_blocking_function cd = map (\p -> newError NoVarName Nothing (Just (nodeInfo p))) $ filter noVarName $ function_parameters_cd cd
      | otherwise = []

    saneExtDecls (CFDefExt (CFunDef ts (CDeclr _ ps _ _ _) _ _ ni))
      | null ps = [newError NoParameterList Nothing (Just ni)]
      | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
      | otherwise = []

    saneExtDecls _ = []

    -- The restrictions on switch statements are not strictly required, but
    -- make the implementation of desugar_control_structures easier.
    -- Furthermore, we see little sense in using these cases deliberately.
    saneStmt (CSwitch _ body ni) =
      let
        isCase (CCase _ _ _)         = True
        isCase _                     = False
        isCases (CCases _ _ _ _)     = True
        isCases _                    = False
        isDefault (CDefault _ _)     = True
        isDefault _                  = False
        extract (CBlockStmt s)       = Just s
        extract _                    = Nothing        

        test = let flt = or . sequence [isCase, isCases, isDefault] in do
          -- Enforce a switch "code block"...
          (CCompound _ items _) <- Just body
          -- ...which must start with a statement...
          ((CBlockStmt stmt):_) <- Just items
          -- ...that has to be a case(es) or a default statement.
          guard $ flt stmt

          -- And finally make sure that there is at most one default
          -- statement and that no case(es) statement follows.
          let
             allcases = filter flt $ mapMaybe extract items
             dfltcases = findIndices isDefault allcases
          guard (null dfltcases || head dfltcases == (length allcases) - 1) 
      in case test of
        Nothing -> [newError IllFormedSwitch Nothing (Just ni)]
        Just () -> []

    saneStmt _ = []

    hasReturnType = any isTypeSpec
      where
        isTypeSpec (CTypeSpec _) = True
        isTypeSpec _ = False

    noVarName (CDecl _ [] _) = True
    noVarName _ = False

check_constraints :: CTranslUnit -> CallGraph -> Either [OcramError] () -- {{{1
check_constraints ast cg = failOrPass $
     checkFunctionPointer cg ast
  ++ checkRecursion cg
  ++ checkStartFunctions cg ast
  ++ checkThreads cg
  ++ checkFeatures cg ast

checkFunctionPointer :: CallGraph -> CTranslUnit -> [OcramError] -- {{{2
checkFunctionPointer cg ast = everything (++) (mkQ [] check) ast
  where
    check (CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni)
      | is_critical cg name = [newError PointerToCriticalFunction Nothing (Just ni)]
      | otherwise = []
    check _ = []

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
    | is_start_function f && not (is_start cg (symbol f)) = 
      newError ThreadNotBlocking Nothing (Just ni) : es
    | otherwise = es
  go _ es = es

checkThreads :: CallGraph -> [OcramError] -- {{{2
checkThreads cg
  | null (start_functions cg) = [newError NoThreads Nothing Nothing]
  | otherwise = []

checkFeatures :: CallGraph -> CTranslUnit -> [OcramError] -- {{{2
checkFeatures cg (CTranslUnit ds _) = concatMap filt ds
  where
    filt (CDeclExt cd)
      | is_blocking_function cd = scan cd
      | otherwise = []
    filt (CFDefExt fd)
      | is_critical cg (symbol fd) = scan fd
      | otherwise = []
    filt _ = []

    scan :: Data a => a -> [OcramError]
    scan = everything (++) (mkQ [] scanDerivedDeclr `extQ` scanDecl `extQ` scanStat `extQ` scanBlockItem `extQ` scanExpr `extQ` scanPartDesig)

    scanDerivedDeclr :: CDerivedDeclr -> [OcramError]
    scanDerivedDeclr x@(CFunDeclr (Right (_, True)) _ _) =
      [newError Ellipses Nothing (Just (nodeInfo x))]
    scanDerivedDeclr _ = []

    scanDecl (CDecl _ l ni)
      | any containsInitList l = [newError InitializerList Nothing (Just ni)]
      | otherwise = []
      where
        containsInitList (_, Just (CInitList _ _), _) = True
        containsInitList _ = False

    scanStat (CAsm _ ni) =
      [newError AssemblerCode Nothing (Just ni)]
    scanStat (CCases _ _ _ ni) =
      [newError CaseRange Nothing (Just ni)]
    scanStat (CGotoPtr _ ni) =
      [newError GotoPtr Nothing (Just ni)]
    scanStat _ = []

    scanBlockItem :: CBlockItem -> [OcramError]
    scanBlockItem (CNestedFunDef o) =
      [newError NestedFunction Nothing (Just (nodeInfo o))]
    scanBlockItem _ = []

    scanExpr (CStatExpr _ ni) =
      [newError StatExpression Nothing (Just ni)]
    scanExpr o@(CBuiltinExpr _) =
      [newError BuiltinExpr Nothing (Just (nodeInfo o))]
    scanExpr _ = []

    scanPartDesig (CRangeDesig _ _ ni) =
      [newError RangeDesignator Nothing (Just ni)]
    scanPartDesig _ = []
    
-- util {{{1
failOrPass :: [a] -> Either [a] ()
failOrPass [] = Right ()
failOrPass x = Left x
