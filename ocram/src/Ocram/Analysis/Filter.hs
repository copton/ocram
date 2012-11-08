{-# LANGUAGE TemplateHaskell #-}
module Ocram.Analysis.Filter
-- exports {{{1
(
  global_constraints, critical_constraints, ErrorCode(..)
) where

-- imports {{{1
import Control.Monad (guard)
import Data.Generics (mkQ, everything, extQ)
import Data.List (findIndices)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (NodeInfo, nodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis.CallGraph (start_functions, is_critical)
import Ocram.Analysis.Fgl (find_loop, edge_label)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Query (is_start_function, is_blocking_function, function_parameters_cd, return_type_fd, function_parameters_fd)
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

data ErrorCode = -- {{{2
    AssemblerCode
  | BuiltinExpr
  | CaseRange
  | CriticalRecursion
  | Ellipses
  | GotoPtr
  | IllFormedSwitch
  | ArrayInitializerList 
  | NestedFunction
  | NoParameterList
  | NoReturnType
  | NoThreads
  | NoVarName
  | OldStyleParams
  | PointerToCriticalFunction
  | RangeDesignator
  | StartFunctionSignature
  | StatExpression
  | ThreadLocalStorage
  | ThreadNotBlocking
  deriving (Eq, Enum, Show)

errorText :: ErrorCode -> String -- {{{3
errorText AssemblerCode =
  "transformation of assembler code is not supported"
errorText BuiltinExpr =
  "GNU C builtin expressions are not supported"
errorText CaseRange =
  "case ranges are not part of C99 and are thus not supported"
errorText CriticalRecursion =
  "recursion of critical functions"
errorText Ellipses =
  "No ellipses for critical functions"
errorText GotoPtr =
  "Computed gotos are not part of C99 and are thus not supported"
errorText IllFormedSwitch =
  "ill-formed switch statement"
errorText ArrayInitializerList =
  "Sorry, initializer list in critical functions is not supported yet."
errorText NestedFunction =
  "nested functions are not part of C99 and are thus not supported"
errorText NoParameterList =
  "function without parameter list"
errorText NoReturnType =
  "function without explicit return type"
errorText NoThreads =
  "at least one thread must be started"
errorText NoVarName =
  "Function parameters of blocking functions must have names."
errorText OldStyleParams =
  "Old style parameter declarations are not supported."
errorText PointerToCriticalFunction =
  "taking pointer from critical function"
errorText RangeDesignator =
  "GNU C array range designators are not supported"
errorText StartFunctionSignature =
  "thread start functions must have the following signature 'void name()'"
errorText StatExpression =
  "GNU C compound statement as expressions are not supported"
errorText ThreadLocalStorage =
  "GNU C thread local storage is not supported"
errorText ThreadNotBlocking =
  "thread does not call any blocking functions"

global_constraints :: CTranslUnit -> Either [OcramError] () -- {{{1
global_constraints ast = failOrPass $ everything (++) (mkQ [] scanBlockItem `extQ` scanStorageSpec) ast
  where
    scanBlockItem :: CBlockItem -> [OcramError]
    scanBlockItem (CNestedFunDef o) =
      [newError NestedFunction Nothing (Just (nodeInfo o))]
    scanBlockItem _ = []

    scanStorageSpec :: CStorageSpec -> [OcramError]
    scanStorageSpec (CThread ni) =
      [newError ThreadLocalStorage Nothing (Just ni)]
    scanStorageSpec _ = []

critical_constraints :: CTranslUnit -> CallGraph -> Either [OcramError] () -- {{{1
critical_constraints ast cg = failOrPass $
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
    | not (is_start_function f)           = es
    | not (is_critical cg (symbol f))     = 
        newError ThreadNotBlocking Nothing (Just ni) : es
    | (not . isVoid . fst . return_type_fd) f =
        newError StartFunctionSignature Nothing (Just ni) : es
    | (not . null . function_parameters_fd) f =
        newError StartFunctionSignature Nothing (Just ni) : es
    | otherwise                           = es
    where isVoid (CVoidType _) = True
          isVoid _             = False
  go _ es = es

checkThreads :: CallGraph -> [OcramError] -- {{{2
checkThreads cg
  | null (start_functions cg) = [newError NoThreads Nothing Nothing]
  | otherwise = []

checkFeatures :: CallGraph -> CTranslUnit -> [OcramError] -- {{{2
checkFeatures cg (CTranslUnit ds _) = concatMap filt ds
  where
    filt (CDeclExt cd@(CDecl ts _ ni))
      | not (is_blocking_function cd) = []
      | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
      | otherwise = noVarNames ++ everything (++) (mkQ [] scanDerivedDeclr) cd
      where
      noVarNames = mapMaybe noVarName $ function_parameters_cd cd
      noVarName p@(CDecl _ [] _) = Just $ newError NoVarName Nothing (Just (nodeInfo p))
      noVarName _                = Nothing
        
    filt (CFDefExt fd@(CFunDef ts (CDeclr _ ps _ _ _) _ _ ni))
      | not (is_critical cg (symbol fd)) = []
      | null ps = [newError NoParameterList Nothing (Just ni)]
      | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
      | otherwise = scan fd

    filt _ = []

    hasReturnType = any isTypeSpec
      where
        isTypeSpec (CTypeSpec _) = True
        isTypeSpec _ = False

    scan :: CFunDef -> [OcramError]
    scan = everything (++) (mkQ [] 
                                   scanDerivedDeclr
                            `extQ` scanDecl
                            `extQ` scanStat
                            `extQ` scanExpr
                            `extQ` scanPartDesig
                           )

    scanDerivedDeclr :: CDerivedDeclr -> [OcramError]
    scanDerivedDeclr x@(CFunDeclr (Right (_, True)) _ _) =
      [newError Ellipses Nothing (Just (nodeInfo x))]
    scanDerivedDeclr x@(CFunDeclr (Left _) _ _) =
      [newError OldStyleParams Nothing (Just (nodeInfo x))]
    scanDerivedDeclr _ = []

    scanDecl (CDecl _ l ni)
      | any containsArrayInitList l =
          [newError ArrayInitializerList Nothing (Just ni)]
      | otherwise = []
      where
        containsArrayInitList (Just declr, Just (CInitList _ _), _) =
          containsArrayDeclr declr
        containsArrayInitList _ = False

        containsArrayDeclr (CDeclr _ dds _ _ _) = any isArrayDeclr dds
      
        isArrayDeclr (CArrDeclr _ _ _) = True
        isArrayDeclr _                 = False

    scanStat (CAsm _ ni) =
      [newError AssemblerCode Nothing (Just ni)]
    scanStat (CCases _ _ _ ni) =
      [newError CaseRange Nothing (Just ni)]
    scanStat (CGotoPtr _ ni) =
      [newError GotoPtr Nothing (Just ni)]
    
    -- The restrictions on switch statements are not strictly required, but
    -- make the implementation of desugar_control_structures easier.
    -- Furthermore, we see little sense in using these cases deliberately.
    scanStat (CSwitch _ body ni) =
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

    scanStat _ = []

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
