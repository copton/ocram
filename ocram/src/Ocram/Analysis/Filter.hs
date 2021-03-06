{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Ocram.Analysis.Filter
-- exports {{{1
(
  global_constraints, critical_constraints, ErrorCode(..)
) where

-- imports {{{1
import Control.Monad (guard)
import Data.Generics (mkQ, everything, extQ)
import Data.Data (Data)
import Data.List (findIndices)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (NodeInfo, nodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis.CallGraph (start_functions, is_critical)
import Ocram.Analysis.Fgl (find_loop, edge_label)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Names (ecPrefix, blockingAttr, startAttr)
import Ocram.Query (is_start_function, is_blocking_function, function_parameters_cd, return_type_fd, function_parameters_fd)
import Ocram.Symbols (symbol)
import Ocram.Text (OcramError, new_error)
import Ocram.Util (fromJust_s, head_s, lookup_s)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.List as L

-- errors {{{1
data ErrorCode = -- {{{2
    ArrayInitializerList 
  | AssemblerCode
  | BuiltinExpr
  | CaseRange
  | CriticalGroup
  | CriticalRecursion
  | Ellipses
  | GnucAttribute
  | GotoPtr
  | IllFormedSwitch
  | MainFunction
  | NestedFunction
  | NoParameterList
  | NoReturnType
  | NoThreads
  | NoVarName
  | OldStyleParams
  | RangeDesignator
  | ReservedPrefix
  | StartFunctionSignature
  | StatExpression
  | ThreadLocalStorage
  | ThreadNotBlocking
  | VolatileQualifier
  deriving (Eq, Enum, Show)

errorText :: ErrorCode -> String -- {{{2
errorText ArrayInitializerList =
  "Sorry, initializer list for arrays are not supported for critical functions."
errorText AssemblerCode =
  "transformation of assembler code is not supported"
errorText BuiltinExpr =
  "GNU C builtin expressions are not supported"
errorText CaseRange =
  "case ranges are not part of C99 and are thus not supported"
errorText CriticalGroup =
  "A declarator of critical function must be contained in its own declaration"
errorText CriticalRecursion =
  "recursion of critical functions"
errorText Ellipses =
  "No ellipses for critical functions"
errorText GnucAttribute =
  "__attribute__ is a GNU extension and is only allowed to declare thread start functions and blocking functions"
errorText GotoPtr =
  "Computed gotos are not part of C99 and are thus not supported"
errorText IllFormedSwitch =
  "Ill-formed switch statement"
errorText MainFunction =
  "A T-code application must not have a 'main' function."
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
errorText RangeDesignator =
  "GNU C array range designators are not supported"
errorText ReservedPrefix =
  "The prefix 'ec_' is reserved, you may not use it."
errorText StartFunctionSignature =
  "thread start functions must have the following signature 'void name()'"
errorText StatExpression =
  "GNU C compound statement as expressions are not supported"
errorText ThreadLocalStorage =
  "GNU C thread local storage is not supported"
errorText ThreadNotBlocking =
  "thread does not call any blocking functions"
errorText VolatileQualifier =
  "volatile type qualifiers are not supported in critical functions"

newError :: ErrorCode -> Maybe String -> Maybe NodeInfo -> OcramError  -- {{{2
newError code extraWhat where_ =
  let
    extraWhat' = fromMaybe "" extraWhat
    what = errorText code ++ extraWhat'
  in
    new_error (fromEnum code) what where_


global_constraints :: CTranslUnit -> Either [OcramError] () -- {{{1
global_constraints ast = failOrPass $ everything (++) (mkQ [] scanExtDecl `extQ` scanBlockItem `extQ` scanStorageSpec `extQ` scanIdent) ast
  where
    scanExtDecl :: CExtDecl -> [OcramError]
    scanExtDecl (CDeclExt (CDecl _ ds ni))
      | any ((== Just "main") . fmap symbol . (\(x, _, _)->x)) ds
                            = [newError MainFunction Nothing (Just ni)]
      | otherwise           = []
    scanExtDecl (CFDefExt fd)
      | symbol fd == "main" = [newError MainFunction Nothing (Just (nodeInfo fd))]
      | otherwise           = []
    scanExtDecl _           = []
      
    scanBlockItem :: CBlockItem -> [OcramError]
    scanBlockItem (CNestedFunDef o) =
      [newError NestedFunction Nothing (Just (nodeInfo o))]
    scanBlockItem _ = []

    scanStorageSpec :: CStorageSpec -> [OcramError]
    scanStorageSpec (CThread ni) =
      [newError ThreadLocalStorage Nothing (Just ni)]
    scanStorageSpec _ = []

    scanIdent :: Ident -> [OcramError]
    scanIdent ident
      | ecPrefix `L.isPrefixOf` symbol ident =
          [newError ReservedPrefix Nothing (Just (nodeInfo ident))]
      | otherwise = []

critical_constraints :: CallGraph -> CTranslUnit -> Either [OcramError] () -- {{{1
critical_constraints cg ast = failOrPass $
     checkRecursion cg
  ++ checkStartFunctions cg ast
  ++ checkThreads cg
  ++ checkFeatures cg ast

checkRecursion :: CallGraph -> [OcramError] -- {{{2
checkRecursion cg@(CallGraph gd gi) = mapMaybe (fmap (createRecError cg) . find_loop gd . $lookup_s gi) $ start_functions cg

createRecError :: CallGraph -> [G.Node] -> OcramError
createRecError (CallGraph gd _) call_stack =
  newError CriticalRecursion (Just errText) (Just location)
  where
    errText = L.intercalate " -> " $
      map (show . lblName . $fromJust_s . G.lab gd) call_stack
    (callee:caller:[]) = take 2 $ L.reverse call_stack 
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
    filt (CDeclExt cd@(CDecl ts dds ni))
      | length dds /= 1 = concatMap criticalGroup dds
      | not (is_critical cg (symbol cd)) = []
      | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
      | is_blocking_function cd = noVarNames cd ++ blockingFunctionConstraints cd
      | otherwise = criticalFunctionConstraints cd
      where
      noVarNames = mapMaybe noVarName . function_parameters_cd

      noVarName p@(CDecl _ [] _) = Just $ newError NoVarName Nothing (Just (nodeInfo p))
      noVarName _                = Nothing

      criticalGroup (Nothing, _, _) = []
      criticalGroup (Just declr, _, _) = [newError CriticalGroup Nothing (Just ni) | is_critical cg (symbol declr)]
      
        
    filt (CFDefExt fd@(CFunDef ts (CDeclr _ ps _ _ _) _ _ ni))
      | not (is_critical cg (symbol fd)) = []
      | not (hasReturnType ts) = [newError NoReturnType Nothing (Just ni)]
      | null ps = [newError NoParameterList Nothing (Just ni)]
      | otherwise = criticalFunctionConstraints fd

    filt _ = []

    criticalFunctionConstraints :: forall a. Data a => a -> [OcramError]
    criticalFunctionConstraints = everything (++) (mkQ []
             scanAttribute
      `extQ` scanDerivedDeclr
      `extQ` scanDecl
      `extQ` scanStat
      `extQ` scanExpr
      `extQ` scanPartDesig
      `extQ` scanTypeQualifier
      )

    blockingFunctionConstraints = everything (++) (mkQ [] scanAttribute `extQ` scanDerivedDeclr)

    hasReturnType = any isTypeSpec
      where
        isTypeSpec (CTypeSpec _) = True
        isTypeSpec _ = False

    scanAttribute :: CAttribute NodeInfo -> [OcramError]
    scanAttribute (CAttr (Ident name _ _) _ ni)
      | name `elem` [blockingAttr, startAttr] = []
      | otherwise =
          [newError GnucAttribute Nothing (Just ni)]

    scanTypeQualifier :: CTypeQualifier NodeInfo -> [OcramError]
    scanTypeQualifier (CVolatQual ni) =
      [newError VolatileQualifier Nothing (Just ni)]
    scanTypeQualifier _ = []

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
