{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Translate.ThreadFunctions
where

-- imports {{{1
import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (runState, get, put)
import Data.Generics (everywhereM, everywhere, mkT, mkM)
import Data.Maybe (maybeToList, catMaybes)
import Language.C.Data.Node (nodeInfo)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST
import Ocram.Analysis (start_functions, call_chain, call_order, is_blocking, is_critical, CallGraph)
import Ocram.Debug (un, ENodeInfo(..), Substitution(..))
import Ocram.Query (function_definition, function_parameters, local_variables_fd)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Names
import Ocram.Transformation.Util (ident)
import Ocram.Transformation.Types
import Ocram.Util ((?:), fromJust_s, abort)
import Prelude hiding (exp, id)
import qualified Data.Map as Map

add_thread_functions :: CallGraph -> CTranslUnit' -> CTranslUnit' -- {{{1
add_thread_functions cg ast@(CTranslUnit decls ni) =
  CTranslUnit (decls ++ thread_functions) ni
  where
    thread_functions = map (CFDefExt . create) $ zip [0..] (start_functions cg)

    create (tid, startFunction) =
      let
        fun = CFunDef [CTypeSpec (CVoidType un)] (CDeclr (Just (ident (threadExecutionFunction tid))) [CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un) [] (CCompound [] (intro : concat functions) un) un
        intro = CBlockStmt (CIf (CVar (ident contVar) un) (CGotoPtr (CVar (ident contVar) un) un) Nothing un)
        onlyDefs name = not (is_blocking cg name) && is_critical cg name
        functions = map (inlineCriticalFunction cg ast startFunction) $ zip (True : repeat False) $ filter onlyDefs $ $fromJust_s $ call_order cg startFunction
      in
        fmap (\eni -> eni {enThreadId = Just tid}) fun

inlineCriticalFunction :: CallGraph -> CTranslUnit' -> Symbol -> (Bool, Symbol) -> [CBlockItem'] -- {{{2
inlineCriticalFunction cg ast startFunction (isThreadStartFunction, inlinedFunction) = lbl ?: inlinedBody
  where
    callChain = $fromJust_s $ call_chain cg startFunction inlinedFunction
    fd = $fromJust_s $ function_definition ast inlinedFunction
    localVariables = local_variables_fd fd

    lbl = if isThreadStartFunction
      then Nothing
      else Just $ createLabel inlinedFunction 0

    inlinedBody = extractBody $ (rewriteCriticalFunctionCalls . rewriteReturns . rewriteLocalVariableAccess . rewriteLocalVariableDecls) fd

    extractBody (CFunDef _ _ _ (CCompound _ body _) _) = body
    extractBody _ = $abort "unexpected parameters"

    rewriteLocalVariableDecls :: CFunDef' -> CFunDef' -- {{{3
    rewriteLocalVariableDecls = everywhere (mkT rewrite)
      where
        rewrite (CCompound x items ni) =
          let (subst, items') = ((concat *** catMaybes) . unzip . map transform) items in
          CCompound x items' (ni {enSubst = subst})

        rewrite o = o

        transform (CBlockDecl cd)
          | Map.member (symbol cd) localVariables =
              ((,) <$> fmap renameTvar . enSubst . annotation <*> initialize . disableSubst) cd
          | otherwise =
              ((,) <$> enSubst . annotation <*> Just . CBlockDecl . disableSubst) cd
          where
            disableSubst = fmap (\ni -> ni {enSubst = []})
            renameTvar subst =
              let expr = stackAccess callChain (Just (substTVar subst)) un in
              subst {substTVar = (show . pretty . fmap nodeInfo) expr}

        transform o = ([], Just o)

        initialize cd@(CDecl _ [(_, Just (CInitExpr expr _), _)] _) = Just $
          CBlockStmt (CExpr (Just (CAssign CAssignOp (var cd ni) expr ni)) ni)
            where ni = annotation expr
        initialize _ = Nothing

        var cd ni = stackAccess callChain (Just (symbol cd)) ni

    rewriteLocalVariableAccess :: CFunDef' -> CFunDef' -- {{{3
    rewriteLocalVariableAccess = everywhere (mkT rewrite)
      where
        rewrite o@(CVar iden _)
          | Map.member name localVariables = stackAccess callChain (Just name) (annotation o)
          | otherwise = o
          where name = symbol iden
        rewrite o = o

    rewriteReturns :: CFunDef' -> CFunDef' -- {{{3
    rewriteReturns fdef
      | startFunction == inlinedFunction = fdef
      | otherwise = everywhere (mkT rewrite) fdef
      where
        rewrite :: CStat' -> CStat'
        rewrite o@(CReturn Nothing _) = goto (annotation o)
        rewrite o@(CReturn (Just cexpr) _) = CCompound [] (map CBlockStmt [assign cexpr, goto ni]) ni
          where ni = annotation o
        rewrite o = o
        assign e = CExpr (Just (CAssign CAssignOp (stackAccess callChain (Just resVar) ni) e ni)) ni
          where ni = annotation e
        goto ni = CGotoPtr (stackAccess callChain (Just contVar) ni) ni

    rewriteCriticalFunctionCalls :: CFunDef' -> CFunDef' -- {{{3
    rewriteCriticalFunctionCalls fd' = fst $ runState (everywhereM (mkM rewrite) fd') 1
      where
        rewrite (CCompound x items y) = do
          items' <- mapM transform items
          return $ CCompound x (concat items') y
        rewrite o = return o

        transform o@(CBlockStmt stat@(CExpr (Just (CCall (CVar iden _) params _)) _))
          | is_critical cg calledFunction = callSequence calledFunction (annotation stat) params Nothing
          | otherwise = return [o]
          where calledFunction = symbol iden

        transform o@(CBlockStmt expr@(CExpr (Just (CAssign CAssignOp lhs (CCall (CVar iden _) params _) _)) _))
          | is_critical cg calledFunction = callSequence calledFunction (annotation expr) params (Just lhs)
          | otherwise = return [o]
          where calledFunction = symbol iden 

        transform o = return [o]

        callSequence calledFunction ni params resultLhs = do
          lblIdx <- get
          put (lblIdx + 1)  
          return $ criticalFunctionCallSequence calledFunction ni lblIdx params resultLhs

    criticalFunctionCallSequence :: Symbol -> ENodeInfo -> Int -> [CExpr'] -> Maybe (CExpr') -> [CBlockItem'] -- {{{3
    criticalFunctionCallSequence calledFunction ni lblIdx params resultLhs =
      parameters ++ continuation : callExp : returnExp ?: lbl' : resultExp ?: []
      where
        bni = ni {enBlockingCall = True}
        callChain' = callChain ++ [calledFunction]
        blocking = is_blocking cg calledFunction
        parameters = zipWith createParamAssign params $ $fromJust_s $ function_parameters ast calledFunction
        continuation = createAssign (stackAccess callChain' (Just contVar) ni) (CLabAddrExpr (ident $ label inlinedFunction lblIdx) un)

        lbl' = createLabel inlinedFunction lblIdx
        resultExp = fmap assignResult resultLhs
        assignResult lhs = createAssign lhs (stackAccess callChain' (Just resVar) ni)

        callExp = CBlockStmt $ if blocking
          then CExpr (Just (CCall (CVar (ident calledFunction) ni) [CUnary CAdrOp (stackAccess callChain' Nothing ni) ni] bni)) ni
          else CGoto (ident $ label calledFunction 0) ni

        returnExp = if blocking
          then Just (CBlockStmt (CReturn Nothing ni))
          else Nothing

        createParamAssign exp decl = createAssign (stackAccess callChain' (Just (symbol decl)) ni) exp

        createAssign lhs rhs = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs ni)) ni)

stackAccess :: [Symbol] -> Maybe Symbol -> ENodeInfo -> CExpr' -- {{{2
stackAccess (sf:chain) variable ni = foldl create base members
  where
    variables = maybeToList variable
    base = CVar (ident $ stackVar sf) ni
    members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
    create inner member = CMember inner (ident member) False ni
stackAccess [] _ _ = $abort "called stackAccess with empty call chain"

createLabel :: Symbol -> Int -> CBlockItem' -- {{{2
createLabel name id = CBlockStmt $ CLabel (ident (label name id)) (CExpr Nothing un) [] un


