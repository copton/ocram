module Visitor.Traverse (
      execTrav
    , traverseCTranslUnit
) where


import Language.C.Syntax.AST
import Control.Monad.State
import Visitor.Visitor

execTrav :: Visitor v => (a -> State v ()) -> a -> v -> v
execTrav trav obj init = execState (trav obj) init

maybeTraverse f a = maybe (return ()) f a
maybeHandle f a = modify $ maybe id f a

traverseCTranslUnit :: Visitor v => CTranslUnit -> State v ()
traverseCTranslUnit (CTranslUnit decls _) = mapM_ traverseCExtDecl decls

traverseCExtDecl :: Visitor v => CExtDecl -> State v ()
traverseCExtDecl ced@(CDeclExt cd) = modify (handleCExtDecl ced) >> traverseCDecl cd
traverseCExtDecl ced@(CFDefExt cfd) = modify (handleCExtDecl ced) >> traverseCFunDef cfd
traverseCExtDecl ced@(CAsmExt ca _) = modify (handleCExtDecl ced)

traverseCDecl :: Visitor v => CDecl -> State v ()
traverseCDecl cd@(CDecl _ decls _) = modify (handleCDecl cd) >> hDecls decls
    where hDecls [] = return ()
          hDecls ((cd, ci, expr):ds) = do
            maybeTraverse traverseCDeclr cd
            maybeTraverse traverseCInit ci
            maybeTraverse traverseCExpr expr
            hDecls ds

traverseCFunDef cfd@(CFunDef _ cdr cds cst _) = modify (handleCFunDef cfd) >>
        traverseCDeclr cdr >> hCds cds >> traverseCStat cst
    where hCds [] = return ()
          hCds (cd:cds) = traverseCDecl cd >> hCds cds

traverseCDeclr cdr@(CDeclr id cdds _ _ _) = modify (handleCDeclr cdr) >>
        maybeHandle handleIdent id >> hCdds cdds 
    where hCdds [] = return ()
          hCdds (cdd:cdds) = modify (handleCDerivedDeclr cdd) >> hCdds cdds

traverseCInit ci@(CInitExpr ce _) = modify (handleCInit ci) >> traverseCExpr ce
traverseCInit ci@(CInitList cis _) = hCis cis
    where hCis [] = return ()
          hCis ((_,ci):cis) = modify (handleCInit ci) >> hCis cis

traverseCExpr = undefined
traverseCStat = undefined
