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

traverseCTranslUnit :: Visitor v => CTranslUnit -> State v ()
traverseCTranslUnit (CTranslUnit decls _) = mapM_ traverseCExtDecl decls

traverseCExtDecl :: Visitor v => CExtDecl -> State v ()
traverseCExtDecl ced@(CDeclExt cd) = modify (handleCExtDecl ced) >> traverseCDecl cd
traverseCExtDecl ced@(CFDefExt cfd) = modify (handleCExtDecl ced) >> traverseCFunDef cfd
traverseCExtDecl ced@(CAsmExt ca _) = modify (handleCExtDecl ced)

traverseCDecl :: Visitor v => CDecl -> State v ()
traverseCDecl cd@(CDecl _ decls _) = modify (handleCDecl cd) >> hDecls decls
    where hDecls [] = return ()
          hDecls ((cd, ci, expr):ds) = 
               maybeTraverse traverseCDeclr cd
            >> maybeTraverse traverseCInit ci
            >> maybeTraverse traverseCExpr expr
            >> hDecls ds

traverseCFunDef cfd@(CFunDef _ cdr cds cst _) = modify (handleCFunDef cfd)
    >> traverseCDeclr cdr 
    >> hCds cds 
    >> traverseCStat cst
    where hCds [] = return ()
          hCds (cd:cds) = traverseCDecl cd >> hCds cds

traverseCDeclr cdr@(CDeclr id cdds _ _ _) = modify (handleCDeclr cdr)
    >> maybeTraverse traverseIdent id 
    >> hCdds cdds 
    where hCdds [] = return ()
          hCdds (cdd:cdds) = modify (handleCDerivedDeclr cdd) >> hCdds cdds

traverseCInit ci@(CInitExpr ce _) = modify (handleCInit ci) >> traverseCExpr ce
traverseCInit ci@(CInitList cis _) = modify (handleCInit ci) >> traverseCInitList cis

traverseCInitList cis = mapM_  (modify . handleCInit . snd) cis

traverseIdent id = modify $ handleIdent id

traverseCExpr ce@(CComma ces _) = modify (handleCExpr ce) 
    >> mapM_ traverseCExpr ces
traverseCExpr ce@(CAssign _ ce1 ce2 _) = modify (handleCExpr ce) 
    >> traverseCExpr ce1
    >> traverseCExpr ce2
traverseCExpr ce@(CCond ce1 ce2 ce3 _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
    >> maybeTraverse traverseCExpr ce2
    >> traverseCExpr ce3 
traverseCExpr ce@(CBinary _ ce1 ce2 _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
    >> traverseCExpr ce2
traverseCExpr ce@(CCast cd ce1 _) = modify (handleCExpr ce)
    >> traverseCDecl cd
    >> traverseCExpr ce1
traverseCExpr ce@(CUnary _ ce1 _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
traverseCExpr ce@(CSizeofExpr ce1 _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
traverseCExpr ce@(CSizeofType cd _) = modify (handleCExpr ce)
    >> traverseCDecl cd
traverseCExpr ce@(CAlignofExpr ce1 _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
traverseCExpr ce@(CAlignofType cd _) = modify (handleCExpr ce)
    >> traverseCDecl cd
traverseCExpr ce@(CComplexReal ce1 _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
traverseCExpr ce@(CComplexImag ce1 _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
traverseCExpr ce@(CIndex ce1 ce2 _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
    >> traverseCExpr ce2
traverseCExpr ce@(CCall ce1 ces _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
    >> mapM_ traverseCExpr ces
traverseCExpr ce@(CMember ce1 id _ _) = modify (handleCExpr ce)
    >> traverseCExpr ce1
    >> traverseIdent id
traverseCExpr ce@(CVar id _) = modify (handleCExpr ce)
    >> traverseIdent id
traverseCExpr ce@(CConst _) = modify (handleCExpr ce)
traverseCExpr ce@(CCompoundLit cd cil _) = modify (handleCExpr ce)
    >> traverseCDecl cd
    >> traverseCInitList cil
traverseCExpr ce@(CStatExpr cst _) = modify (handleCExpr ce)
    >> traverseCStat cst
traverseCExpr ce@(CLabAddrExpr id _) = modify (handleCExpr ce)
    >> traverseIdent id
traverseCExpr ce@(CBuiltinExpr _) = modify (handleCExpr ce)

traverseCStat = undefined
