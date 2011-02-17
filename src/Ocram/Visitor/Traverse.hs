module Ocram.Visitor.Traverse (
	execTrav,
	traverseCTranslUnit
) where


import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident)
import Control.Monad.State
import Ocram.Visitor.Visitor

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
	where
		hDecls [] = return ()
		hDecls ((cd, ci, expr):ds) = 
				  maybeTraverse traverseCDeclr cd
			>> maybeTraverse traverseCInit ci
			>> maybeTraverse traverseCExpr expr
			>> hDecls ds

traverseCFunDef :: Visitor v => CFunDef -> State v ()
traverseCFunDef cfd@(CFunDef _ cdr cds cst _) = modify (handleCFunDef cfd)
	>> traverseCDeclr cdr 
	>> hCds cds 
	>> traverseCStat cst
	where
		hCds [] = return ()
		hCds (cd:cds) = traverseCDecl cd >> hCds cds

traverseCDeclr :: Visitor v => CDeclr -> State v ()
traverseCDeclr cdr@(CDeclr id cdds _ _ _) = modify (handleCDeclr cdr)
	>> maybeTraverse traverseIdent id 
	>> hCdds cdds 
	where
		hCdds [] = return ()
		hCdds (cdd:cdds) = modify (handleCDerivedDeclr cdd) >> hCdds cdds

traverseCInit :: Visitor v => CInit -> State v ()
traverseCInit ci@(CInitExpr ce _) = modify (handleCInit ci) >> traverseCExpr ce
traverseCInit ci@(CInitList cis _) = modify (handleCInit ci) >> traverseCInitList cis

traverseCInitList :: Visitor v => CInitList -> State v ()
traverseCInitList cis = mapM_  (modify . handleCInit . snd) cis

traverseIdent :: Visitor v => Ident -> State v ()
traverseIdent id = modify $ handleIdent id

traverseCBlockItem :: Visitor v => CBlockItem -> State v ()
traverseCBlockItem (CBlockStmt cs1) = traverseCStat cs1
traverseCBlockItem (CBlockDecl cd) = traverseCDecl cd
traverseCBlockItem (CNestedFunDef cfd) = traverseCFunDef cfd

traverseCExpr :: Visitor v => CExpr -> State v ()
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

traverseCStat :: Visitor v => CStat -> State v ()
traverseCStat cs@(CLabel _ cs1 _ _) = modify (handleCStat cs)
	>> traverseCStat cs1
traverseCStat cs@(CCase ce1 cs1 _) = modify (handleCStat cs)
	>> traverseCExpr ce1
	>> traverseCStat cs1
traverseCStat cs@(CCases ce1 ce2 cs1 _) = modify (handleCStat cs)
	>> traverseCExpr ce1
	>> traverseCExpr ce2
	>> traverseCStat cs1
traverseCStat cs@(CDefault cs1 _) = modify (handleCStat cs)
	>> traverseCStat cs1
traverseCStat cs@(CExpr ce1 _) = modify (handleCStat cs)
	>> maybeTraverse traverseCExpr ce1 
traverseCStat cs@(CCompound _ ccbis _) = modify (handleCStat cs)
	>> mapM_ traverseCBlockItem ccbis
traverseCStat cs@(CIf ce1 cs1 cs2 _) = modify (handleCStat cs)
	>> traverseCExpr ce1
	>> traverseCStat cs1
	>> maybeTraverse traverseCStat cs2
traverseCStat cs@(CSwitch ce1 cs1 _) = modify (handleCStat cs)
	>> traverseCExpr ce1
	>> traverseCStat cs1
traverseCStat cs@(CWhile ce1 cs1 _ _) = modify (handleCStat cs)
	>> traverseCExpr ce1
	>> traverseCStat cs1
traverseCStat cs@(CFor (Left ce1) ce2 ce3 cs1 _) = modify (handleCStat cs)
	>> maybeTraverse traverseCExpr ce1
	>> maybeTraverse traverseCExpr ce2
	>> maybeTraverse traverseCExpr ce3
	>> traverseCStat cs1
traverseCStat cs@(CFor (Right cd1) ce1 ce2 cs1 _) = modify (handleCStat cs)
	>> traverseCDecl cd1
	>> maybeTraverse traverseCExpr ce1
	>> maybeTraverse traverseCExpr ce2
	>> traverseCStat cs1
traverseCStat cs@(CGoto _ _) = modify (handleCStat cs)
traverseCStat cs@(CGotoPtr ce1 _) = modify (handleCStat cs)
	>> traverseCExpr ce1
traverseCStat cs@(CCont _) = modify (handleCStat cs)
traverseCStat cs@(CBreak _) = modify (handleCStat cs)
traverseCStat cs@(CReturn ce1 _) = modify (handleCStat cs)
	>> maybeTraverse traverseCExpr ce1
traverseCStat cs@(CAsm _ _) = modify (handleCStat cs)
