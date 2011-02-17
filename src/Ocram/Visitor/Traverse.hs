module Ocram.Visitor.Traverse (
	traverseCTranslUnit
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident)
import Ocram.Visitor.Visitor

trav :: (DownVisitor d, UpVisitor d u) => (a -> d -> u) -> a -> d -> [u]
trav f x d = [f x d]

maybeTrav :: (DownVisitor d, UpVisitor d u) => (a -> d -> u) -> Maybe a -> d -> [u]
maybeTrav f (Just x) d = [f x d]
maybeTrav f Nothing d = []

mapTrav :: (DownVisitor d, UpVisitor d u) => (a -> d -> u) -> [a] -> d -> [u]
mapTrav f xs d = map (\x -> f x d) xs

traverseCTranslUnit :: (DownVisitor d, UpVisitor d u) => CTranslUnit -> d -> u
traverseCTranslUnit ctu@(CTranslUnit decls _) d = u
	where
		d' = downCTranslUnit ctu d
		us = mapTrav traverseCExtDecl decls d'
		u  = upCTranslUnit ctu d' us

recurseCExtDecl :: (DownVisitor d, UpVisitor d u) => CExtDecl -> d -> (a -> d -> [u]) -> a -> u
recurseCExtDecl ced d f x = u
	where
		d' = downCExtDecl ced d
		us = f x d'
		u  = upCExtDecl ced d' us

traverseCExtDecl :: (DownVisitor d, UpVisitor d u) => CExtDecl -> d -> u
traverseCExtDecl ced@(CDeclExt cd) d = u
	where
		d' = downCExtDecl ced d
		us = trav traverseCDecl cd d'
		u  = upCExtDecl ced d' us

traverseCExtDecl ced@(CFDefExt cfd) d = u
	where
		d' = downCExtDecl ced d
		us = trav traverseCFunDef cfd d'
		u  = upCExtDecl ced d' us

traverseCExtDecl ced@(CAsmExt _ _) d = u
	where
		d' = downCExtDecl ced d
		u  = upCExtDecl ced d' []

traverseCFunDef = undefined

traverseCDecl :: (DownVisitor d, UpVisitor d u) => CDecl -> d -> u
traverseCDecl cd@(CDecl _ decls _) = modify (handleCDecl cd) >> hDecls decls
	where
		hDecls [] = return ()
		hDecls ((cd, ci, expr):ds) = 
					maybeTraverse traverseCDeclr cd
			>> maybeTraverse traverseCInit ci
			>> maybeTraverse traverseCExpr expr
			>> hDecls ds

-- traverseCFunDef :: (DownVisitor d, UpVisitor d u) => CFunDef -> d -> u
-- traverseCFunDef cfd@(CFunDef _ cdr cds cst _) = modify (handleCFunDef cfd)
--   >> traverseCDeclr cdr 
--   >> hCds cds 
--   >> traverseCStat cst
--   where
--     hCds [] = return ()
--     hCds (cd:cds) = traverseCDecl cd >> hCds cds

-- traverseCDeclr :: (DownVisitor d, UpVisitor d u) => CDeclr -> d -> u
-- traverseCDeclr cdr@(CDeclr id cdds _ _ _) = modify (handleCDeclr cdr)
--   >> maybeTraverse traverseIdent id 
--   >> hCdds cdds 
--   where
--     hCdds [] = return ()
--     hCdds (cdd:cdds) = modify (handleCDerivedDeclr cdd) >> hCdds cdds

-- traverseCInit :: (DownVisitor d, UpVisitor d u) => CInit -> d -> u
-- traverseCInit ci@(CInitExpr ce _) = modify (handleCInit ci) >> traverseCExpr ce
-- traverseCInit ci@(CInitList cis _) = modify (handleCInit ci) >> traverseCInitList cis

-- traverseCInitList :: (DownVisitor d, UpVisitor d u) => CInitList -> d -> u
-- traverseCInitList cis = mapM_  (modify . handleCInit . snd) cis

-- traverseIdent :: (DownVisitor d, UpVisitor d u) => Ident -> d -> u
-- traverseIdent id = modify $ handleIdent id

-- traverseCBlockItem :: (DownVisitor d, UpVisitor d u) => CBlockItem -> d -> u
-- traverseCBlockItem (CBlockStmt cs1) = traverseCStat cs1
-- traverseCBlockItem (CBlockDecl cd) = traverseCDecl cd
-- traverseCBlockItem (CNestedFunDef cfd) = traverseCFunDef cfd

-- traverseCExpr :: (DownVisitor d, UpVisitor d u) => CExpr -> d -> u
-- traverseCExpr ce@(CComma ces _) = modify (handleCExpr ce) 
--   >> mapM_ traverseCExpr ces
-- traverseCExpr ce@(CAssign _ ce1 ce2 _) = modify (handleCExpr ce) 
--   >> traverseCExpr ce1
--   >> traverseCExpr ce2
-- traverseCExpr ce@(CCond ce1 ce2 ce3 _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
--   >> maybeTraverse traverseCExpr ce2
--   >> traverseCExpr ce3 
-- traverseCExpr ce@(CBinary _ ce1 ce2 _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
--   >> traverseCExpr ce2
-- traverseCExpr ce@(CCast cd ce1 _) = modify (handleCExpr ce)
--   >> traverseCDecl cd
--   >> traverseCExpr ce1
-- traverseCExpr ce@(CUnary _ ce1 _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
-- traverseCExpr ce@(CSizeofExpr ce1 _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
-- traverseCExpr ce@(CSizeofType cd _) = modify (handleCExpr ce)
--   >> traverseCDecl cd
-- traverseCExpr ce@(CAlignofExpr ce1 _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
-- traverseCExpr ce@(CAlignofType cd _) = modify (handleCExpr ce)
--   >> traverseCDecl cd
-- traverseCExpr ce@(CComplexReal ce1 _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
-- traverseCExpr ce@(CComplexImag ce1 _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
-- traverseCExpr ce@(CIndex ce1 ce2 _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
--   >> traverseCExpr ce2
-- traverseCExpr ce@(CCall ce1 ces _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
--   >> mapM_ traverseCExpr ces
-- traverseCExpr ce@(CMember ce1 id _ _) = modify (handleCExpr ce)
--   >> traverseCExpr ce1
--   >> traverseIdent id
-- traverseCExpr ce@(CVar id _) = modify (handleCExpr ce)
--   >> traverseIdent id
-- traverseCExpr ce@(CConst _) = modify (handleCExpr ce)
-- traverseCExpr ce@(CCompoundLit cd cil _) = modify (handleCExpr ce)
--   >> traverseCDecl cd
--   >> traverseCInitList cil
-- traverseCExpr ce@(CStatExpr cst _) = modify (handleCExpr ce)
--   >> traverseCStat cst
-- traverseCExpr ce@(CLabAddrExpr id _) = modify (handleCExpr ce)
--   >> traverseIdent id
-- traverseCExpr ce@(CBuiltinExpr _) = modify (handleCExpr ce)

-- traverseCStat :: (DownVisitor d, UpVisitor d u) => CStat -> d -> u
-- traverseCStat cs@(CLabel _ cs1 _ _) = modify (handleCStat cs)
--   >> traverseCStat cs1
-- traverseCStat cs@(CCase ce1 cs1 _) = modify (handleCStat cs)
--   >> traverseCExpr ce1
--   >> traverseCStat cs1
-- traverseCStat cs@(CCases ce1 ce2 cs1 _) = modify (handleCStat cs)
--   >> traverseCExpr ce1
--   >> traverseCExpr ce2
--   >> traverseCStat cs1
-- traverseCStat cs@(CDefault cs1 _) = modify (handleCStat cs)
--   >> traverseCStat cs1
-- traverseCStat cs@(CExpr ce1 _) = modify (handleCStat cs)
--   >> maybeTraverse traverseCExpr ce1 
-- traverseCStat cs@(CCompound _ ccbis _) = modify (handleCStat cs)
--   >> mapM_ traverseCBlockItem ccbis
-- traverseCStat cs@(CIf ce1 cs1 cs2 _) = modify (handleCStat cs)
--   >> traverseCExpr ce1
--   >> traverseCStat cs1
--   >> maybeTraverse traverseCStat cs2
-- traverseCStat cs@(CSwitch ce1 cs1 _) = modify (handleCStat cs)
--   >> traverseCExpr ce1
--   >> traverseCStat cs1
-- traverseCStat cs@(CWhile ce1 cs1 _ _) = modify (handleCStat cs)
--   >> traverseCExpr ce1
--   >> traverseCStat cs1
-- traverseCStat cs@(CFor (Left ce1) ce2 ce3 cs1 _) = modify (handleCStat cs)
--   >> maybeTraverse traverseCExpr ce1
--   >> maybeTraverse traverseCExpr ce2
--   >> maybeTraverse traverseCExpr ce3
--   >> traverseCStat cs1
-- traverseCStat cs@(CFor (Right cd1) ce1 ce2 cs1 _) = modify (handleCStat cs)
--   >> traverseCDecl cd1
--   >> maybeTraverse traverseCExpr ce1
--   >> maybeTraverse traverseCExpr ce2
--   >> traverseCStat cs1
-- traverseCStat cs@(CGoto _ _) = modify (handleCStat cs)
-- traverseCStat cs@(CGotoPtr ce1 _) = modify (handleCStat cs)
--   >> traverseCExpr ce1
-- traverseCStat cs@(CCont _) = modify (handleCStat cs)
-- traverseCStat cs@(CBreak _) = modify (handleCStat cs)
-- traverseCStat cs@(CReturn ce1 _) = modify (handleCStat cs)
--   >> maybeTraverse traverseCExpr ce1
-- traverseCStat cs@(CAsm _ _) = modify (handleCStat cs)
