module Ocram.Visitor.Traverse 
-- export {{{1
(
	 traverseCDecl
	,traverseCDeclr
	,traverseCDerivedDeclr
	,traverseCExpr
	,traverseCExtDecl
	,traverseCFunDef
	,traverseCInit
	,traverseCStat
	,traverseCBlockItem
	,traverseCTranslUnit
	,traverseIdent
) where

-- import {{{1
import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Ident (Ident)
import Ocram.Visitor.Visitor
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (mappend, mempty, Monoid)

-- recurse {{{1
recurse :: 
	   (o -> d -> d) -- down handler
	-> (o -> d -> u -> (Maybe o, u)) -- up handler
	-> o -- outer object
	-> (o -> i -> o) -- create
	-> (i -> d -> (Maybe i, u)) -- traverse inner object
	-> i -- inner object tuple
	-> d -- down state
	-> (Maybe o, u) -- (maybe new outer object, up state)
recurse downHandler upHandler outerObject create traverse innerObjects downState =
  let downState' = downHandler outerObject downState in
	case traverse innerObjects downState' of
		(Nothing, upState) -> 
			upHandler outerObject downState' upState
		(Just innerObjects', upState) ->
			let outerObject' = create outerObject innerObjects' in
			case upHandler outerObject' downState' upState of
				(Nothing, upState') -> (Just outerObject', upState')
				res -> res

-- mapTrav {{{1
mapTrav :: (Monoid u) =>
	   (o -> d -> u -> (Maybe [o], d)) -- cross handler; recurse function
	-> (d -> u) -- last handler; recursion termination
	-> (o -> d -> (Maybe o, u)) -- traversal of a single object
	-> ([o] -> d -> (Maybe [o], u)) -- traversal method accepting list of objects
mapTrav cross last trav os d = 
	case foldl iter (False, [], d, mempty) os of
		(True, os', d, u) -> (Just (reverse os'), mappend (last d) u)
		(False, _, d, u) -> (Nothing, mappend (last d) u)
	where
		iter (flag, os, c, u) o =
			case trav o c of
				(Just o', u') -> case cross o' c u' of
					(Just o'', c') -> (True, o'' ++ os, c', mappend u u')
					(Nothing, c') -> (True, o' : os, c', mappend u u')
				(Nothing, u') -> case cross o c u' of
					(Just o', c') -> (True, o' ++ os, c', mappend u u')
					(Nothing, c') -> (flag, o : os, c', mappend u u')


-- CTranslUnit {{{1
traverseCTranslUnit :: (DownVisitor d, UpVisitor d u) => CTranslUnit -> d -> (Maybe CTranslUnit, u)
traverseCTranslUnit ctu@(CTranslUnit decls _) d = recurse downCTranslUnit mapCTranslUnit ctu create traverse decls d
	where 
		traverse = mapTrav crossCExtDecl lastCExtDecl traverseCExtDecl
		create (CTranslUnit _ ni) decls = CTranslUnit decls ni

-- CExtDecl {{{1
traverseCExtDecl :: (DownVisitor d, UpVisitor d u) => CExtDecl -> d -> (Maybe CExtDecl, u)
traverseCExtDecl ced@(CDeclExt cd) d = recurse downCExtDecl mapCExtDecl ced create traverse cd d 
	where 
		traverse = traverseCDecl
		create _  cd = CDeclExt cd

traverseCExtDecl ced@(CFDefExt cfd) d = recurse downCExtDecl mapCExtDecl ced create traverse cfd d 
	where 
		traverse = traverseCFunDef
		create _ cfd = CFDefExt cfd

traverseCExtDecl ced@(CAsmExt _ _) d = recurse downCExtDecl mapCExtDecl ced noCreate noTrav noChildren d

-- CDecl {{{1
traverseCDecl :: (DownVisitor d, UpVisitor d u) => CDecl -> d -> (Maybe CDecl, u)
traverseCDecl cd@(CDecl _ decls _) = recurse downCDecl mapCDecl cd create traverse decls
	where 
		traverse = mapTrav crossCDeclMember lastCDeclMember traverse'
		traverse' :: (DownVisitor d, UpVisitor d u) => (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> d -> (Maybe (Maybe CDeclr, Maybe CInit, Maybe CExpr), u)
		traverse' all@(cd, ci, ce) d = merge3 d all
					(maybeTrav traverseCDeclr cd d)
					(maybeTrav traverseCInit ci d)
					(maybeTrav traverseCExpr ce d)
		create (CDecl x _ y) decls = CDecl x decls y

-- CDerivedDeclr {{{1
traverseCDerivedDeclr :: (DownVisitor d, UpVisitor d u) => CDerivedDeclr -> d -> (Maybe CDerivedDeclr, u)
traverseCDerivedDeclr cdd@(CFunDeclr (Left ids) _ _) = recurse downCDerivedDeclr mapCDerivedDeclr cdd create traverse ids
	where
		traverse = mapTrav crossIdent lastIdent traverseIdent
		create (CFunDeclr _ x y) ids = CFunDeclr (Left ids) x y

traverseCDerivedDeclr cdd@(CFunDeclr (Right (cds, _)) _ _) = recurse downCDerivedDeclr mapCDerivedDeclr cdd create traverse cds
	where 
		traverse = mapTrav crossCDecl lastCDecl traverseCDecl
		create (CFunDeclr (Right (_, x)) y z) ods = CFunDeclr (Right (ods, x)) y z

traverseCDerivedDeclr cdd = recurse downCDerivedDeclr mapCDerivedDeclr cdd noCreate noTrav noChildren

-- CFunDef {{{1
traverseCFunDef :: (DownVisitor d, UpVisitor d u) => CFunDef -> d -> (Maybe CFunDef, u)
traverseCFunDef cfd@(CFunDef _ cdr cds cst _) = recurse downCFunDef mapCFunDef cfd create traverse (cdr, cds, cst)
	where 
		traverse all@(cdr, t2, t3) d = merge3 d all
			(traverseCDeclr cdr d)
			(mapTrav crossCDecl lastCDecl traverseCDecl cds d)
			(traverseCStat t3 d)
		create (CFunDef x _ _ _ y) (cdr, cds, cst) = CFunDef x cdr cds cst y

-- CDeclr {{{1
traverseCDeclr :: (DownVisitor d, UpVisitor d u) => CDeclr -> d -> (Maybe CDeclr, u)
traverseCDeclr cdr@(CDeclr id cdds _ _ _) = recurse downCDeclr mapCDeclr cdr create traverse (id, cdds)
	where 
		traverse all@(id, cdds) d = merge2 d all
			(maybeTrav traverseIdent id d)
			(mapTrav crossCDerivedDeclr lastCDerivedDeclr traverseCDerivedDeclr cdds d)
		create (CDeclr _ _ x y z) (id, cdds) = CDeclr id cdds x y z

-- CInit {{{1
traverseCInit :: (DownVisitor d, UpVisitor d u) => CInit -> d -> (Maybe CInit, u)
traverseCInit ci@(CInitExpr ce _) = recurse downCInit mapCInit ci create traverse ce
	where 
		traverse = traverseCExpr
		create (CInitExpr _ x) ce = CInitExpr ce x

traverseCInit ci@(CInitList cis _) = recurse downCInit mapCInit ci create dissolveCInitList cis
	where 
		create (CInitList _ x) cis = CInitList cis x


-- Ident {{{1
traverseIdent :: (DownVisitor d, UpVisitor d u) => Ident -> d -> (Maybe Ident, u)
traverseIdent id = recurse downIdent mapIdent id noCreate noTrav noChildren

-- CExpr {{{1
traverseCExpr :: (DownVisitor d, UpVisitor d u) => CExpr -> d -> (Maybe CExpr, u)
traverseCExpr ce@(CComma ces _) = recurse downCExpr mapCExpr ce create traverse ces
	where
		traverse = mapTrav crossCExpr lastCExpr traverseCExpr
		create (CComma _ x) ces = CComma ces x

traverseCExpr ce@(CAssign _ ce1 ce2 _) = recurse downCExpr mapCExpr ce create traverse (ce1, ce2)
	where 
		traverse all@(ce1, ce2) d = merge2 d all
			(traverseCExpr ce1 d)
			(traverseCExpr ce2 d)
		create (CAssign x _ _ y) (ce1, ce2) = CAssign x ce1 ce2 y

traverseCExpr ce@(CCond ce1 ce2 ce3 _) = recurse downCExpr mapCExpr ce create traverse (ce1, ce2, ce3)
	where 
		traverse all@(ce1, ce2, ce3) d = merge3 d all
			(traverseCExpr ce1 d)
			(maybeTrav traverseCExpr ce2 d)
			(traverseCExpr ce3 d)
		create (CCond _ _ _ x) (ce1, ce2, ce3) = CCond ce1 ce2 ce3 x

traverseCExpr ce@(CBinary _ ce1 ce2 _) = recurse downCExpr mapCExpr ce create traverse (ce1, ce2)
	where 
		traverse all@(ce1, ce2) d = merge2 d all
			(traverseCExpr ce1 d)
			(traverseCExpr ce2 d)
		create (CBinary x _ _ y) (ce1, ce2) = CBinary x ce1 ce2 y

traverseCExpr ce@(CCast cd ce1 _) = recurse downCExpr mapCExpr ce create traverse (cd, ce1)
	where 
		traverse all@(cd, ce1) d = merge2 d all
			(traverseCDecl cd d)
			(traverseCExpr ce1 d)
		create (CCast _ _ x) (cd, ce1) = CCast cd ce1 x

traverseCExpr ce@(CUnary _ ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = traverseCExpr
		create (CUnary x _ y) ce1 = CUnary x ce1 y
	
traverseCExpr ce@(CSizeofExpr ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = traverseCExpr
		create (CSizeofExpr _ x) ce1 = CSizeofExpr ce1 x

traverseCExpr ce@(CSizeofType cd _) = recurse downCExpr mapCExpr ce create traverse cd
	where 
		traverse = traverseCDecl
		create (CSizeofType _ x) cd = CSizeofType cd x

traverseCExpr ce@(CAlignofExpr ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = traverseCExpr
		create (CAlignofExpr _ x) ce1 = CAlignofExpr ce1 x

traverseCExpr ce@(CAlignofType cd _) = recurse downCExpr mapCExpr ce create traverse cd
	where 
		traverse = traverseCDecl 
		create (CAlignofType _ x) cd = CAlignofType cd x

traverseCExpr ce@(CComplexReal ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = traverseCExpr
		create (CComplexReal _ x) ce1 = CComplexReal ce1 x

traverseCExpr ce@(CComplexImag ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = traverseCExpr
		create (CComplexImag _ x) ce1 = CComplexImag ce1 x

traverseCExpr ce@(CIndex ce1 ce2 _) = recurse downCExpr mapCExpr ce create traverse (ce1, ce2)
	where 
		traverse all@(ce1, ce2) d = merge2 d all
			(traverseCExpr ce1 d)
			(traverseCExpr ce2 d)
		create (CIndex _ _ x) (ce1, ce2) = CIndex ce1 ce2 x

traverseCExpr ce@(CCall ce1 ces _) = recurse downCExpr mapCExpr ce create traverse (ce1, ces)
	where 
		traverse all@(ce1, ces) d = merge2 d all
			(traverseCExpr ce1 d)
			(mapTrav crossCExpr lastCExpr traverseCExpr ces d)
		create (CCall _ _ x) (ce1, ces) = CCall ce1 ces x

traverseCExpr ce@(CMember ce1 id _ _) = recurse downCExpr mapCExpr ce create traverse (ce1, id)
	where 
		traverse all@(ce1, id) d = merge2 d all
			(traverseCExpr ce1 d)
			(traverseIdent id d)
		create (CMember _ _ x y) (ce1, id) = CMember ce1 id x y

traverseCExpr ce@(CVar id _) = recurse downCExpr mapCExpr ce create traverse id
	where 
		traverse = traverseIdent
		create (CVar _ x) id = CVar id x

traverseCExpr ce@(CConst _) = recurse downCExpr mapCExpr ce noCreate noTrav noChildren

traverseCExpr ce@(CCompoundLit cd cil _) = recurse downCExpr mapCExpr ce create traverse (cd, cil)
	where 
		traverse all@(cd, cil) d = merge2 d all
			(traverseCDecl cd d)
			(dissolveCInitList cil d)
		create (CCompoundLit _ _ x) (cd, cil) = CCompoundLit cd cil x

traverseCExpr ce@(CStatExpr cst _) = recurse downCExpr mapCExpr ce create traverse cst
	where 
		traverse = traverseCStat
		create (CStatExpr _ x) cst = CStatExpr cst x

traverseCExpr ce@(CLabAddrExpr id _) = recurse downCExpr mapCExpr ce create traverse id
	where 
		traverse = traverseIdent
		create (CLabAddrExpr _ x) id = CLabAddrExpr id x

traverseCExpr ce@(CBuiltinExpr _) = recurse downCExpr mapCExpr ce noCreate noTrav noChildren

-- CStat {{{1
traverseCStat :: (DownVisitor d, UpVisitor d u) => CStat -> d -> (Maybe CStat, u)
traverseCStat cs@(CLabel _ cs1 _ _) d = recurse downCStat mapCStat cs create traverse cs1 d
	where 
		traverse = traverseCStat
		create (CLabel x _ y z) cs1 = CLabel x cs1 y z

traverseCStat cs@(CCase ce1 cs1 _) d = recurse downCStat mapCStat cs create traverse (ce1, cs1) d
	where 
		traverse all@(ce1, cs1) d = merge2 d all
			(traverseCExpr ce1 d)
			(traverseCStat cs1 d)
		create (CCase _ _ x) (ce1, cs1) = CCase ce1 cs1 x

traverseCStat cs@(CCases ce1 ce2 cs1 _) d = recurse downCStat mapCStat cs create traverse (ce1, ce2, cs1) d
	where 
		traverse all@(ce1, ce2, cs1) d = merge3 d all
			(traverseCExpr ce1 d)
			(traverseCExpr ce2 d)
			(traverseCStat cs1 d)
		create (CCases _ _ _ x) (ce1, ce2, cs1) = CCases ce1 ce2 cs1 x

traverseCStat cs@(CDefault cs1 _) d = recurse downCStat mapCStat cs create traverse cs1 d
	where 	
		traverse = traverseCStat
		create (CDefault _ x) cs1 = CDefault cs1 x

traverseCStat cs@(CExpr ce1 _) d = recurse downCStat mapCStat cs create traverse ce1 d
	where 
		traverse = maybeTrav traverseCExpr
		create (CExpr _ x) ce1 = CExpr ce1 x

traverseCStat cs@(CCompound _ ccbis _) d = recurse downCStat mapCStat cs create traverse ccbis d
	where 
		traverse = mapTrav crossCBlockItem lastCBlockItem traverseCBlockItem
		create (CCompound x _ y) ccbis = CCompound x ccbis y

traverseCStat cs@(CIf ce1 cs1 cs2 _) d = recurse downCStat mapCStat cs create traverse (ce1, cs1, cs2) d
	where 
		traverse all@(ce1, cs1, cs2) d = merge3 d all
			(traverseCExpr ce1 d)
			(traverseCStat cs1 d)
			(maybeTrav traverseCStat cs2 d)
		create (CIf _ _ _ x) (ce1, cs1, cs2) = CIf ce1 cs1 cs2 x

traverseCStat cs@(CSwitch ce1 cs1 _) d = recurse downCStat mapCStat cs create traverse (ce1, cs1) d
	where 
		traverse all@(ce1, cs1) d = merge2 d all
			(traverseCExpr ce1 d)
			(traverseCStat cs1 d)
		create (CSwitch _ _ x) (ce1, cs1) = CSwitch ce1 cs1 x

traverseCStat cs@(CWhile ce1 cs1 _ _) d = recurse downCStat mapCStat cs create traverse (ce1, cs1) d
	where 
		traverse all@(ce1, cs1) d = merge2 d all
			(traverseCExpr ce1 d)
			(traverseCStat cs1 d)
		create (CWhile _ _ x y) (ce1, cs1) = CWhile ce1 cs1 x y

traverseCStat cs@(CFor (Left ce1) ce2 ce3 cs1 _) d = recurse downCStat mapCStat cs create traverse (ce1, ce2, ce3, cs1) d
	where 
		traverse all@(ce1, ce2, ce3, cs1) d = merge4 d all
			(maybeTrav traverseCExpr ce1 d)
			(maybeTrav traverseCExpr ce2 d)
			(maybeTrav traverseCExpr ce3 d)
			(traverseCStat cs1 d)
		create (CFor _ _ _ _ x) (ce1, ce2, ce3, cs1) = CFor (Left ce1) ce2 ce3 cs1 x

traverseCStat cs@(CFor (Right cd1) ce1 ce2 cs1 _) d = recurse downCStat mapCStat cs create traverse (cd1, ce1, ce2, cs1) d
	where 
		traverse all@(cd1, ce1, ce2, cs1) d = merge4 d all
			(traverseCDecl cd1 d)
			(maybeTrav traverseCExpr ce1 d)
			(maybeTrav traverseCExpr ce2 d)
			(traverseCStat cs1 d)
		create (CFor _ _ _ _ x) (cd1, ce1, ce2, cs1) = CFor (Right cd1) ce1 ce2 cs1 x

traverseCStat cs@(CGoto _ _) d = recurse downCStat mapCStat cs noCreate noTrav noChildren d

traverseCStat cs@(CGotoPtr ce1 _) d = recurse downCStat mapCStat cs create traverse ce1 d
	where 
		traverse = traverseCExpr
		create (CGotoPtr _ x) ce1 = CGotoPtr ce1 x

traverseCStat cs@(CCont _) d = recurse downCStat mapCStat cs noCreate noTrav noChildren d

traverseCStat cs@(CBreak _) d = recurse downCStat mapCStat cs noCreate noTrav noChildren d

traverseCStat cs@(CReturn ce1 _) d = recurse downCStat mapCStat cs create traverse ce1 d
	where 
		traverse = maybeTrav traverseCExpr
		create (CReturn _ x) ce1 = CReturn ce1 x

traverseCStat cs@(CAsm _ _) d = recurse downCStat mapCStat cs noCreate noTrav noChildren d

-- CBlockItem {{{1
traverseCBlockItem :: (DownVisitor d, UpVisitor d u) => CBlockItem -> d -> (Maybe CBlockItem, u)
traverseCBlockItem cbi@(CBlockStmt cs1) d = recurse downCBlockItem mapCBlockItem cbi create traverse cs1 d
	where
		traverse = traverseCStat
		create _ cs1 = CBlockStmt cs1

traverseCBlockItem cbi@(CBlockDecl cd) d = recurse downCBlockItem mapCBlockItem cbi create traverse cd d
	where
		traverse = traverseCDecl
		create _ cd = CBlockDecl cd

traverseCBlockItem cbi@(CNestedFunDef cfd) d = recurse downCBlockItem mapCBlockItem cbi create traverse cfd d
	where
		traverse = traverseCFunDef
		create _ cfd = CNestedFunDef cfd

-- dissolve {{{1

dissolveCInitList :: (DownVisitor d, UpVisitor d u) => CInitList -> d -> (Maybe CInitList, u)
dissolveCInitList = mapTrav crossCInitListMember lastCInitListMember traverse'
	where
		traverse' (x, ci) d = 
			case traverseCInit ci d of
				(Just y, u) -> (Just (x, y), u)
				(Nothing, u) -> (Nothing, u)

-- util {{{1
noCreate :: a -> () -> a
noCreate _ _ = error "function was not supposed to be called"

noTrav :: (DownVisitor d, UpVisitor d u) => () -> d -> (Maybe (), u) 
noTrav _ _ = (Nothing, mempty)

noChildren = ()

maybeTrav :: (DownVisitor d, UpVisitor d u) => (o->d->(Maybe o, u)) -> Maybe o -> d -> (Maybe (Maybe o), u)
maybeTrav _ Nothing _ = (Nothing, mempty)
maybeTrav f (Just o) d = 
	case f o d of
		(Nothing, u) -> (Nothing, u)
		(Just o', u) -> (Just $ Just o', u)

merge2 :: (DownVisitor d, UpVisitor d u) => d -> (a1,a2) -> (Maybe a1, u) -> (Maybe a2, u) -> (Maybe (a1,a2), u)
merge2 _ (a1,a2) (Nothing, u1) (Nothing, u2) = (Nothing, mappend u1 u2)
merge2 _ (a1,a2) (Just a1', u1) (Nothing, u2) = (Just (a1', a2), mappend u1 u2)
merge2 _ (a1,a2) (Nothing, u1) (Just a2', u2) = (Just (a1, a2'), mappend u1 u2)
merge2 _ (a1,a2) (Just a1', u1) (Just a2', u2) = (Just (a1', a2'), mappend u1 u2)

merge3 :: (DownVisitor d, UpVisitor d u) => d -> (a1,a2,a3) -> (Maybe a1, u) -> (Maybe a2, u) -> (Maybe a3, u) -> (Maybe (a1,a2,a3), u)
merge3 t (a1,a2,a3) ma ma2 ma3 =
	let part1 = merge2 t (a1,a2) ma ma2 in
	case merge2 t ((a1,a2),a3) part1 ma3 of
		(Nothing, u) -> (Nothing, u)
		(Just ((a1,a2),a3), u) -> (Just (a1,a2,a3), u)
	
merge4 :: (DownVisitor d, UpVisitor d u) => d -> (a1,a2,a3,a4) -> (Maybe a1, u) -> (Maybe a2, u) -> (Maybe a3, u) -> (Maybe a4, u) -> (Maybe (a1,a2,a3,a4), u)
merge4 d (a1,a2,a3,a4) ma1 ma2 ma3 ma4 = 
	let part1 = merge2 d (a1,a2) ma1 ma2 in
	let part2 = merge2 d (a3,a4) ma3 ma4 in
	case merge2 d ((a1,a2),(a3,a4)) part1 part2 of
		(Nothing, u) -> (Nothing, u)
		(Just ((a1,a2),(a3,a4)), u) -> (Just (a1,a2,a3,a4), u)
