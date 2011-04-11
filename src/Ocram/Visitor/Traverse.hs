module Ocram.Visitor.Traverse (
	 traverseCDecl
	,traverseCDeclr
	,traverseCDerivedDeclr
	,traverseCExpr
	,traverseCExtDecl
	,traverseCFunDef
	,traverseCInit
	,traverseCStat
	,traverseCTranslUnit
	,traverseIdent
) where

import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Ident (Ident)
import Ocram.Visitor.Visitor
import Data.Maybe (isJust, fromMaybe)

recurse :: 
	   (o -> d -> d) -- down handler
	-> (o -> d -> [u] -> (Maybe o, u)) -- up handler
	-> o -- outer object
	-> (o -> i -> o) -- create
	-> (i -> d -> (Maybe i, [u])) -- traverse
	-> i -- inner object tuple
	-> d -- down state
	-> (Maybe o, u) -- (maybe new outer object, up state)
recurse downHandler upHandler outerObject create traverse innerObjects downState =
  let downState' = downHandler outerObject downState in
	case traverse innerObjects downState' of
		(Nothing, upStates) -> 
			upHandler outerObject downState' upStates
			-- case upHandler outerObject downState' upStates of
			--   (Nothing, upState) -> (Nothing, upState)
			--   res -> res
		(Just innerObjects', upStates) ->
			let outerObject' = create outerObject innerObjects' in
			case upHandler outerObject' downState' upStates of
				(Nothing, upState) -> (Just outerObject', upState)
				res -> res

traverseCTranslUnit :: (DownVisitor d, UpVisitor d u) => CTranslUnit -> d -> (Maybe CTranslUnit, u)
traverseCTranslUnit ctu@(CTranslUnit decls _) = recurse downCTranslUnit mapCTranslUnit ctu create traverse decls
	where 
		traverse = mapTrav traverseCExtDecl
		create (CTranslUnit _ ni) decls = CTranslUnit decls ni

traverseCExtDecl :: (DownVisitor d, UpVisitor d u) => CExtDecl -> d -> (Maybe CExtDecl, u)
traverseCExtDecl ced@(CDeclExt cd) = recurse downCExtDecl mapCExtDecl ced create traverse cd
	where 
		traverse = trav traverseCDecl
		create _  cd = CDeclExt cd

traverseCExtDecl ced@(CFDefExt cfd) = recurse downCExtDecl mapCExtDecl ced create traverse cfd
	where 
		traverse = trav traverseCFunDef
		create _ cfd = CFDefExt cfd

traverseCExtDecl ced@(CAsmExt _ _) = recurse downCExtDecl mapCExtDecl ced noCreate noTrav noChildren

traverseCDecl :: (DownVisitor d, UpVisitor d u) => CDecl -> d -> (Maybe CDecl, u)
traverseCDecl cd@(CDecl _ decls _) = recurse downCDecl mapCDecl cd create traverse decls
	where 
		traverse = recTrav traverse'
		traverse' all@(cd, ci, ce) d = merge3 all
			(maybeTrav traverseCDeclr cd d)
			(maybeTrav traverseCInit ci d)
			(maybeTrav traverseCExpr ce d)
		create (CDecl x _ y) decls = CDecl x decls y

traverseCDerivedDeclr :: (DownVisitor d, UpVisitor d u) => CDerivedDeclr -> d -> (Maybe CDerivedDeclr, u)
traverseCDerivedDeclr cdd@(CFunDeclr (Left ids) _ _) = recurse downCDerivedDeclr mapCDerivedDeclr cdd create traverse ids
	where
		traverse = mapTrav traverseIdent
		create (CFunDeclr _ x y) ids = CFunDeclr (Left ids) x y

traverseCDerivedDeclr cdd@(CFunDeclr (Right (cds, _)) _ _) = recurse downCDerivedDeclr mapCDerivedDeclr cdd create traverse cds
	where 
		traverse = mapTrav traverseCDecl
		create (CFunDeclr (Right (_, x)) y z) ods = CFunDeclr (Right (ods, x)) y z

traverseCDerivedDeclr cdd = recurse downCDerivedDeclr mapCDerivedDeclr cdd noCreate noTrav noChildren

traverseCFunDef :: (DownVisitor d, UpVisitor d u) => CFunDef -> d -> (Maybe CFunDef, u)
traverseCFunDef cfd@(CFunDef _ cdr cds cst _) = recurse downCFunDef mapCFunDef cfd create traverse (cdr, cds, cst)
	where 
		traverse all@(cdr, t2, t3) d = merge3 all
			(trav traverseCDeclr cdr d)
			(mapTrav traverseCDecl cds d)
			(trav traverseCStat t3 d)
		create (CFunDef x _ _ _ y) (cdr, cds, cst) = CFunDef x cdr cds cst y

traverseCDeclr :: (DownVisitor d, UpVisitor d u) => CDeclr -> d -> (Maybe CDeclr, u)
traverseCDeclr cdr@(CDeclr id cdds _ _ _) = recurse downCDeclr mapCDeclr cdr create traverse (id, cdds)
	where 
		traverse all@(id, cdds) d = merge2 all
			(maybeTrav traverseIdent id d)
			(mapTrav traverseCDerivedDeclr cdds d)
		create (CDeclr _ _ x y z) (id, cdds) = CDeclr id cdds x y z

traverseCInit :: (DownVisitor d, UpVisitor d u) => CInit -> d -> (Maybe CInit, u)
traverseCInit ci@(CInitExpr ce _) = recurse downCInit mapCInit ci create traverse ce
	where 
		traverse = trav traverseCExpr
		create (CInitExpr _ x) ce = CInitExpr ce x

traverseCInit ci@(CInitList cis _) = recurse downCInit mapCInit ci create dissolveCInitList cis
	where 
		create (CInitList _ x) cis = CInitList cis x


traverseIdent :: (DownVisitor d, UpVisitor d u) => Ident -> d -> (Maybe Ident, u)
traverseIdent id = recurse downIdent mapIdent id noCreate noTrav noChildren

traverseCExpr :: (DownVisitor d, UpVisitor d u) => CExpr -> d -> (Maybe CExpr, u)
traverseCExpr ce@(CComma ces _) = recurse downCExpr mapCExpr ce create traverse ces
	where
		traverse = mapTrav traverseCExpr
		create (CComma _ x) ces = CComma ces x

traverseCExpr ce@(CAssign _ ce1 ce2 _) = recurse downCExpr mapCExpr ce create traverse (ce1, ce2)
	where 
		traverse all@(ce1, ce2) d = merge2 all
			(trav traverseCExpr ce1 d)
			(trav traverseCExpr ce2 d)
		create (CAssign x _ _ y) (ce1, ce2) = CAssign x ce1 ce2 y

traverseCExpr ce@(CCond ce1 ce2 ce3 _) = recurse downCExpr mapCExpr ce create traverse (ce1, ce2, ce3)
	where 
		traverse all@(ce1, ce2, ce3) d = merge3 all
			(trav traverseCExpr ce1 d)
			(maybeTrav traverseCExpr ce2 d)
			(trav traverseCExpr ce3 d)
		create (CCond _ _ _ x) (ce1, ce2, ce3) = CCond ce1 ce2 ce3 x

traverseCExpr ce@(CBinary _ ce1 ce2 _) = recurse downCExpr mapCExpr ce create traverse (ce1, ce2)
	where 
		traverse all@(ce1, ce2) d = merge2 all
			(trav traverseCExpr ce1 d)
			(trav traverseCExpr ce2 d)
		create (CBinary x _ _ y) (ce1, ce2) = CBinary x ce1 ce2 y

traverseCExpr ce@(CCast cd ce1 _) = recurse downCExpr mapCExpr ce create traverse (cd, ce1)
	where 
		traverse all@(cd, ce1) d = merge2 all
			(trav traverseCDecl cd d)
			(trav traverseCExpr ce1 d)
		create (CCast _ _ x) (cd, ce1) = CCast cd ce1 x

traverseCExpr ce@(CUnary _ ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = trav traverseCExpr
		create (CUnary x _ y) ce1 = CUnary x ce1 y
	
traverseCExpr ce@(CSizeofExpr ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = trav traverseCExpr
		create (CSizeofExpr _ x) ce1 = CSizeofExpr ce1 x

traverseCExpr ce@(CSizeofType cd _) = recurse downCExpr mapCExpr ce create traverse cd
	where 
		traverse = trav traverseCDecl
		create (CSizeofType _ x) cd = CSizeofType cd x

traverseCExpr ce@(CAlignofExpr ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = trav traverseCExpr
		create (CAlignofExpr _ x) ce1 = CAlignofExpr ce1 x

traverseCExpr ce@(CAlignofType cd _) = recurse downCExpr mapCExpr ce create traverse cd
	where 
		traverse = trav traverseCDecl 
		create (CAlignofType _ x) cd = CAlignofType cd x

traverseCExpr ce@(CComplexReal ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = trav traverseCExpr
		create (CComplexReal _ x) ce1 = CComplexReal ce1 x

traverseCExpr ce@(CComplexImag ce1 _) = recurse downCExpr mapCExpr ce create traverse ce1
	where 
		traverse = trav traverseCExpr
		create (CComplexImag _ x) ce1 = CComplexImag ce1 x

traverseCExpr ce@(CIndex ce1 ce2 _) = recurse downCExpr mapCExpr ce create traverse (ce1, ce2)
	where 
		traverse all@(ce1, ce2) d = merge2 all
			(trav traverseCExpr ce1 d)
			(trav traverseCExpr ce2 d)
		create (CIndex _ _ x) (ce1, ce2) = CIndex ce1 ce2 x

traverseCExpr ce@(CCall ce1 ces _) = recurse downCExpr mapCExpr ce create traverse (ce1, ces)
	where 
		traverse all@(ce1, ces) d = merge2 all
			(trav traverseCExpr ce1 d)
			(mapTrav traverseCExpr ces d)
		create (CCall _ _ x) (ce1, ces) = CCall ce1 ces x

traverseCExpr ce@(CMember ce1 id _ _) = recurse downCExpr mapCExpr ce create traverse (ce1, id)
	where 
		traverse all@(ce1, id) d = merge2 all
			(trav traverseCExpr ce1 d)
			(trav traverseIdent id d)
		create (CMember _ _ x y) (ce1, id) = CMember ce1 id x y

traverseCExpr ce@(CVar id _) = recurse downCExpr mapCExpr ce create traverse id
	where 
		traverse = trav traverseIdent
		create (CVar _ x) id = CVar id x

traverseCExpr ce@(CConst _) = recurse downCExpr mapCExpr ce noCreate noTrav noChildren

traverseCExpr ce@(CCompoundLit cd cil _) = recurse downCExpr mapCExpr ce create traverse (cd, cil)
	where 
		traverse all@(cd, cil) d = merge2 all
			(trav traverseCDecl cd d)
			(dissolveCInitList cil d)
		create (CCompoundLit _ _ x) (cd, cil) = CCompoundLit cd cil x

traverseCExpr ce@(CStatExpr cst _) = recurse downCExpr mapCExpr ce create traverse cst
	where 
		traverse = trav traverseCStat
		create (CStatExpr _ x) cst = CStatExpr cst x

traverseCExpr ce@(CLabAddrExpr id _) = recurse downCExpr mapCExpr ce create traverse id
	where 
		traverse = trav traverseIdent
		create (CLabAddrExpr _ x) id = CLabAddrExpr id x

traverseCExpr ce@(CBuiltinExpr _) = recurse downCExpr mapCExpr ce noCreate noTrav noChildren

traverseCStat :: (DownVisitor d, UpVisitor d u) => CStat -> d -> (Maybe CStat, u)
traverseCStat cs@(CLabel _ cs1 _ _) = recurse downCStat mapCStat cs create traverse cs1
	where 
		traverse = trav traverseCStat
		create (CLabel x _ y z) cs1 = CLabel x cs1 y z

traverseCStat cs@(CCase ce1 cs1 _) = recurse downCStat mapCStat cs create traverse (ce1, cs1)
	where 
		traverse all@(ce1, cs1) d = merge2 all
			(trav traverseCExpr ce1 d)
			(trav traverseCStat cs1 d)
		create (CCase _ _ x) (ce1, cs1) = CCase ce1 cs1 x

traverseCStat cs@(CCases ce1 ce2 cs1 _) = recurse downCStat mapCStat cs create traverse (ce1, ce2, cs1)
	where 
		traverse all@(ce1, ce2, cs1) d = merge3 all
			(trav traverseCExpr ce1 d)
			(trav traverseCExpr ce2 d)
			(trav traverseCStat cs1 d)
		create (CCases _ _ _ x) (ce1, ce2, cs1) = CCases ce1 ce2 cs1 x

traverseCStat cs@(CDefault cs1 _) = recurse downCStat mapCStat cs create traverse cs1
	where 	
		traverse = trav traverseCStat
		create (CDefault _ x) cs1 = CDefault cs1 x

traverseCStat cs@(CExpr ce1 _) = recurse downCStat mapCStat cs create traverse ce1
	where 
		traverse = maybeTrav traverseCExpr
		create (CExpr _ x) ce1 = CExpr ce1 x

traverseCStat cs@(CCompound _ ccbis _) = recurse downCStat mapCStat cs create traverse ccbis
	where 
		traverse = recTrav dissolveCBlockItem
		create (CCompound x _ y) ccbis = CCompound x ccbis y

traverseCStat cs@(CIf ce1 cs1 cs2 _) = recurse downCStat mapCStat cs create traverse (ce1, cs1, cs2)
	where 
		traverse all@(ce1, cs1, cs2) d = merge3 all
			(trav traverseCExpr ce1 d)
			(trav traverseCStat cs1 d)
			(maybeTrav traverseCStat cs2 d)
		create (CIf _ _ _ x) (ce1, cs1, cs2) = CIf ce1 cs1 cs2 x

traverseCStat cs@(CSwitch ce1 cs1 _) = recurse downCStat mapCStat cs create traverse (ce1, cs1)
	where 
		traverse all@(ce1, cs1) d = merge2 all
			(trav traverseCExpr ce1 d)
			(trav traverseCStat cs1 d)
		create (CSwitch _ _ x) (ce1, cs1) = CSwitch ce1 cs1 x

traverseCStat cs@(CWhile ce1 cs1 _ _) = recurse downCStat mapCStat cs create traverse (ce1, cs1)
	where 
		traverse all@(ce1, cs1) d = merge2 all
			(trav traverseCExpr ce1 d)
			(trav traverseCStat cs1 d)
		create (CWhile _ _ x y) (ce1, cs1) = CWhile ce1 cs1 x y

traverseCStat cs@(CFor (Left ce1) ce2 ce3 cs1 _) = recurse downCStat mapCStat cs create traverse (ce1, ce2, ce3, cs1)
	where 
		traverse all@(ce1, ce2, ce3, cs1) d = merge4 all
			(maybeTrav traverseCExpr ce1 d)
			(maybeTrav traverseCExpr ce2 d)
			(maybeTrav traverseCExpr ce3 d)
			(trav traverseCStat cs1 d)
		create (CFor _ _ _ _ x) (ce1, ce2, ce3, cs1) = CFor (Left ce1) ce2 ce3 cs1 x

traverseCStat cs@(CFor (Right cd1) ce1 ce2 cs1 _) = recurse downCStat mapCStat cs create traverse (cd1, ce1, ce2, cs1)
	where 
		traverse all@(cd1, ce1, ce2, cs1) d = merge4 all
			(trav traverseCDecl cd1 d)
			(maybeTrav traverseCExpr ce1 d)
			(maybeTrav traverseCExpr ce2 d)
			(trav traverseCStat cs1 d)
		create (CFor _ _ _ _ x) (cd1, ce1, ce2, cs1) = CFor (Right cd1) ce1 ce2 cs1 x

traverseCStat cs@(CGoto _ _) = recurse downCStat mapCStat cs noCreate noTrav noChildren

traverseCStat cs@(CGotoPtr ce1 _) = recurse downCStat mapCStat cs create traverse ce1
	where 
		traverse = trav traverseCExpr
		create (CGotoPtr _ x) ce1 = CGotoPtr ce1 x

traverseCStat cs@(CCont _) = recurse downCStat mapCStat cs noCreate noTrav noChildren

traverseCStat cs@(CBreak _) = recurse downCStat mapCStat cs noCreate noTrav noChildren

traverseCStat cs@(CReturn ce1 _) = recurse downCStat mapCStat cs create traverse ce1
	where 
		traverse = maybeTrav traverseCExpr
		create (CReturn _ x) ce1 = CReturn ce1 x

traverseCStat cs@(CAsm _ _) = recurse downCStat mapCStat cs noCreate noTrav noChildren

dissolveCBlockItem :: (DownVisitor d, UpVisitor d u) => CBlockItem -> d -> (Maybe CBlockItem, [u])
dissolveCBlockItem (CBlockStmt cs1) d = 
	case trav traverseCStat cs1 d of
		(Just cs, us) -> (Just (CBlockStmt cs), us)
		(Nothing, us) -> (Nothing, us)

dissolveCBlockItem (CBlockDecl cd) d = 
		case trav traverseCDecl cd d of
			(Just cbd, us) -> (Just (CBlockDecl cbd), us)
			(Nothing, us) -> (Nothing, us)

dissolveCBlockItem (CNestedFunDef cfd) d = 
		case trav traverseCFunDef cfd d of
			(Just cfd, us) -> (Just (CNestedFunDef cfd), us)
			(Nothing, us) -> (Nothing, us)


dissolveCInitList :: (DownVisitor d, UpVisitor d u) => CInitList -> d -> (Maybe CInitList, [u])
dissolveCInitList = recTrav traverse'
	where
		traverse' (x, ci) d = 
			case trav traverseCInit ci d of
				(Just y, us) -> (Just (x, y), us)
				(Nothing, us) -> (Nothing, us)

noCreate :: a -> () -> a
noCreate _ _ = error "function was not supposed to be called"

noTrav :: () -> d -> (Maybe (), [u]) 
noTrav _ _ = (Nothing, [])

noChildren = ()

trav :: (o->d->(Maybe o, u)) -> o -> d -> (Maybe o, [u])
trav f o d =
	let (maybeo, u) = f o d in
	(maybeo, [u])

mapTrav :: (o->d->(Maybe o, u)) -> [o] -> d -> (Maybe [o], [u])
mapTrav f os d = 
	let (maybeos, us) = unzip $ map (\o -> f o d) os in
	if any isJust maybeos
	then
		let os' = map (uncurry fromMaybe) $ zip os maybeos in
		(Just os', us)
	else
		(Nothing, us)

recTrav :: (o->d->(Maybe o, [u])) -> [o] -> d -> (Maybe [o], [u])
recTrav f os d =
	let (maybeos, uss) = mapTrav f os d
	in (maybeos, concat uss)

maybeTrav :: (o->d->(Maybe o, u)) -> Maybe o -> d -> (Maybe (Maybe o), [u])
maybeTrav _ Nothing _ = (Nothing, [])
maybeTrav f (Just o) d = 
	case trav f o d of
		(Nothing, us) -> (Nothing, us)
		(Just os, us) -> (Just $ Just os, us)

merge2 :: (a,b) -> (Maybe a, [u]) -> (Maybe b, [u]) -> (Maybe (a,b), [u])
merge2 (a,b) (Nothing, u1) (Nothing, u2) = (Nothing, u1 ++ u2)
merge2 (a,b) (Just a', u1) (Nothing, u2) = (Just (a', b), u1 ++ u2)
merge2 (a,b) (Nothing, u1) (Just b', u2) = (Just (a, b'), u1 ++ u2)
merge2 (a,b) (Just a', u1) (Just b', u2) = (Just (a', b'), u1 ++ u2)

merge3 :: (a,b,c) -> (Maybe a, [u]) -> (Maybe b, [u]) -> (Maybe c, [u]) -> (Maybe (a,b,c), [u])
merge3 (a,b,c) ma mb mc =
	let part1 = merge2 (a,b) ma mb in
	case merge2 ((a,b),c) part1 mc of
		(Nothing, us) -> (Nothing, us)
		(Just ((a,b),c), us) -> (Just (a,b,c), us)
	
merge4 :: (a,b,c,d) -> (Maybe a, [u]) -> (Maybe b, [u]) -> (Maybe c, [u]) -> (Maybe d, [u]) -> (Maybe (a,b,c,d), [u])
merge4 (a,b,c,d) ma mb mc md = 
	let part1 = merge2 (a,b) ma mb in
	let part2 = merge2 (c,d) mc md in
	case merge2 ((a,b),(c,d)) part1 part2 of
		(Nothing, us) -> (Nothing, us)
		(Just ((a,b),(c,d)), us) -> (Just (a,b,c,d), us)
