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
			case upHandler outerObject downState' upStates of
				(Nothing, upState) -> (Nothing, upState)
				res -> res
		(Just innerObjects', upStates) ->
			let outerObject' = create outerObject innerObjects' in
			case upHandler outerObject' downState' upStates of
				(Nothing, upState) -> (Just outerObject', upState)
				res -> res

noCreate :: a -> () -> a
noCreate _ _ = error "function was not supposed to be called"

noTrav :: () -> d -> (Maybe (), [u]) 
noTrav _ _ = (Nothing, [])

noChildren = ()

mapTrav :: (o->d->(Maybe o, u)) -> [o] -> d -> (Maybe [o], [u])
mapTrav f os d = 
	let (maybeos, us) = unzip $ map (\o -> f o d) os in
	if any isJust maybeos
	then
		let os' = map (uncurry fromMaybe) $ zip os maybeos in
		(Just os', us)
	else
		(Nothing, us)

trav :: (o->d->(Maybe o, u)) -> o -> d -> (Maybe o, [u])
trav f o d =
	let (maybeo, u) = f o d in
	(maybeo, [u])

maybeTrav :: (o->d->(Maybe o, u)) -> Maybe o -> d -> (Maybe (Maybe o), [u])
maybeTrav _ Nothing _ = (Nothing, [])
maybeTrav f (Just o) d = let (maybeo,us) = trav f o d in (Just maybeo, us)

merge2 :: (a,b) -> (Maybe a, [u]) -> (Maybe b, [u]) -> (Maybe (a,b), [u])
merge2 (a,b) (Nothing, u1) (Nothing, u2) = (Nothing, u1 ++ u2)
merge2 (a,b) (Just a', u1) (Nothing, u2) = (Just (a', b), u1 ++ u2)
merge2 (a,b) (Nothing, u1) (Just b', u2) = (Just (a, b'), u1 ++ u2)
merge2 (a,b) (Just a', u1) (Just b', u2) = (Just (a', b'), u1 ++ u2)

merge3 :: (a,b,c) -> (Maybe a, [u]) -> (Maybe b, [u]) -> (Maybe c, [u]) -> (Maybe (a,b,c), [u])
merge3 (a,b,c) (Nothing, u1) (Nothing, u2) (Nothing, u3) = (Nothing, u1 ++ u2 ++ u3)
merge3 (a,b,c) (Just a', u1) (Nothing, u2) (Nothing, u3) = (Just (a', b, c), u1 ++ u2 ++ u3)
merge3 (a,b,c) (Nothing, u1) (Just b', u2) (Nothing, u3) = (Just (a, b', c), u1 ++ u2 ++ u3)
merge3 (a,b,c) (Just a', u1) (Just b', u2) (Nothing, u3) = (Just (a', b', c), u1 ++ u2 ++ u3)
merge3 (a,b,c) (Nothing, u1) (Nothing, u2) (Just c', u3) = (Just (a, b, c'), u1 ++ u2 ++ u3)
merge3 (a,b,c) (Just a', u1) (Nothing, u2) (Just c', u3) = (Just (a', b, c'), u1 ++ u2 ++ u3)
merge3 (a,b,c) (Nothing, u1) (Just b', u2) (Just c', u3) = (Just (a, b', c'), u1 ++ u2 ++ u3)
merge3 (a,b,c) (Just a', u1) (Just b', u2) (Just c', u3) = (Just (a', b', c'), u1 ++ u2 ++ u3)

traverseCTranslUnit :: (DownVisitor d, UpVisitor d u) => CTranslUnit -> d -> (Maybe CTranslUnit, u)
traverseCTranslUnit ctu@(CTranslUnit decls _) = recurse downCTranslUnit mapCTranslUnit ctu create traverse decls
	where 
		traverse = mapTrav traverseCExtDecl
		create (CTranslUnit _ ni) decls = CTranslUnit decls ni

traverseCFunDef = undefined
traverseCDeclr = undefined
traverseCExpr = undefined
traverseCInit = undefined
traverseCStat = undefined
traverseIdent = undefined

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
		traverse decls d = let (a,b) = mapTrav traverse' decls d in (a, concat b)
		traverse' all@(cd, ci, ce) d = merge3 all
			(maybeTrav traverseCDeclr cd d)
			(maybeTrav traverseCInit ci d)
			(maybeTrav traverseCExpr ce d)
		create (CDecl dss _ ni) decls = CDecl dss decls ni

traverseCDerivedDeclr :: (DownVisitor d, UpVisitor d u) => CDerivedDeclr -> d -> (Maybe CDerivedDeclr, u)
traverseCDerivedDeclr cdd@(CFunDeclr (Left ids) _ _) = recurse downCDerivedDeclr mapCDerivedDeclr cdd create traverse ids
	where
		traverse = mapTrav traverseIdent
		create (CFunDeclr _ as ni) ids = CFunDeclr (Left ids) as ni

-- traverseCDerivedDeclr cdd@(CFunDeclr (Right (cds, _)) _ _) = recurse downCDerivedDeclr mapCDerivedDeclr cdd create traverse cds
--   where 
--     traverse = mapTrav traverseCDecl
--     create (CFunDeclr (Right (_, flag)) as ni) ts = CFunDeclr (Right ((map cast' ts), flag)) as ni

-- traverseCDerivedDeclr cdd = recurse downCDerivedDeclr mapCDerivedDeclr cdd noCreate noTrav noChildren

-- traverseCFunDef :: (DownVisitor d, UpVisitor d u) => CFunDef -> d -> (Maybe CFunDef, u)
-- traverseCFunDef cfd@(CFunDef _ cdr cds cst _) = recurse downCFunDef mapCFunDef cfd create traverse [(cdr, cds, cst)]
--   where 
--     traverse tr d = concatMap (traverse' d) tr
--     traverse' d (t1, t2, t3) =
--              trav traverseCDeclr t1 d 
--           ++ mapTrav traverseCDecl t2 d 
--           ++ trav traverseCStat t3 d
--     create (CFunDef dss _ _ _ ni) [cdr, cds, cst] = CFunDef dss (cast' cdr) (map cast' cds) (cast' cst) ni

-- traverseCDeclr :: (DownVisitor d, UpVisitor d u) => CDeclr -> d -> u
-- traverseCDeclr cdr@(CDeclr id cdds _ _ _) = recurse downCDeclr upCDeclr traverse cdr (id, cdds)
--   where traverse (id, cdds) d = 
--              maybeTrav traverseIdent id d 
--           ++ mapTrav traverseCDerivedDeclr cdds d

-- traverseCInit :: (DownVisitor d, UpVisitor d u) => CInit -> d -> u
-- traverseCInit ci@(CInitExpr ce _) = recurse downCInit upCInit traverse ci ce
--   where traverse = trav traverseCExpr

-- traverseCInit ci@(CInitList cis _) = recurse downCInit upCInit dissolveCInitList ci cis
--   where traverse cis = mapTrav traverseCInit $ map snd cis

-- traverseIdent :: (DownVisitor d, UpVisitor d u) => Ident -> d -> u
-- traverseIdent id = recurse downIdent upIdent noTrav id Nothing

-- dissolveCInitList :: (DownVisitor d, UpVisitor d u) => CInitList -> d -> [u]
-- dissolveCInitList cis = mapTrav traverseCInit $ map snd cis

-- traverseCExpr :: (DownVisitor d, UpVisitor d u) => CExpr -> d -> u
-- traverseCExpr ce@(CComma ces _) = recurse downCExpr upCExpr traverse ce ces
--   where traverse = mapTrav traverseCExpr

-- traverseCExpr ce@(CAssign _ ce1 ce2 _) = recurse downCExpr upCExpr traverse ce (ce1, ce2)
--   where traverse (ce1, ce2) d = 
--              trav traverseCExpr ce1 d 
--           ++ trav traverseCExpr ce2 d

-- traverseCExpr ce@(CCond ce1 ce2 ce3 _) = recurse downCExpr upCExpr traverse ce (ce1, ce2, ce3)
--   where traverse (ce1, ce2, ce3) d = 
--              trav traverseCExpr ce1 d 
--           ++ maybeTrav traverseCExpr ce2 d 
--           ++ trav traverseCExpr ce3 d

-- traverseCExpr ce@(CBinary _ ce1 ce2 _) = recurse downCExpr upCExpr traverse ce (ce1, ce2)
--   where traverse (ce1, ce2) d = 
--              trav traverseCExpr ce1 d 
--           ++ trav traverseCExpr ce2 d

-- traverseCExpr ce@(CCast cd ce1 _) = recurse downCExpr upCExpr traverse ce (cd, ce1)
--   where traverse (cd, ce1) d = 
--              trav traverseCDecl cd d 
--           ++ trav traverseCExpr ce1 d

-- traverseCExpr ce@(CUnary _ ce1 _) = recurse downCExpr upCExpr traverse ce ce1
--   where traverse = trav traverseCExpr
--   
-- traverseCExpr ce@(CSizeofExpr ce1 _) = recurse downCExpr upCExpr traverse ce ce1
--   where traverse = trav traverseCExpr

-- traverseCExpr ce@(CSizeofType cd _) = recurse downCExpr upCExpr traverse ce cd
--   where traverse = trav traverseCDecl

-- traverseCExpr ce@(CAlignofExpr ce1 _) = recurse downCExpr upCExpr traverse ce ce1
--   where traverse = trav traverseCExpr

-- traverseCExpr ce@(CAlignofType cd _) = recurse downCExpr upCExpr traverse ce cd
--   where traverse = trav traverseCDecl 

-- traverseCExpr ce@(CComplexReal ce1 _) = recurse downCExpr upCExpr traverse ce ce1
--   where traverse = trav traverseCExpr

-- traverseCExpr ce@(CComplexImag ce1 _) = recurse downCExpr upCExpr traverse ce ce1
--   where traverse = trav traverseCExpr

-- traverseCExpr ce@(CIndex ce1 ce2 _) = recurse downCExpr upCExpr traverse ce (ce1, ce2)
--   where traverse (ce1, ce2) d = 
--              trav traverseCExpr ce1 d 
--           ++ trav traverseCExpr ce2 d

-- traverseCExpr ce@(CCall ce1 ces _) = recurse downCExpr upCExpr traverse ce (ce1, ces)
--   where traverse (ce1, ces) d = 
--              trav traverseCExpr ce1 d 
--           ++ mapTrav traverseCExpr ces d

-- traverseCExpr ce@(CMember ce1 id _ _) = recurse downCExpr upCExpr traverse ce (ce1, id)
--   where traverse (ce1, id) d = 
--              trav traverseCExpr ce1 d 
--           ++ trav traverseIdent id d

-- traverseCExpr ce@(CVar id _) = recurse downCExpr upCExpr traverse ce id
--   where traverse = trav traverseIdent

-- traverseCExpr ce@(CConst _) = recurse downCExpr upCExpr noTrav ce Nothing

-- traverseCExpr ce@(CCompoundLit cd cil _) = recurse downCExpr upCExpr traverse ce (cd, cil)
--   where traverse (cd, cil) d =
--              trav traverseCDecl cd d 
--           ++ dissolveCInitList cil d

-- traverseCExpr ce@(CStatExpr cst _) = recurse downCExpr upCExpr traverse ce cst
--   where traverse = trav traverseCStat

-- traverseCExpr ce@(CLabAddrExpr id _) = recurse downCExpr upCExpr traverse ce id
--   where traverse = trav traverseIdent

-- traverseCExpr ce@(CBuiltinExpr _) = recurse downCExpr upCExpr noTrav ce Nothing

-- dissolveCBlockItem :: (DownVisitor d, UpVisitor d u) => CBlockItem -> d -> [u]
-- dissolveCBlockItem (CBlockStmt cs1) = trav traverseCStat cs1
-- dissolveCBlockItem (CBlockDecl cd) = trav traverseCDecl cd
-- dissolveCBlockItem (CNestedFunDef cfd) = trav traverseCFunDef cfd

-- traverseCStat :: (DownVisitor d, UpVisitor d u) => CStat -> d -> u
-- traverseCStat cs@(CLabel _ cs1 _ _) = recurse downCStat upCStat traverse cs cs1
--   where traverse = trav traverseCStat

-- traverseCStat cs@(CCase ce1 cs1 _) = recurse downCStat upCStat traverse cs (ce1, cs1)
--   where traverse (ce1, cs1) d = 
--              trav traverseCExpr ce1 d 
--           ++ trav traverseCStat cs1 d

-- traverseCStat cs@(CCases ce1 ce2 cs1 _) = recurse downCStat upCStat traverse cs (ce1, ce2, cs1)
--   where traverse (ce1, ce2, cs1) d =
--              trav traverseCExpr ce1 d 
--           ++ trav traverseCExpr ce2 d 
--           ++ trav traverseCStat cs1 d

-- traverseCStat cs@(CDefault cs1 _) = recurse downCStat upCStat traverse cs cs1
--   where traverse = trav traverseCStat

-- traverseCStat cs@(CExpr ce1 _) = recurse downCStat upCStat traverse cs ce1
--   where traverse = maybeTrav traverseCExpr

-- traverseCStat cs@(CCompound _ ccbis _) = recurse downCStat upCStat traverse cs ccbis
--   where traverse ccbis d = concatMap (flip dissolveCBlockItem d) ccbis

-- traverseCStat cs@(CIf ce1 cs1 cs2 _) = recurse downCStat upCStat traverse cs (ce1, cs1, cs2)
--   where traverse (ce1, cs1, cs2) d = 
--              trav traverseCExpr ce1 d
--           ++ trav traverseCStat cs1 d
--           ++ maybeTrav traverseCStat cs2 d

-- traverseCStat cs@(CSwitch ce1 cs1 _) = recurse downCStat upCStat traverse cs (ce1, cs1)
--   where traverse (ce1, cs1) d =
--              trav traverseCExpr ce1 d
--           ++ trav traverseCStat cs1 d

-- traverseCStat cs@(CWhile ce1 cs1 _ _) = recurse downCStat upCStat traverse cs (ce1, cs1)
--   where traverse (ce1, cs1) d =
--              trav traverseCExpr ce1 d
--           ++ trav traverseCStat cs1 d

-- traverseCStat cs@(CFor (Left ce1) ce2 ce3 cs1 _) = recurse downCStat upCStat traverse cs (ce1, ce2, ce3, cs1)
--   where traverse (ce1, ce2, ce3, cs1) d =
--              maybeTrav traverseCExpr ce1 d
--           ++ maybeTrav traverseCExpr ce2 d
--           ++ maybeTrav traverseCExpr ce3 d
--           ++ trav traverseCStat cs1 d

-- traverseCStat cs@(CFor (Right cd1) ce1 ce2 cs1 _) = recurse downCStat upCStat traverse cs (cd1, ce1, ce2, cs1)
--   where traverse (cd1, ce1, ce2, cs1) d =
--              trav traverseCDecl cd1 d
--           ++ maybeTrav traverseCExpr ce1 d
--           ++ maybeTrav traverseCExpr ce2 d
--           ++ trav traverseCStat cs1 d

-- traverseCStat cs@(CGoto _ _) = recurse downCStat upCStat noTrav cs Nothing

-- traverseCStat cs@(CGotoPtr ce1 _) = recurse downCStat upCStat traverse cs ce1
--   where traverse = trav traverseCExpr

-- traverseCStat cs@(CCont _) = recurse downCStat upCStat noTrav cs Nothing

-- traverseCStat cs@(CBreak _) = recurse downCStat upCStat noTrav cs Nothing

-- traverseCStat cs@(CReturn ce1 _) = recurse downCStat upCStat traverse cs ce1
--   where traverse = maybeTrav traverseCExpr

-- traverseCStat cs@(CAsm _ _) = recurse downCStat upCStat noTrav cs Nothing
