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
) where

-- import {{{1
import Ocram.Visitor.Visitor
import Ocram.Util (mapt2)

import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Ident (Ident)

import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (mappend, mempty, Monoid)

import Debug.Trace (trace)

-- types {{{1
type DownHandler o d = o -> d -> d
type UpHandler o d u = o -> d -> u -> (o, u)
type Traverse o d u = o -> d -> (o, u)
type Recurse o d u = Traverse o d u

type CrossHandler o d u = o -> d -> u -> ([o], d, u)

-- util {{{2
maybeTrav :: (Monoid u) => Traverse o d u -> Traverse (Maybe o) d u
maybeTrav _ Nothing _ = (Nothing, mempty)
maybeTrav trav (Just o) d = mapt2 (Just, id) $ trav o d

maybeDown :: DownHandler o d -> DownHandler (Maybe o) d
maybeDown _ Nothing d = d
maybeDown downh (Just o) d = downh o d

maybeUp :: (Monoid u) => UpHandler o d u -> UpHandler (Maybe o) d u
maybeUp _ Nothing _ _ = (Nothing, mempty)
maybeUp uph (Just o) d u = mapt2 (Just, id) $ uph o d u

-- trav {{{1
trav ::
	   DownHandler o d
	-> UpHandler o d u
	-> Recurse o d u
	-> Traverse o d u
trav downh uph recf o d =
	let
		d' = downh o d
		(o', u') = recf o d'
	in
		uph o' d' u'

listTrav :: (Monoid u) =>
	   DownHandler o d
	-> UpHandler o d u
	-> Recurse o d u
	-> Traverse [o] d u
	-> Traverse [o] d u
listTrav _ _ _ _ [] _ = ([], mempty)
listTrav downh uph recf travf (o:os) d =
	let
		d' = downh o d
		(o', u) = recf o d'
		(o'', u') = uph o' d' u
		(os', us) = travf os d'
	in
		(o'':os', us `mappend` u')

-- public trav functionss {{{2
traverseCTranslUnit :: (DownVisitor d, UpVisitor d u) => Traverse CTranslUnit d u
traverseCTranslUnit = trav downCTranslUnit upCTranslUnit recurseCTranslUnit

traverseCExtDecl :: (DownVisitor d, UpVisitor d u) => Traverse CExtDecl d u
traverseCExtDecl = trav downCExtDecl upCExtDecl recurseCExtDecl

traverseCDecl :: (DownVisitor d, UpVisitor d u) => Traverse CDecl d u
traverseCDecl = trav downCDecl upCDecl recurseCDecl

traverseCDerivedDeclr :: (DownVisitor d, UpVisitor d u) => Traverse CDerivedDeclr d u
traverseCDerivedDeclr = trav downCDerivedDeclr upCDerivedDeclr recurseCDerivedDeclr

traverseCFunDef :: (DownVisitor d, UpVisitor d u) => Traverse CFunDef d u
traverseCFunDef = trav downCFunDef upCFunDef recurseCFunDef

traverseCDeclr :: (DownVisitor d, UpVisitor d u) => Traverse CDeclr d u
traverseCDeclr = trav downCDeclr upCDeclr recurseCDeclr

traverseCInit :: (DownVisitor d, UpVisitor d u) => Traverse CInit d u
traverseCInit = trav downCInit upCInit recurseCInit

traverseCExpr :: (DownVisitor d, UpVisitor d u) => Traverse CExpr d u
traverseCExpr = trav downCExpr upCExpr recurseCExpr

traverseCStat :: (DownVisitor d, UpVisitor d u) => Traverse CStat d u
traverseCStat = trav downCStat upCStat recurseCStat

traverseCBlockItem :: (DownVisitor d, UpVisitor d u) => Traverse CBlockItem d u
traverseCBlockItem = trav downCBlockItem upCBlockItem recurseCBlockItem

-- list trav functions {{{2
traverseCExtDecls :: (DownVisitor d, UpVisitor d u) => Traverse [CExtDecl] d u
traverseCExtDecls = listTrav downCExtDecl upCExtDecl recurseCExtDecl traverseCExtDecls

traverseCDecls :: (DownVisitor d, UpVisitor d u) => Traverse [CDecl] d u
traverseCDecls = listTrav downCDecl upCDecl recurseCDecl traverseCDecls

traverseCExprs :: (DownVisitor d, UpVisitor d u) => Traverse [CExpr] d u
traverseCExprs = listTrav downCExpr upCExpr recurseCExpr traverseCExprs

-- traverseCBlockItems :: (DownVisitor d, UpVisitor d u) => Traverse [CBlockItem] d u
-- traverseCBlockItems = listTrav downCBlockItem upCBlockItem recurseCBlockItem traverseCBlockItem

traverseCDerivedDeclrs :: (DownVisitor d, UpVisitor d u) => Traverse [CDerivedDeclr] d u
traverseCDerivedDeclrs = listTrav downCDerivedDeclr upCDerivedDeclr recurseCDerivedDeclr traverseCDerivedDeclrs

traverseCInitListMembers :: (DownVisitor d, UpVisitor d u) => Traverse [([CDesignator], CInit)] d u
traverseCInitListMembers [] _ = ([], mempty)
traverseCInitListMembers ((x, o):os) d =
	let
		d' = downCInit o d
		(o', u) = recurseCInit o d'
		(o'', u') = upCInit o' d u
		(os', us) = traverseCInitListMembers os d'
	in
		((x, o''):os', us `mappend` u')

traverseCDeclMembers :: (DownVisitor d, UpVisitor d u) => Traverse [(Maybe CDeclr, Maybe CInit, Maybe CExpr)] d u
traverseCDeclMembers [] _ = ([], mempty)
traverseCDeclMembers ((i1,i2,i3):os) d =
	let
		d1 = maybeDown downCDeclr i1 d
		(i1', u1) = maybeTrav recurseCDeclr i1 d1
		(i1'', u1') = maybeUp upCDeclr i1' d1 u1

		d2 = maybeDown downCInit i2 d1
		(i2', u2) = maybeTrav recurseCInit i2 d2
		(i2'', u2') = maybeUp upCInit i2' d2 u2

		d3 = maybeDown downCExpr i3 d2
		(i3', u3) = maybeTrav recurseCExpr i3 d3
		(i3'', u3') = maybeUp upCExpr i3' d3 u3

		(os', u) = traverseCDeclMembers os d3
	in
		((i1'',i2'',i3''):os', u `mappend` u1' `mappend` u2' `mappend` u3')

-- recurse {{{1
-- CTranslUnit {{{2
recurseCTranslUnit (CTranslUnit i x) d = let (i', u) = traverseCExtDecls i d in (CTranslUnit i' x, u)

-- CExtDecl {{{2
recurseCExtDecl (CDeclExt i) d = let (i', u) = traverseCDecl i d in (CDeclExt i', u)
recurseCExtDecl (CFDefExt i) d = let (i', u) = traverseCFunDef i d in (CFDefExt i', u)
recurseCExtDecl o@(CAsmExt _ _) _ = (o, mempty)

-- CDecl {{{2
recurseCDecl (CDecl x1 i x2) d = let (i', u) = traverseCDeclMembers i d in (CDecl x1 i' x2, u)

-- CDerivedDeclr {{{2
recurseCDerivedDeclr (CFunDeclr (Right (i, x1)) x2 x3) d = let (i', u) = traverseCDecls i d in (CFunDeclr (Right (i', x1)) x2 x3, u)
recurseCDerivedDeclr o@(CFunDeclr (Left _) _ _) _ = (o, mempty)
recurseCDerivedDeclr o@(CPtrDeclr _ _) _ = (o, mempty)
recurseCDerivedDeclr o@(CArrDeclr _ _ _) _ = (o, mempty)

-- CFunDef {{{2
recurseCFunDef (CFunDef x1 i1 i2 i3 x2) d =
	let
		(i1', u1) = traverseCDeclr i1 d
		(i2', u2) = traverseCDecls i2 d
		(i3', u3) = traverseCStat i3 d
	in
		(CFunDef x1 i1' i2' i3' x2, u1 `mappend` u2 `mappend` u3)

-- CDeclr {{{2
recurseCDeclr (CDeclr x1 i x2 x3 x4) d = let (i', u) = traverseCDerivedDeclrs i d in (CDeclr x1 i' x2 x3 x4, u)

-- CInit {{{2
recurseCInit (CInitExpr i x) d = let (i', u) = traverseCExpr i d in (CInitExpr i' x, u)
recurseCInit (CInitList i x) d = let (i', u) = traverseCInitListMembers i d in (CInitList i' x, u)

-- CExpr {{{2
recurseCExpr (CComma i x) d = let (i', u) = traverseCExprs i d in (CComma i' x, u)

recurseCExpr (CAssign x1 i1 i2 x2) d =
	let
		(i1', u1) = traverseCExpr i1 d
		(i2', u2) = traverseCExpr i2 d
	in
		(CAssign x1 i1' i2' x2, u1 `mappend` u2)

recurseCExpr (CCond i1 i2 i3 x) d =
	let
		(i1', u1) = traverseCExpr i1 d
		(i2', u2) = maybeTrav traverseCExpr i2 d
		(i3', u3) = traverseCExpr i3 d
	in
		(CCond i1' i2' i3' x, u1 `mappend` u2 `mappend` u3)

recurseCExpr (CBinary x1 i1 i2 x2) d =
	let
		(i1', u1) = traverseCExpr i1 d
		(i2', u2) = traverseCExpr i2 d
	in
		(CBinary x1 i1' i2' x2, u1 `mappend` u2)

recurseCExpr (CCast i1 i2 x) d =
	let
		(i1', u1) = traverseCDecl i1 d
		(i2', u2) = traverseCExpr i2 d
	in
		(CCast i1' i2' x, u1 `mappend` u2)

recurseCExpr (CUnary x1 i x2) d = let (i', u) = traverseCExpr i d in (CUnary x1 i' x2, u)

recurseCExpr (CSizeofExpr i x) d = let (i', u) = traverseCExpr i d in (CSizeofExpr i' x, u)

recurseCExpr (CSizeofType i x) d = let (i', u) = traverseCDecl i d in (CSizeofType i' x, u)

recurseCExpr (CAlignofExpr i x) d = let (i', u) = traverseCExpr i d in (CAlignofExpr i' x, u)

recurseCExpr (CAlignofType i x) d = let (i', u) = traverseCDecl i d in (CAlignofType i' x, u)

recurseCExpr (CComplexReal i x) d = let (i', u) = traverseCExpr i d in (CComplexReal i' x, u)

recurseCExpr (CComplexImag i x) d = let (i', u) = traverseCExpr i d in (CComplexImag i' x, u)

recurseCExpr (CIndex i1 i2 x) d =
	let
		(i1', u1) = traverseCExpr i1 d
		(i2', u2) = traverseCExpr i2 d
	in
		(CIndex i1' i2' x, u1 `mappend` u2)

recurseCExpr (CCall i1 i2 x) d =
	let
		(i1', u1) = traverseCExpr i1 d
		(i2', u2) = traverseCExprs i2 d
	in
		(CCall i1' i2' x, u1 `mappend` u2)

recurseCExpr (CMember i x1 x2 x3) d = let (i', u) = traverseCExpr i d in (CMember i' x1 x2 x3, u)

recurseCExpr o@(CVar _ _) _ = (o, mempty)

recurseCExpr o@(CConst _) _ = (o, mempty)

recurseCExpr (CCompoundLit i1 i2 x) d =
	let
		(i1', u1) = traverseCDecl i1 d
		(i2', u2) = traverseCInitListMembers i2 d
	in
		(CCompoundLit i1' i2' x, u1 `mappend` u2)

recurseCExpr (CStatExpr i x) d = let (i', u) = traverseCStat i d in (CStatExpr i' x, u)

recurseCExpr o@(CLabAddrExpr _ _) _ = (o, mempty)

recurseCExpr o@(CBuiltinExpr _) _ = (o, mempty)

-- CStat {{{2
recurseCStat (CLabel x1 i x2 x3) d = let (i', u) = traverseCStat i d in (CLabel x1 i' x2 x3, u)

recurseCStat (CCase i1 i2 x) d =
	let
		(i1', u1) = traverseCExpr i1 d
		(i2', u2) = traverseCStat i2 d
	in
		(CCase i1' i2' x, u1 `mappend` u2)

recurseCStat (CCases i1 i2 i3 x) d =
	let
		(i1', u1) = traverseCExpr i1 d
		(i2', u2) = traverseCExpr i2 d
		(i3', u3) = traverseCStat i3 d
	in
		(CCases i1' i2' i3' x, u1 `mappend` u2 `mappend` u3)

recurseCStat (CDefault i x) d = let (i', u) = traverseCStat i d in (CDefault i' x, u)
	
recurseCStat (CExpr i x) d = let (i', u) = maybeTrav traverseCExpr i d in (CExpr i' x, u)

--recurseCStat (CCompound x1 i x2) d = let (i', u) = traverseCBlockItems i d in (CCompound x1 i' x2, u)

-- traverseCStat cs@(CIf ce1 cs1 cs2 _) d = recurse downCStat mapCStat traverse create (ce1, cs1, cs2) cs d
--   where 
--     traverse all@(ce1, cs1, cs2) d = merge3 d all
--       (traverseCExpr ce1 d)
--       (traverseCStat cs1 d)
--       (maybeTrav traverseCStat cs2 d)
--     create (CIf _ _ _ x) (ce1, cs1, cs2) = CIf ce1 cs1 cs2 x

-- traverseCStat cs@(CSwitch ce1 cs1 _) d = recurse downCStat mapCStat traverse create (ce1, cs1) cs d
--   where 
--     traverse all@(ce1, cs1) d = merge2 d all
--       (traverseCExpr ce1 d)
--       (traverseCStat cs1 d)
--     create (CSwitch _ _ x) (ce1, cs1) = CSwitch ce1 cs1 x

-- traverseCStat cs@(CWhile ce1 cs1 _ _) d = recurse downCStat mapCStat traverse create (ce1, cs1) cs d
--   where 
--     traverse all@(ce1, cs1) d = merge2 d all
--       (traverseCExpr ce1 d)
--       (traverseCStat cs1 d)
--     create (CWhile _ _ x y) (ce1, cs1) = CWhile ce1 cs1 x y

-- traverseCStat cs@(CFor (Left ce1) ce2 ce3 cs1 _) d = recurse downCStat mapCStat traverse create (ce1, ce2, ce3, cs1) cs d
--   where 
--     traverse all@(ce1, ce2, ce3, cs1) d = merge4 d all
--       (maybeTrav traverseCExpr ce1 d)
--       (maybeTrav traverseCExpr ce2 d)
--       (maybeTrav traverseCExpr ce3 d)
--       (traverseCStat cs1 d)
--     create (CFor _ _ _ _ x) (ce1, ce2, ce3, cs1) = CFor (Left ce1) ce2 ce3 cs1 x

-- traverseCStat cs@(CFor (Right cd1) ce1 ce2 cs1 _) d = recurse downCStat mapCStat traverse create (cd1, ce1, ce2, cs1) cs d
--   where 
--     traverse all@(cd1, ce1, ce2, cs1) d = merge4 d all
--       (traverseCDecl cd1 d)
--       (maybeTrav traverseCExpr ce1 d)
--       (maybeTrav traverseCExpr ce2 d)
--       (traverseCStat cs1 d)
--     create (CFor _ _ _ _ x) (cd1, ce1, ce2, cs1) = CFor (Right cd1) ce1 ce2 cs1 x

-- traverseCStat cs@(CGoto _ _) d = recurse downCStat mapCStat noTrav noCreate noChildren cs d

-- traverseCStat cs@(CGotoPtr ce1 _) d = recurse downCStat mapCStat traverse create ce1 cs d
--   where 
--     traverse = traverseCExpr
--     create (CGotoPtr _ x) ce1 = CGotoPtr ce1 x

-- traverseCStat cs@(CCont _) d = recurse downCStat mapCStat noTrav noCreate noChildren cs d

-- traverseCStat cs@(CBreak _) d = recurse downCStat mapCStat noTrav noCreate noChildren cs d

-- traverseCStat cs@(CReturn ce1 _) d = recurse downCStat mapCStat traverse create ce1 cs d
--   where 
--     traverse = maybeTrav traverseCExpr
--     create (CReturn _ x) ce1 = CReturn ce1 x

-- traverseCStat cs@(CAsm _ _) d = recurse downCStat mapCStat noTrav noCreate noChildren cs d

-- CBlockItem {{{1
recurseCBlockItem (CBlockStmt i) d = let (i', u) = traverseCStat i d in (CBlockStmt i', u)

-- traverseCBlockItem cbi@(CBlockDecl cd) d = recurse downCBlockItem mapCBlockItem traverse create cd cbi d
--   where
--     traverse = traverseCDecl
--     create _ cd = CBlockDecl cd

-- traverseCBlockItem cbi@(CNestedFunDef cfd) d = recurse downCBlockItem mapCBlockItem traverse create cfd cbi d
--   where
--     traverse = traverseCFunDef
--     create _ cfd = CNestedFunDef cfd
