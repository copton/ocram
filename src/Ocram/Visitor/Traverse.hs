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
import Data.Typeable (Typeable, cast)
import Data.Maybe (fromJust, isJust, fromMaybe)

cast' :: (Typeable a, Typeable b) => a -> b
cast' x = fromJust.cast $ x

recurse :: (Typeable t) =>
     (o -> d -> d) -- down handler
  -> (o -> d -> [u] -> (Maybe o, u)) -- up handler
  -> o -- outer object
  -> (o -> [t] -> o) -- create
  -> ([t] -> d -> [(Maybe t, u)]) -- traverse
  -> [t] -- inner objects
	-> d -- down state
  -> (Maybe o, u) -- (maybe new outer object, up state)
recurse downHandler upHandler outerObject create traverse innerObjects downState =
  let downState' = downHandler outerObject downState in
  let (maybeInnerObjects, upStates) = unzip $ traverse innerObjects downState' in
  if any isJust maybeInnerObjects
  then
    let innerObjects' = map (uncurry fromMaybe) $ zip innerObjects maybeInnerObjects in
    let outerObject' = create outerObject innerObjects' in
    case upHandler outerObject' downState' upStates of
      (Nothing, upState) -> (Just outerObject', upState)
      res -> res
  else
    case upHandler outerObject downState' upStates of
      (Nothing, upState) -> (Nothing, upState)
      res -> res

maybeTrav :: (Typeable t, Typeable o) => (o -> d -> (Maybe o, u)) -> Maybe t -> d -> [(Maybe o, u)]
maybeTrav _ Nothing _ = []
maybeTrav f (Just t) d = [f (cast' t) d]

mapTrav :: (Typeable t, Typeable o) => (o -> d -> (Maybe o, u)) -> [t] -> d -> [(Maybe o, u)]
mapTrav f ts d = map (\t -> f (cast' t) d) ts

trav :: (Typeable t, Typeable o) => (o -> d -> (Maybe o, u)) -> t -> d -> [(Maybe o, u)]
trav f t d = [f (cast' t) d]

noCreate :: a -> b -> c
noCreate _ _ = undefined

noTrav :: a -> b -> [(Maybe c, d)]
noTrav _ _ = []

noChildren :: [()]
noChildren = []

traverseCTranslUnit :: (DownVisitor d, UpVisitor d u) => CTranslUnit -> d -> (Maybe CTranslUnit, u)
traverseCTranslUnit ctu@(CTranslUnit decls _) = recurse downCTranslUnit mapCTranslUnit ctu create traverse decls
	where 
		traverse = mapTrav traverseCExtDecl
		create (CTranslUnit _ ni) ts = CTranslUnit (map cast' ts) ni

traverseCDeclr _ _ = undefined
traverseCDerivedDeclr _ _ = undefined
traverseCExpr _ _ = undefined
traverseCFunDef _ _ = undefined
traverseCInit _ _ = undefined
traverseCStat _ _ = undefined
traverseIdent _ _ = undefined

traverseCExtDecl :: (DownVisitor d, UpVisitor d u) => CExtDecl -> d -> (Maybe CExtDecl, u)
traverseCExtDecl ced@(CDeclExt cd) = recurse downCExtDecl mapCExtDecl ced create traverse [cd]
	where 
		traverse [t] = trav traverseCDecl t
		create _ [t] = CDeclExt (cast' t)

traverseCExtDecl ced@(CFDefExt cfd) = recurse downCExtDecl mapCExtDecl ced create traverse [cfd]
	where 
		traverse [t] = trav traverseCFunDef t
		create _ [t] = CFDefExt (cast' t)

traverseCExtDecl ced@(CAsmExt _ _) = recurse downCExtDecl mapCExtDecl ced noCreate noTrav noChildren

traverseCDecl :: (DownVisitor d, UpVisitor d u) => CDecl -> d -> (Maybe CDecl, u)
traverseCDecl cd@(CDecl _ decls _) = recurse downCDecl mapCDecl cd create traverse decls
	where 
		traverse :: Typeable t => [t] -> d -> [(Maybe o, u)]
		traverse tr d = concatMap ((traverse' d).cast') tr
		traverse' :: (Typeable t1, Typeable t2, Typeable t3) => (t1, t2, t3) -> [(Maybe o, u)]
		traverse' d (t1, t2, t3) = 
				 maybeTrav traverseCDeclr t1 d 
			++ maybeTrav traverseCInit t2 d 
			++ maybeTrav traverseCExpr t3 d
		create (CDecl dss _ ni) ts = CDecl dss (map (create'.cast') ts) ni
		create' (t1, t2, t3) = (cast' t1, cast' t2, cast' t3)
			

-- traverseCDerivedDeclr :: (DownVisitor d, UpVisitor d u) => CDerivedDeclr -> d -> u
-- traverseCDerivedDeclr cdd@(CFunDeclr (Left id) _ _) = recurse downCDerivedDeclr upCDerivedDeclr traverse cdd id
--   where traverse = mapTrav traverseIdent

-- traverseCDerivedDeclr cdd@(CFunDeclr (Right (cds, _)) _ _) = recurse downCDerivedDeclr upCDerivedDeclr traverse cdd cds
--   where traverse = mapTrav traverseCDecl

-- traverseCDerivedDeclr cdd = recurse downCDerivedDeclr upCDerivedDeclr noTrav cdd Nothing


-- traverseCFunDef :: (DownVisitor d, UpVisitor d u) => CFunDef -> d -> u
-- traverseCFunDef cfd@(CFunDef _ cdr cds cst _) = recurse downCFunDef upCFunDef traverse cfd (cdr, cds, cst)
--   where traverse (cdr, cds, cst) d = 
--              trav traverseCDeclr cdr d 
--           ++ mapTrav traverseCDecl cds d 
--           ++ trav traverseCStat cst d

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
