{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Visitor where

import Data.Monoid
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Typeable (cast, Typeable)

data Stmt = Stmt | NestedStmt [Stmt] | Call Int deriving (Show, Typeable)
data FunDef = FunDef Int Stmt deriving (Show, Typeable)
data TranslUnit = TranslUnit [FunDef] deriving (Show, Typeable)

class VisitorDown d where
	handleFunctionDefinitionDown :: FunDef -> d -> d
	handleFunctionDefinitionDown _ = id
	
	handleTranslUnitDown :: TranslUnit -> d -> d
	handleTranslUnitDown _ = id

	handleStmtDown :: Stmt -> d -> d
	handleStmtDown _ = id

class (VisitorDown d, Monoid u) => VisitorUp d u where
	handleFunctionDefinitionUp :: FunDef -> d -> [u] -> u
	handleFunctionDefinitionUp _ _ = mconcat

	mapFunctionDefinitionUp :: FunDef -> d -> [u] -> (Maybe FunDef, u)
	mapFunctionDefinitionUp x d us = (Nothing, handleFunctionDefinitionUp x d us)

	handleTranslUnitUp :: TranslUnit -> d -> [u] -> u
	handleTranslUnitUp _ _ = mconcat

	mapTranslUnitUp :: TranslUnit -> d -> [u] -> (Maybe TranslUnit, u)
	mapTranslUnitUp x d us = (Nothing, handleTranslUnitUp x d us)

	handleStmtUp :: Stmt -> d -> [u] -> u
	handleStmtUp _ _ = mconcat

	mapStmtUp :: Stmt -> d -> [u] -> (Maybe Stmt, u)
	mapStmtUp x d us = (Nothing, handleStmtUp x d us)

cast' :: (Typeable a, Typeable b) => a -> b
cast' x = fromJust.cast $ x

noCreate :: a -> b -> c
noCreate _ _ = error "foo"

noTrav :: a -> b -> [(Maybe c, d)]
noTrav _ _ = []

noChildren :: [()]
noChildren = []

recurse :: (Typeable t) =>
	   d -- down state 
	-> (o -> d -> d) -- down handler
	-> (o -> d -> [u] -> (Maybe o, u)) -- up handler
	-> o -- outer object
	-> (o -> [t] -> o) -- create
	-> ([t] -> d -> [(Maybe t, u)]) -- traverse
	-> [t] -- inner objects
	-> (Maybe o, u) -- (maybe new outer object, up state)
recurse downState downHandler upHandler outerObject create traverse innerObjects = 
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

travTranslUnit :: (VisitorDown d, VisitorUp d u) => TranslUnit -> d -> (Maybe TranslUnit, u)
travTranslUnit tu@(TranslUnit fds) d = recurse d handleTranslUnitDown mapTranslUnitUp tu create traverse fds
	where
		traverse ts d =	map (\t -> travFunDef (cast' t) d) ts
		create _ ts = TranslUnit $ map cast' ts

travFunDef :: (VisitorDown d, VisitorUp d u) => FunDef -> d -> (Maybe FunDef, u)
travFunDef fd@(FunDef _ s) d = recurse d handleFunctionDefinitionDown mapFunctionDefinitionUp fd create traverse [s]
	where
		traverse [t] d = [travStmt (cast' t) d]
		create (FunDef id _) [s] = FunDef id (cast' s)

travStmt :: (VisitorDown d, VisitorUp d u) => Stmt -> d -> (Maybe Stmt, u)
travStmt s@(Stmt) d = recurse d handleStmtDown mapStmtUp s noCreate noTrav noChildren

travStmt s@(Call _) d = recurse d handleStmtDown mapStmtUp s noCreate noTrav noChildren

travStmt s@(NestedStmt ss) d = recurse d handleStmtDown mapStmtUp s create traverse ss
	where
		traverse ts d = map (\t -> travStmt (cast' t) d) ts
		create _ ts = NestedStmt $ map cast' ts
