module Visitor where

import Data.Monoid

data Stmt = Stmt | NestedStmt [Stmt] | Call Int deriving Show
data FunDef = FunDef Int Stmt deriving Show
data TranslUnit = TranslUnit [FunDef] deriving Show

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

	handleTranslUnitUp :: TranslUnit -> d -> [u] -> u
	handleTranslUnitUp _ _ = mconcat

	handleStmtUp :: Stmt -> d -> [u] -> u
	handleStmtUp _ _ = mconcat

travTranslUnit :: (VisitorDown d, VisitorUp d u) => TranslUnit -> d -> u
travTranslUnit tu@(TranslUnit fds) d = u
	where
		d' = handleTranslUnitDown tu d
		us = map (\fd -> travFunDef fd d') fds
		u = handleTranslUnitUp tu d' us

travFunDef :: (VisitorDown d, VisitorUp d u) => FunDef -> d -> u
travFunDef fd@(FunDef _ s) d = u
	where
		d' = handleFunctionDefinitionDown fd d
		us = [travStmt s d']
		u = handleFunctionDefinitionUp fd d' us

travStmt :: (VisitorDown d, VisitorUp d u) => Stmt -> d -> u
travStmt s@(Stmt) d = u
	where
		d' = handleStmtDown s d
		us = []
		u = handleStmtUp s d' us

travStmt s@(Call _) d = u
	where
		d' = handleStmtDown s d
		us = []
		u = handleStmtUp s d' us

travStmt s@(NestedStmt ss) d = u
	where
		d' = handleStmtDown s d
		us = map (\s -> travStmt s d') ss
		u = handleStmtUp s d' us
