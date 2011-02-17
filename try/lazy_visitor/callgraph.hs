{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Monoid

data Stmt = Stmt | NestedStmt [Stmt] | Call Int deriving Show
data FunDef = FunDef Int Stmt deriving Show
data TranslUnit = TranslUnit [FunDef] deriving Show


example = TranslUnit([
   FunDef 1 Stmt
 , FunDef 2 (NestedStmt [Call 1])
    ])

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

data DownState = DownState {
	downCaller :: Maybe Int
} deriving Show

data UpState = UpState {
	upCalls :: [(Int, Int)]
} deriving Show

instance Monoid UpState where
	mempty = UpState []
	(UpState s) `mappend` (UpState s') = UpState $ s ++ s'	

instance VisitorDown DownState where 
	handleFunctionDefinitionDown (FunDef id _) _ = DownState $ Just id

instance VisitorUp DownState UpState where
	handleStmtUp (Call callee) (DownState (Just caller)) _ = UpState [(caller, callee)]
	handleStmtUp _ _ us = UpState $ concatMap upCalls us

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

main :: IO ()
main = putStrLn $ show $ (go $ DownState Nothing :: UpState)
	where
		go = travTranslUnit example
