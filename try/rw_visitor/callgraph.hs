{-# LANGUAGE MultiParamTypeClasses #-}

import Visitor
import Data.Monoid

example = TranslUnit([
   FunDef 1 Stmt
 , FunDef 2 (NestedStmt [Call 1])
    ])

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
	handleStmtUp _ _ us = mconcat us

main :: IO ()
main = putStrLn $ show $ (snd $ go $ DownState Nothing :: UpState)
	where
		go = travTranslUnit example
