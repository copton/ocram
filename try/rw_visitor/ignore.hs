{-# LANGUAGE MultiParamTypeClasses #-}

import Visitor
import Data.Monoid

example = TranslUnit([
   FunDef 1 Stmt
 , FunDef 2 (NestedStmt [Call 1])
    ])

newtype DownState = DownState ()

instance VisitorDown DownState

newtype UpState = UpState { usCount :: Int }

instance Monoid UpState where
	mempty = UpState 0
	s `mappend` s' = UpState $ (usCount s) + (usCount s')

instance VisitorUp DownState UpState where
	handleStmtUp _ _ = error "should not be evaluated"		
	handleFunctionDefinitionUp _ _ _ = UpState 1

main = putStrLn $ show $ usCount $ snd $ travTranslUnit example $ DownState ()
	
