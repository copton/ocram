{-# LANGUAGE MultiParamTypeClasses #-}

import Visitor
import Data.Monoid

example = TranslUnit([
   FunDef 1 Stmt
 , FunDef 2 (NestedStmt [Call 1])
    ])

data DownState = DownState ()

data UpState = UpState ()

instance Monoid UpState where
	mempty = UpState ()
	_ `mappend` _ = mempty

instance VisitorDown DownState

instance VisitorUp DownState UpState where
	mapStmtUp (NestedStmt [stmt]) _ _ = (Just stmt, UpState ())
	mapStmtUp _ _ _ = (Nothing, UpState ())

main :: IO ()
main = putStrLn $ show $ fst $ (travTranslUnit example $ DownState () :: (Maybe TranslUnit, UpState))
