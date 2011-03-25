module Ocram.Sanity (
	checkSanity, outputErrors
) where

import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Language.C.Syntax.AST 
import Language.C.Data.Node (NodeInfo(NodeInfo, OnlyPos))
import Language.C.Data.Position (posFile, posRow)
import Data.Monoid (mconcat)

type Error = (Int, NodeInfo)

outputErrors :: [Error] -> IO ()
outputErrors [] = putStrLn "input program passed all sanity checks"
outputErrors es = do
	putStrLn "input program failed the following sanity checks"
	output $ zip [1..] es

printNodeInfo (OnlyPos p _) = printPosition p
printNodeInfo (NodeInfo p _ _) = printPosition p

printPosition p = "row: " ++ (show $ posRow p) ++ " in file: " ++ (posFile p)

output [] = return ()
output ((idx, (id, ni)):es) = do
	putStrLn $ show idx ++ ") error id: " ++ show id ++ ": " ++ printError id
	putStrLn $ printNodeInfo ni
	putStrLn ""
	output es

printError :: Int -> String
printError 1 = "function without parameter list"

checkSanity :: CTranslUnit -> [Error]
checkSanity ctu = snd $ traverseCTranslUnit ctu emptyDownState

type UpState = [Error]

instance UpVisitor EmptyDownState UpState where
	upCExtDecl (CFDefExt (CFunDef _ (CDeclr _ [] _ _ _) _ _ ni)) _ us = (1, ni) : mconcat us
 
