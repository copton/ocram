module Ocram.Sanity (
	checkSanity, outputErrors
) where

import Language.C.Syntax.AST (CTranslUnit, CTranslationUnit(CTranslUnit))
import Language.C.Data.Node (NodeInfo(NodeInfo, OnlyPos))
import Language.C.Data.Position (posFile, posRow)

type Error = (Int, NodeInfo, String)

checkSanity :: CTranslUnit -> [Error]
checkSanity (CTranslUnit _ ni) = [(23, ni, "test")]

outputErrors :: [Error] -> IO ()
outputErrors [] = putStrLn "input program passed all sanity checks"
outputErrors es = do
	putStrLn "input program failed the following sanity checks"
	output $ zip [1..] es

printNodeInfo (OnlyPos p _) = printPosition p
printNodeInfo (NodeInfo p _ _) = printPosition p

printPosition p = "row: " ++ (show $ posRow p) ++ " in file: " ++ (posFile p)

output [] = return ()
output ((idx, (id, ni, s)):es) = do
	putStrLn $ show idx ++ ") error id: " ++ show id ++ ": " ++ s
	putStrLn $ printNodeInfo ni
	putStrLn ""
	output es
