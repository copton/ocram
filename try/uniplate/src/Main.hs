module Main where

import Ast
import Data.Generics.Uniplate.Operations

test = TranslUnit [DeclExt (CDecl 1), FDefExt (FunDef 2)]

f :: TranslUnit -> Int
f y = sum [x | FunDef x <- universe y]

main = putStrLn $ show $ f test

