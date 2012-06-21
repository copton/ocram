{-# LANGUAGE TemplateHaskell #-}
module Ocram.Symbols.Test 
-- exports {{{1
(
  tests
) where

-- imports {{{1
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CExternalDeclaration(CFDefExt, CDeclExt))
import Ocram.Symbols (symbol)
import Ocram.Test.Lib (parse, enumTestGroup)
import Ocram.Util (abort)
import Test.HUnit ((@=?))
import Test.Framework (Test)

tests :: Test -- {{{1
tests = enumTestGroup "Symbol" $ map runTest [
    ("void foo();", "foo")
  , ("void foo() { }", "foo")
  , ("struct foo { };", "foo")
  , ("union foo { };", "foo")
  , ("enum foo { A };", "foo")
  , ("enum { A, B };", "<<no_name>>")
  ]
  where
    reduce (CTranslUnit [CFDefExt cfd] _) = symbol cfd
    reduce (CTranslUnit [CDeclExt cde] _) = symbol cde
    reduce _ = $abort "Symols.Test"

    runTest (code, expected) = expected @=? reduce (parse code)
