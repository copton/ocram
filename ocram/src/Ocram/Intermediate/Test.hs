{-# LANGUAGE QuasiQuotes #-}
module Ocram.Intermediate.Test (tests) where

-- imports {{{1
import Language.C.Data.Node (getLastTokenPos, posOfNode)
import Language.C.Data.Position (posRow)
import Language.C.Syntax.AST
import Ocram.Intermediate.Representation
import Ocram.Intermediate.CollectDeclarations (collect_declarations)
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, lpaste, paste)
import Ocram.Symbols (symbol)
import Test.Framework (Test, testGroup)
import Test.HUnit (assertEqual, Assertion)

tests :: Test -- {{{1
tests = testGroup "Representation" [test_collect_declarations]

test_collect_declarations :: Test -- {{{2
test_collect_declarations = enumTestGroup "collect_declarations" $ map runTest [
  -- nothing to do {{3
  ([lpaste|
    int foo(int i) {
      return i;
    }  
  |], [paste|
    int foo(int i) {
      return i;
    }
  |], [
      ("i", "i", 1, 3)
  ]) 
  ]
  where
    runTest :: (String, String, [(String, String, Int, Int)]) -> Assertion
    runTest (inputCode, expectedCode, expectedVars) =
      let
        (CTranslUnit [CFDefExt fd@(CFunDef x1 x2 x3 (CCompound x4 _ x5) x6)] x7) = enrich inputCode
        (outputVars, outputBody) = collect_declarations fd
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputBody x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in do
        assertEqual "output code" expectedCode' outputCode
        assertEqual "number of variables" (length expectedVars) (length outputVars)
        mapM_ (uncurry cmpVar) (zip expectedVars outputVars)

    cmpVar (tname, ename, start, end) var =
      let prefix = "Variable " ++ tname ++ ": " in
      do
        assertEqual (prefix ++ "T-code name") tname ((symbol . var_decl) var)
        assertEqual (prefix ++ "E-cdoe name") ename (var_fqn var)
        assertEqual (prefix ++ "start of scope")  start ((posRow . posOfNode . var_scope) var)
        assertEqual (prefix ++ "end of scope") end ((posRow . fst . getLastTokenPos . var_scope) var)
