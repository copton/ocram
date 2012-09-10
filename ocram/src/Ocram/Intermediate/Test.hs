{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Ocram.Intermediate.Test (tests) where

-- imports {{{1
import Language.C.Data.Node (getLastTokenPos, posOfNode)
import Language.C.Data.Position (posRow)
import Language.C.Syntax.AST
import Ocram.Intermediate.Representation
import Ocram.Intermediate.CollectDeclarations
import Ocram.Intermediate.DesugarControlStructures
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, lpaste, paste)
import Ocram.Symbols (symbol)
import Test.Framework (Test, testGroup)
import Test.HUnit (assertEqual, Assertion, (@=?))

tests :: Test -- {{{1
tests = testGroup "Representation" [test_collect_declarations, test_desugar_control_structures]

test_collect_declarations :: Test -- {{{2
test_collect_declarations = enumTestGroup "collect_declarations" $ map runTest [
  -- , 01 - nothing to do {{{3
  ([lpaste|
01: int foo(int i) {
      return i;
03: }  
  |], [paste|
    int foo(int i) {
      return i;
    }
  |], [
      ("i", "i", 1, 3)
  ]) 
  , -- 02 - local variable with initializer {{{3
  ([lpaste|
01: int foo(int i) {
      int j = 23;
      return i + j;
04: }
  |], [paste|
    int foo(int i) {
      j = 23;
      return i + j;
    }
  |], [
      ("i", "i", 1, 4)
    , ("j", "j", 1, 4)
  ])
  , -- 03 - local variable shadowing {{{3
  ([lpaste|
01: void foo() {
      int i = 23;
03:   {
        int i = 42;
05:   }
06: }
  |], [paste|
    void foo() {
      i = 23;
      {
        ec_shadow_i_0 = 42;
      }
    }
  |], [
      ("i", "ec_shadow_i_0", 3, 5)
    , ("i", "i", 1, 6)
  ])
  , -- 04 - local variable shadowing - with access {{{3
  ([lpaste|
01: void foo() {
      int i = 23;
03:   {
        int i = 42;
        i = 19;
06:   }
07: }
  |], [paste|
    void foo() {
      i = 23;
      {
        ec_shadow_i_0 = 42;
        ec_shadow_i_0 = 19;
      }
    }
  |], [
      ("i", "ec_shadow_i_0", 3, 6)
    , ("i", "i", 1, 7)
  ])
  , -- 05 - for loop with declaration {{{3
  ([lpaste|
01: void foo(int i) {
02:   for (int i = 0; i < 23; i++) {
03:   }
04: } 
  |], [paste|
    void foo(int i) {
      {
        ec_shadow_i_0 = 0;
        for (; ec_shadow_i_0 < 23; ec_shadow_i_0++) { }
      }
    }
  |], [
      ("i", "i", 1, 4)
    , ("i", "ec_shadow_i_0", 2, 3)
  ])
  , -- 06 - multiple declarations {{{3
  ([lpaste|
01: int foo() {
      int i=0, j=1;
03:   {
        int i=23, j=42;
        return i + j;
06:   }
07: }
  |], [paste|
    int foo() {
      i = 0;
      j = 1;
      {
        ec_shadow_i_0 = 23;
        ec_shadow_j_0 = 42;
        return ec_shadow_i_0 + ec_shadow_j_0;
      }
    }
  |], [
      ("i", "ec_shadow_i_0", 3, 6)
    , ("j", "ec_shadow_j_0", 3, 6)
    , ("i", "i", 1, 7)
    , ("j", "j", 1, 7)
  ])
  -- end {{{3
  ]
  where
    runTest :: (String, String, [(String, String, Int, Int)]) -> Assertion -- {{{3
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
      let prefix = "Variable " ++ ename ++ ": " in
      do
        assertEqual (prefix ++ "T-code name") tname ((symbol . var_decl) var)
        assertEqual (prefix ++ "E-cdoe name") ename (var_fqn var)
        assertEqual (prefix ++ "start of scope")  start ((posRow . posOfNode . var_scope) var)
        assertEqual (prefix ++ "end of scope") end ((posRow . fst . getLastTokenPos . var_scope) var)

test_desugar_control_structures:: Test -- {{{2
test_desugar_control_structures = enumTestGroup "desugar_control_structures" $ map runTest [
  -- , 01 - while loop {{{3
  ([paste|
      void foo() {
        a();
        while(1)
          g();
        
        b();
      }
  |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_0: ;
          if (! 1) goto ec_ctrlbl_1;
          g();
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
  |])
  , -- 02 - do loop {{{3
  ([paste|
      void foo() {
        a();
        do {
          g();
        } while(1);
        b();
      }
  |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_1_0: ;
          g();
          if (1) goto ec_ctrlbl_1_0;
          ec_ctrlbl_1_1: ;
        }
        b();
      }
  |])
  , -- 03 - for loop {{{3
  ([paste|
      void foo() {
        a();
        {
          i=0;
          for (; i<23; i++) {
            g(i);
          }
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          i = 0;
          {
            ec_ctrlbl_1_0: ;
            if (! (i<23)) goto ec_ctrlbl_1_1;
            g(i);
            i++;
            goto ec_ctrlbl_1_0;
            ec_ctrlbl_1_1: ;
          }
        }
        b();
      }
    |])
  , -- 04 - for loop - with declaration {{{3
  ([paste|
      void foo() {
        a();
        {
          i = 0;
          for (; i<23; i++) {
            g(i);
          }
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          i = 0;
          {
            ec_ctrlbl_1_0: ;
            if (! (i<23)) goto ec_ctrlbl_1_1;
            g(i);
            i++;
            goto ec_ctrlbl_1_0;
            ec_ctrlbl_1_1: ;
          }
        }
        b();
      }
    |])
  , -- 05 - for loop - no init expression {{{3
  ([paste|
      void foo() {
        a();
        for (; i<23; i++) {
          g(i);
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_1_0: ;
          if (! (i<23)) goto ec_ctrlbl_1_1;
          g(i);
          i++;
          goto ec_ctrlbl_1_0;
          ec_ctrlbl_1_1: ;
        }
        b();
      }
    |])
  , -- 06 - for loop - no break condition {{{3
  ([paste|
      void foo() {
        a();
        i = 0;
        {
          for (; ; i++) {
            g(i);
          }
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          i = 0;
          {
            ec_ctrlbl_1_0: ;
            g(i);
            i++;
            goto ec_ctrlbl_1_0;
            ec_ctrlbl_1_1: ;
          }
        }
        b();
      }
    |])
  , -- 07 - for loop - no incr expression{{{3
  ([paste|
      void foo() {
        a();
        for (i = 0; i<23; ) {
          g(i);
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          i = 0;
          ec_ctrlbl_1_0: ;
          if (! (i<23)) goto ec_ctrlbl_1_1;
          g(i);
          goto ec_ctrlbl_1_0;
          ec_ctrlbl_1_1: ;
        }
        b();
      }
    |])
  , -- 08 - continue and break {{{3
  ([paste|
      void foo() {
        a();
        do {
          continue;
          g();
          break;
        } while(1);
        b();
      }
  |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_1_0: ;
          goto ec_ctrlbl_1_0;
          g();
          goto ec_ctrlbl_1_1;
          if (1) goto ec_ctrlbl_1_0;
          ec_ctrlbl_1_1: ;
        }
        b();
      }
  |])
  , -- 09 - nested {{{3
  ([paste|
      void foo() {
        a();
        while(1) {
          b();
          continue;
          c();
          do {
            d();
            continue;
            e();
            break;
            f();
          } while(23);
          g();
          break;
          h();
        }
        i();
      }
    |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_1_2: ;
          if (!1) goto ec_ctrlbl_1_3;
          b();
          goto ec_ctrlbl_1_2;
          c();
          {
            ec_ctrlbl_1_0: ;
            d();
            goto ec_ctrlbl_1_0;
            e();
            goto ec_ctrlbl_1_1;
            f();
            if (23) goto ec_ctrlbl_1_0;
            ec_ctrlbl_1_1: ;
          }
          g();
          goto ec_ctrlbl_1_3;
          h();
          goto ec_ctrlbl_1_2;
          ec_ctrlbl_1_3: ; 
        }
        i(); 
      }
    |])
  -- end {{{3
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{3
    runTest (inputCode, expectedCode) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7) = enrich inputCode
        outputItems = desugar_control_structures inputItems
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in
        expectedCode' @=? outputCode
        

   
