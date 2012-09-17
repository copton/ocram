{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Ocram.Intermediate.Test (tests) where

-- imports {{{1
import Compiler.Hoopl (showGraph)
import Language.C.Data.Node (getLastTokenPos, posOfNode)
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Position (posRow)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST
import Ocram.Intermediate
import Ocram.Intermediate.BooleanShortCircuiting
import Ocram.Intermediate.BuildBasicBlocks
import Ocram.Intermediate.CollectDeclarations
import Ocram.Intermediate.CriticalVariables
import Ocram.Intermediate.DesugarControlStructures
import Ocram.Intermediate.NormalizeCriticalCalls
import Ocram.Intermediate.SequencializeBody
import Ocram.Symbols (symbol)
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, lpaste, paste)
import Ocram.Util (fromJust_s)
import Test.Framework (Test, testGroup)
import Test.HUnit (assertEqual, Assertion, (@=?))

import qualified Data.Map as M
import qualified Data.Set as S

tests :: Test -- {{{1
tests = testGroup "Intermediate" [
          test_collect_declarations
        , test_desugar_control_structures
        , test_boolean_short_circuiting
        , test_sequencialize_body
        , test_normalize_critical_calls
        , test_build_basic_blocks
        , test_critical_variables
        , test_ast_2_ir
        ]

test_collect_declarations :: Test -- {{{1
test_collect_declarations = enumTestGroup "collect_declarations" $ map runTest [
  -- , 01 - nothing to do {{{2
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
  , -- 02 - local variable with initializer {{{2
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
  , -- 03 - local variable shadowing {{{2
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
  , -- 04 - local variable shadowing - with access {{{2
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
  , -- 05 - for loop with declaration {{{2
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
  , -- 06 - multiple declarations {{{2
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
  -- end {{{2
  ]
  where
    runTest :: (String, String, [(String, String, Int, Int)]) -> Assertion -- {{{2
    runTest (inputCode, expectedCode, expectedVars) =
      let
        (CTranslUnit [CFDefExt fd@(CFunDef x1 x2 x3 (CCompound x4 _ x5) x6)] x7) = enrich inputCode
        (outputBody, outputVars) = collect_declarations fd
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
        assertEqual (prefix ++ "start of scope")  start ((posRow . posOfNode . $fromJust_s . var_scope) var)
        assertEqual (prefix ++ "end of scope") end ((posRow . fst . getLastTokenPos . $fromJust_s . var_scope) var)

test_desugar_control_structures:: Test -- {{{1
test_desugar_control_structures = enumTestGroup "desugar_control_structures" $ map runTest [
  -- , 01 - while loop {{{2
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
  , -- 02 - do loop {{{2
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
          ec_ctrlbl_0: ;
          g();
          if (1) goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
  |])
  , -- 03 - for loop {{{2
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
            ec_ctrlbl_0: ;
            if (! (i<23)) goto ec_ctrlbl_1;
            g(i);
            i++;
            goto ec_ctrlbl_0;
            ec_ctrlbl_1: ;
          }
        }
        b();
      }
    |])
  , -- 04 - for loop - with declaration {{{2
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
            ec_ctrlbl_0: ;
            if (! (i<23)) goto ec_ctrlbl_1;
            g(i);
            i++;
            goto ec_ctrlbl_0;
            ec_ctrlbl_1: ;
          }
        }
        b();
      }
    |])
  , -- 05 - for loop - no init expression {{{2
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
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
    |])
  , -- 06 - for loop - no break condition {{{2
  ([paste|
      void foo() {
        a();
        {
          i = 0;
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
            ec_ctrlbl_0: ;
            g(i);
            i++;
            goto ec_ctrlbl_0;
            ec_ctrlbl_1: ;
          }
        }
        b();
      }
    |])
  , -- 07 - for loop - no incr expression{{{2
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
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
    |])
  , -- 08 - continue and break {{{2
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
          ec_ctrlbl_0: ;
          goto ec_ctrlbl_0;
          g();
          goto ec_ctrlbl_1;
          if (1) goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
  |])
  , -- 09 - nested {{{2
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
          ec_ctrlbl_0: ;
          if (!1) goto ec_ctrlbl_1;
          b();
          goto ec_ctrlbl_0;
          c();
          {
            ec_ctrlbl_2: ;
            d();
            goto ec_ctrlbl_2;
            e();
            goto ec_ctrlbl_3;
            f();
            if (23) goto ec_ctrlbl_2;
            ec_ctrlbl_3: ;
          }
          g();
          goto ec_ctrlbl_1;
          h();
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ; 
        }
        i(); 
      }
    |])
  , -- 10 - if statements {{{2
  ([paste|
    void foo() {
      if (1) {
        b();
      }
    } 
  |], [paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
        }
        ec_ctrlbl_1: ;
      }
    }
  |])
  , -- 11 - if statements with else block {{{2
  ([paste|
    void foo() {
      if (1) {
        b();
        return;
      } else {
        c();
        return;
      }
    } 
  |], [paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
          return;
          goto ec_ctrlbl_2;
        }
        {
          ec_ctrlbl_1: ;
          c();
          return;
        }
        ec_ctrlbl_2: ;
      }
    }
  |])
  , -- 12 - switch statement {{{2
  ([paste|
    void foo(int i) {
      switch (i) {
        case 1: a(); b(); break;
        case 2: c(); d();
        case 3: e(); f(); return;
      }
    }
  |], [paste|
    void foo(int i) {
      {
        if (i==1) goto ec_ctrlbl_1;
        if (i==2) goto ec_ctrlbl_2;
        if (i==3) goto ec_ctrlbl_3;
        goto ec_ctrlbl_0;
        
        {
          ec_ctrlbl_1: ;
          a(); b();
          goto ec_ctrlbl_0;
        }
        {
          ec_ctrlbl_2: ;
          c(); d();
        }
        {
          ec_ctrlbl_3: ;
          e(); f(); return;
        }
        ec_ctrlbl_0: ;
      }
    }
  |])
  , -- 13 - switch statement with default {{{2
  ([paste|
    void foo(int i) {
      switch (i) {
        case 1: a(); b(); break;
        case 2: c(); d();
        default: e(); f();
      }
    }
  |], [paste|
    void foo(int i) {
      {
        if (i==1) goto ec_ctrlbl_1;
        if (i==2) goto ec_ctrlbl_2;
        goto ec_ctrlbl_3;
        
        {
          ec_ctrlbl_1: ;
          a(); b();
          goto ec_ctrlbl_0;
        }
        {
          ec_ctrlbl_2: ;
          c(); d();
        }
        {
          ec_ctrlbl_3: ;
          e(); f();
        }
        ec_ctrlbl_0: ;
      }
    }
  |])
    
  -- end {{{2
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{2
    runTest (inputCode, expectedCode) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7) = enrich inputCode
        outputItems = desugar_control_structures inputItems
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in
        expectedCode' @=? outputCode
        
test_boolean_short_circuiting :: Test -- {{{1
test_boolean_short_circuiting = enumTestGroup "boolean_short_circuiting" $ map runTest [
  -- , 01 - no critical function {{{2
  ([],
  [paste|
    void foo() {
      if(g() || h()) ;
    }
  |], [paste|
    void foo() {
      if(g() || h()) ;
    }
  |], [])
  , -- 02 - critical function on left hand side, or expression {{{2
  (["g"],
  [paste|
    void foo() {
      if(g() || h()) ;
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        if (ec_bool_0) ;
      }
    }
  |], ["int ec_bool_0"])
  , -- 03 - critical function on right hand side, and expression {{{2
  (["h"],
  [paste|
    void foo() {
      if(g() && h()) ;
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (ec_bool_0) {
          ec_bool_0 = !!h();
        }
        if (ec_bool_0) ;
      }
    }
  |], ["int ec_bool_0"])
  , -- 04 - expression statement {{{2
  (["g"],
  [paste|
    void foo() {
      g() || h();
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        ec_bool_0;
      }
    }
  |], ["int ec_bool_0"])
  , -- 05 - switch statement {{{2
  (["g"],
  [paste|
    void foo() {
      switch(g() || h()) ;
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        switch(ec_bool_0) ;
      }
    }
  |], ["int ec_bool_0"])
  , -- 06 - return statement {{{2
  (["g"],
  [paste|
    int foo() {
      return (g() || h()) ;
    }
  |], [paste|
    int foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        return ec_bool_0;
      }
    }
  |], ["int ec_bool_0"])
  , -- 07 - function call {{{2
  (["g"],
  [paste|
    void foo() {
      h(k() || g());
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!k();
        if (! ec_bool_0) {
          ec_bool_0 = !!g();
        }
        h(ec_bool_0);
      }
    }
  |], ["int ec_bool_0"])
  , -- 08 - within algebraic expression {{{2
  (["g"],
  [paste|
    void foo() {
      if ((g() || 1) + 3);
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!1;
        }
        if (ec_bool_0 + 3);
      }
    }
  |], ["int ec_bool_0"])
  , -- 09 - containing algebraic expression {{{2
  (["g"],
  [paste|
    void foo() {
      if ((1+g()) || h());
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!(1 + g());
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        if (ec_bool_0);
      }
    }
  |], ["int ec_bool_0"])
  , -- 10 - nested {{{2
  (["g1", "g2"],
  [paste|
    void foo() {
      if ((g1() || h1()) && (h2() || g2()));
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g1();
        if (! ec_bool_0) {
          ec_bool_0 = !!h1();
        }
        ec_bool_2 = !!ec_bool_0;
        if (ec_bool_2) {
          ec_bool_1 = !!h2();
          if (!ec_bool_1) {
            ec_bool_1 = !!g2();
          }
          ec_bool_2 = !!ec_bool_1;
        }
        if (ec_bool_2);
      }
    }
  |], [
      "int ec_bool_2"
    , "int ec_bool_1"
    , "int ec_bool_0"
  ])
  , -- 11 - generic case {{{2
  (["g", "h"],
  [paste|
      void foo() {
        if ((g() || (i = x(), 1)) && h());
      }
  |], [paste|
      void foo() {
        {
          ec_bool_0 = !!g();
          if (! ec_bool_0) {
            ec_bool_0 = !! (i = x(), 1);
          }
          ec_bool_1 = !!ec_bool_0;
          if (ec_bool_1) {
            ec_bool_1 = !!h();
          }
          if (ec_bool_1);
        }
      }
  |], [
      "int ec_bool_1"
    , "int ec_bool_0"
  ])
  ]
  where
    runTest :: ([String], String, String, [String]) -> Assertion -- {{{2
    runTest (cf, inputCode, expectedCode, expectedDecls) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7)
           = enrich inputCode

        outputAst                           
            = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] (x7 :: NodeInfo)

        (outputItems, outputVariables) = boolean_short_circuiting (S.fromList cf) inputItems
        expectedCode'                       = reduce $ (enrich expectedCode :: CTranslUnit) :: String
        outputCode                          = reduce $ outputAst
        outputDecls                         = map (show . pretty . var_decl) outputVariables
      in do
        expectedCode' @=? outputCode
        expectedDecls @=? outputDecls

test_sequencialize_body :: Test -- {{{1
test_sequencialize_body = enumTestGroup "sequencialize_body" $ map runTest [
  -- , 01 - while loop {{{2
  ([paste|
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
  |], [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      if (! 1) goto ec_ctrlbl_1;
      g();
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 02 - do loop {{{2
  ([paste|
    void foo() {
      a();
      {
        ec_ctrlbl_0: ;
        g();
        if (1) goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      g();
      if (1) goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 03 - for loop {{{2
  ([paste|
    void foo() {
      a();
      {
        i = 0;
        {
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      i = 0;
      ec_ctrlbl_0: ;
      if (! (i<23)) goto ec_ctrlbl_1;
      g(i);
      i++;
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 04 - for loop - with declaration {{{2
  ([paste|
    void foo() {
      a();
      {
        i = 0;
        {
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      i = 0;
      ec_ctrlbl_0: ;
      if (! (i<23)) goto ec_ctrlbl_1;
      g(i);
      i++;
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 05 - for loop - no init expression {{{2
  ([paste|
      void foo() {
        a();
        {
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
  |], [paste|
      void foo() {
        a();
        ec_ctrlbl_0: ;
        if (! (i<23)) goto ec_ctrlbl_1;
        g(i);
        i++;
        goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
        b();
      }
  |])
  , -- 06 - for loop - no break condition {{{2
  ([paste|
    void foo() {
      a();
      {
        i = 0;
        {
          ec_ctrlbl_0: ;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      i = 0;
      ec_ctrlbl_0: ;
      g(i);
      i++;
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 07 - for loop - no incr expression{{{2
  ([paste|
    void foo() {
      a();
      {
        i = 0;
        ec_ctrlbl_0: ;
        if (! (i<23)) goto ec_ctrlbl_1;
        g(i);
        goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      i = 0;
      ec_ctrlbl_0: ;
      if (! (i<23)) goto ec_ctrlbl_1;
      g(i);
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 08 - continue and break {{{2
  ([paste|
    void foo() {
      a();
      {
        ec_ctrlbl_0: ;
        goto ec_ctrlbl_0;
        g();
        goto ec_ctrlbl_1;
        if (1) goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      goto ec_ctrlbl_0;
      g();
      goto ec_ctrlbl_1;
      if (1) goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 09 - nested {{{2
  ([paste|
    void foo() {
      a();
      {
        ec_ctrlbl_0: ;
        if (!1) goto ec_ctrlbl_1;
        b();
        goto ec_ctrlbl_0;
        c();
        {
          ec_ctrlbl_2: ;
          d();
          goto ec_ctrlbl_2;
          e();
          goto ec_ctrlbl_3;
          f();
          if (23) goto ec_ctrlbl_2;
          ec_ctrlbl_3: ;
        }
        g();
        goto ec_ctrlbl_1;
        h();
        goto ec_ctrlbl_0;
        ec_ctrlbl_1: ; 
      }
      i(); 
    }
  |], [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      if (!1) goto ec_ctrlbl_1;
      b();
      goto ec_ctrlbl_0;
      c();
      ec_ctrlbl_2: ;
      d();
      goto ec_ctrlbl_2;
      e();
      goto ec_ctrlbl_3;
      f();
      if (23) goto ec_ctrlbl_2;
      ec_ctrlbl_3: ;
      g();
      goto ec_ctrlbl_1;
      h();
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ; 
      i(); 
    }
  |])
  , -- 10 - if statements {{{2
  ([paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
        }
        ec_ctrlbl_1: ;
      }
    }
  |], [paste|
    void foo() {
      if (1) goto ec_ctrlbl_0;
      else goto ec_ctrlbl_1;
      ec_ctrlbl_0: ;
      b();
      ec_ctrlbl_1: ;
    }
  |])
  , -- 11 - if statements with else block {{{2
  ([paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
          return;
          goto ec_ctrlbl_2;
        }
        {
          ec_ctrlbl_1: ;
          c();
          return;
        }
        ec_ctrlbl_2: ;
      }
    }
  |], [paste|
    void foo() {
      if (1) goto ec_ctrlbl_0;
      else goto ec_ctrlbl_1;
      ec_ctrlbl_0: ;
      b();
      return;
      goto ec_ctrlbl_2;
      ec_ctrlbl_1: ;
      c();
      return;
      ec_ctrlbl_2: ;
    }
  |])
  , -- 12 - switch statement {{{2
  ([paste|
    void foo(int i) {
      {
        if (i==1) goto ec_ctrlbl_1;
        if (i==2) goto ec_ctrlbl_2;
        if (i==3) goto ec_ctrlbl_3;
        
        {
          ec_ctrlbl_1: ;
          a(); b();
          goto ec_ctrlbl_0;
        }
        {
          ec_ctrlbl_2: ;
          c(); d();
        }
        {
          ec_ctrlbl_3: ;
          e(); f(); return;
        }
        ec_ctrlbl_0: ;
      }
    }
  |], [paste|
    void foo(int i) {
      if (i==1) goto ec_ctrlbl_1;
      if (i==2) goto ec_ctrlbl_2;
      if (i==3) goto ec_ctrlbl_3;
      ec_ctrlbl_1: ;
      a(); b();
      goto ec_ctrlbl_0;
      ec_ctrlbl_2: ;
      c(); d();
      ec_ctrlbl_3: ;
      e(); f(); return;
      ec_ctrlbl_0: ;
    }
  |])
  , -- 13 - switch statement with default {{{2
  ([paste|
    void foo(int i) {
      {
        if (i==1) goto ec_ctrlbl_1;
        if (i==2) goto ec_ctrlbl_2;
        goto ec_ctrlbl_3;
        
        {
          ec_ctrlbl_1: ;
          a(); b();
          goto ec_ctrlbl_0;
        }
        {
          ec_ctrlbl_2: ;
          c(); d();
        }
        {
          ec_ctrlbl_3: ;
          e(); f();
        }
        ec_ctrlbl_0: ;
      }
    }
  |], [paste|
    void foo(int i) {
      if (i==1) goto ec_ctrlbl_1;
      if (i==2) goto ec_ctrlbl_2;
      goto ec_ctrlbl_3;
      ec_ctrlbl_1: ;
      a(); b();
      goto ec_ctrlbl_0;
      ec_ctrlbl_2: ;
      c(); d();
      ec_ctrlbl_3: ;
      e(); f();
      ec_ctrlbl_0: ;
    }
  |])
  , -- 14 - empty statements -- {{{2
  ([paste|
    void foo() {
      i++;
      ;
      {
        i*=2;
        ;
      }
      i/=2;
      {
        ;
      }
      i--;
    }
  |], [paste|
    void foo () {
      i++;
      i*=2;
      i/=2;
      i--;
    }
  |])

  -- end {{{2
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{2
    runTest (inputCode, expectedCode) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7) = enrich inputCode
        outputItems = map CBlockStmt $ sequencialize_body inputItems
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in
        expectedCode' @=? outputCode

test_normalize_critical_calls :: Test -- {{{1
test_normalize_critical_calls = enumTestGroup "normalize_critical_calls" $ map runTest [
  -- , 01 - critical call in return statement {{{2
  ([paste|
    int foo() {
      return bar() + 23;
    } 

    int bar() { return 0; }
  |], [paste|
    int foo() {
      ec_crit_0 = bar();
      return ec_crit_0 + 23;
    }
  |], [
      "int ec_crit_0"
  ])
  , -- 02 - critical call in condition of if statement {{{2
  ([paste|
    int foo() {
      if (bar() == 'a') return 0; else return 1;
    } 

    char bar() { return 0; }
  |], [paste|
    int foo() {
      ec_crit_0 = bar();
      if (ec_crit_0 == 'a') return 0; else return 1;
    }
  |], [
      "char ec_crit_0"
  ])
  , -- 03 - critical call in nested expressions {{{2
  ([paste|
    void foo() {
      i = bar() + 23;
    } 

    double bar() { return 0; }
  |], [paste|
    void foo() {
      ec_crit_0 = bar();
      i = ec_crit_0 + 23;
    }
  |], [
      "double ec_crit_0"
  ])
  , -- 04 - empty function {{{2
  ("void foo(){ }", "void foo() { }", [])
  -- end {{{2
  ]
  where
    runTest :: (String, String, [String]) -> Assertion -- {{{2
    runTest (inputCode, expectedCode, expectedDecls) =
      let
        (CTranslUnit eds y) = enrich inputCode
        (fd:fds) = map unwrapFd eds
        cf       = M.fromList $ zip (map symbol fds) fds 
        (CFunDef x1 x2 x3 (CCompound x4 bitems x5) x6) = fd
        inputItems = map unwrapB bitems
        (outputItems, outputVariables) = normalize_critical_calls cf inputItems
        outputFd = CFunDef x1 x2 x3 (CCompound x4 (map CBlockStmt outputItems) x5) x6
        outputAst = CTranslUnit [CFDefExt outputFd] y
        outputCode = reduce outputAst
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputDecls = map (show . pretty . var_decl) outputVariables
      in do
        expectedCode' @=? outputCode
        expectedDecls @=? outputDecls

    unwrapFd (CFDefExt fd) = fd
    unwrapFd _             = error "unwrapFd"

    unwrapB (CBlockStmt s) = s
    unwrapB _              = error "unwrapB"

test_build_basic_blocks :: Test -- {{{1
test_build_basic_blocks = enumTestGroup "build_basic_blocks" $ map runTest [
  -- , 01 - just return {{{2
  ([],
  [paste|
    void foo() {
      return;      
    }
  |], [paste|
    L1:
    RETURN
  |], "L1")
  , -- 02 - while loop {{{2
  ([],
  [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      if (! 1) goto ec_ctrlbl_1;
      g();
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |], [paste|
    L1:
    a();
    GOTO L2/ec_ctrlbl_0

    L2/ec_ctrlbl_0:
    IF !1 THEN L4/ec_ctrlbl_1 ELSE L3

    L3:
    g();
    GOTO L2/ec_ctrlbl_0

    L4/ec_ctrlbl_1:
    b();
    RETURN
  |], "L1")
  , -- 03 - do loop {{{2
  ([], 
  [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      g();
      if (1) goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |], [paste|
    L1:
    a();
    GOTO L2/ec_ctrlbl_0

    L2/ec_ctrlbl_0:
    g();
    IF 1 THEN L2/ec_ctrlbl_0 ELSE L3/ec_ctrlbl_1

    L3/ec_ctrlbl_1:
    b();
    RETURN
  |], "L1")
  , -- 04 - continue and break {{{2
  ([],
  [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      goto ec_ctrlbl_0;
      g();
      goto ec_ctrlbl_1;
      if (1) goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |], [paste|
    L1:
    a();
    GOTO L2/ec_ctrlbl_0

    L2/ec_ctrlbl_0:
    GOTO L2/ec_ctrlbl_0

    L3:
    g();
    GOTO L5/ec_ctrlbl_1

    L4:
    IF 1 THEN L2/ec_ctrlbl_0 ELSE L5/ec_ctrlbl_1
 
    L5/ec_ctrlbl_1:
    b();
    RETURN
  |], "L1")
  , -- 05 - switch statement {{{2
  ([],
  [paste|
    void foo(int i) {
      if (i==1) goto ec_ctrlbl_1;
      if (i==2) goto ec_ctrlbl_2;
      if (i==3) goto ec_ctrlbl_3;
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      a(); b();
      goto ec_ctrlbl_0;
      ec_ctrlbl_2: ;
      c(); d();
      ec_ctrlbl_3: ;
      e(); f(); return;
      ec_ctrlbl_0: ;
    }
  |], [paste|
    L1:
    IF i == 1 THEN L5/ec_ctrlbl_1 ELSE L2

    L2:
    IF i == 2 THEN L6/ec_ctrlbl_2 ELSE L3
 
    L3:
    IF i == 3 THEN L7/ec_ctrlbl_3 ELSE L4

    L4:
    GOTO L8/ec_ctrlbl_0

    L5/ec_ctrlbl_1:
    a();
    b();
    GOTO L8/ec_ctrlbl_0

    L6/ec_ctrlbl_2:
    c();
    d();
    GOTO L7/ec_ctrlbl_3

    L7/ec_ctrlbl_3:
    e();
    f();
    RETURN

    L8/ec_ctrlbl_0:
    RETURN
  |], "L1")
  , -- 06 - critical call {{{2
  (["g"],
  [paste|
    void foo() {
      ec_ctrlbl_0: ;
      if (! 1) goto ec_ctrlbl_1;
      g();
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
    }
  |], [paste| 
    L1/ec_ctrlbl_0:
    IF !1 THEN L4/ec_ctrlbl_1 ELSE L2
    
    L2:
    g(); GOTO L3

    L3:
    GOTO L1/ec_ctrlbl_0

    L4/ec_ctrlbl_1:
    RETURN
  |], "L1/ec_ctrlbl_0")
  -- end {{{2
  ]
  where
    runTest :: ([String], String, String, String) -> Assertion -- {{{2
    runTest (cf, inputCode, expectedIr, expectedEntry) = 
      let
        (CTranslUnit [CFDefExt fd] _) = enrich inputCode :: CTranslationUnit NodeInfo
        (CFunDef _ _ _ (CCompound _ bitems _) _) = fd
        inputItems = map (\(CBlockStmt s) -> s) bitems
        (outputEntry, outputIr) = build_basic_blocks (S.fromList cf) inputItems
        expectedIr' = (unlines . drop 1 . map (drop 4) . lines) expectedIr
      in do
        expectedIr' @=? showGraph show outputIr
        expectedEntry @=? show outputEntry

test_ast_2_ir :: Test  -- {{{1
test_ast_2_ir = enumTestGroup "ast_2_ir" $ map runTest [
  ]
  where
    runTest = undefined

test_critical_variables :: Test -- {{{1
test_critical_variables = enumTestGroup "critical_variables" $ map runTest [
  -- , 01 - single critical variable {{{2
  ([paste|
    int foo() {
      int i = 0;
      g();
      return i;
    }

    void g() { }
  |], ["i"], [])
  , -- 02 - single non-critical variable {{{2
  ([paste|
    int foo() {
      for (int i=0; i<23; i++) ;
      g();
      return 23;
    }

    void g() { }
  |], [], ["i"])
  , -- 03 - single critical variable in loop {{{2
  ([paste|
    void foo() {
      for (int i=0; i<23; i++) {
        g();
      }
    }

    void g() { }
  |], ["i"], [])
  , -- 04 - both critical and non-critical variables {{{2
  ([paste|
    void foo() {
      int j;
      for (int i=0; i<23; i++) {
        g();
      }
      j = 23;
    }

    void g() { }
  |], ["i"], ["j"])
  , -- 05 - kill liveness {{{2
  ([paste|
    void foo() {
      int j = 23;
      g();
      j = 42;
    }
  |], [], ["j"]) 
  , -- 06 - don't reuse {{{2
  ([paste|
    void foo() {
      int j = 23;
      g();
    }
  |], [], ["j"])
  -- end {{{2
  ]
  where
    runTest :: (String, [String], [String]) -> Assertion
    runTest (inputCode, expectedCriticalVars, expecedUncriticalVars) =
      let
        (CTranslUnit eds _) = enrich inputCode
        fds = map unwrapFd eds
        cf = M.fromList $ map (\fd -> (symbol fd, fd)) fds
        funs = ast_2_ir cf
        fun = $fromJust_s $ M.lookup ((symbol . head) fds) funs
        (Function outputCriticalVars outputUncriticalVars _ _ _) = critical_variables fun
        outputCriticalVars' = map var_fqn outputCriticalVars
        outputUncriticalVars' = map var_fqn outputUncriticalVars
      in do
        assertEqual "critical variables" expectedCriticalVars outputCriticalVars'
        assertEqual "uncritical variables" expecedUncriticalVars outputUncriticalVars'

    unwrapFd (CFDefExt fd) = fd
    unwrapFd _             = error "unwrapFd"
