{-# LANGUAGE QuasiQuotes #-}
module Ocram.Transformation.Normalize.Test
-- exports {{{1
(
  tests
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Data.Generics (everything, mkQ)
import Language.C.Data.Node (nodeInfo)
import Language.C.Syntax.AST
import Ocram.Debug (enrich_node_info, ENodeInfo(..), Substitution(..))
import Ocram.Symbols (Symbol, symbol)
import Ocram.Test.Lib (enumTestGroup, paste, enrich, reduce)
import Ocram.Transformation.Normalize.Internal
import Ocram.Transformation.Normalize.ShortCircuiting
import Ocram.Transformation.Normalize.UniqueIdentifiers
import Ocram.Transformation.Types
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), Assertion, assertBool)

import qualified Data.Set as Set

tests :: Test -- {{{1
tests = testGroup "Normalize" [
    test_explicit_return
  , test_desugar_control_structures
  , test_short_circuiting
  , test_wrap_dangling_statements
  , test_unlist_declarations
  , test_unique_identifiers
  , test_defer_criticial_initialization
  , test_critical_statemtents
  ]

test_explicit_return :: Test -- {{{1
test_explicit_return = enumTestGroup "explicit_return" $ map (runTest explicit_return) [
-- empty function {{{2
    ([paste|
      void foo() { }
    |], [paste|
      void foo() {
        return;
      }
    |])
-- explicit return {{{2
  , ([paste|
      void foo() {
        return;  
      }
    |], [paste|
      void foo() {
        return;  
      }
    |])
-- non-void function {{{2
  , ([paste|
      int foo() {
        return 23;
      }
    |], [paste|
      int foo() {
        return 23;
      }
    |])
-- unreachable code {{{2
-- TODO: avoid extra return statement
  , ([paste|
      void foo(int i) {
        if (i) {
          return;
        } else {
          return;
        }
      }
    |], [paste|
      void foo(int i) {
        if (i) {
          return;
        } else {
          return;
        }
        return;
      }
    |])
  ]
test_desugar_control_structures :: Test -- {{{1
test_desugar_control_structures = enumTestGroup "desugar_control_structures" $ map (runTest (desugar_control_structures 1)) [
  -- while loop {{{2
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
          ec_ctrlbl_1_0: ;
          if (! 1) goto ec_ctrlbl_1_1;
          g();
          goto ec_ctrlbl_1_0;
          ec_ctrlbl_1_1: ;
        }
        b();
      }
  |])
  , -- do loop {{{2
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
  , -- for loop {{{2
  ([paste|
      void foo() {
        a();
        int i;
        for (i = 0; i<23; i++) {
          g(i);
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        int i;
        {
          i = 0;
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
  , -- for loop - with declaration {{{2
  ([paste|
      void foo() {
        a();
        for (int i = 0; i<23; i++) {
          g(i);
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          int i = 0;
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
  , -- for loop - no init expression {{{2
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
  , -- for loop - no break condition {{{2
  ([paste|
      void foo() {
        a();
        int i;
        for (i = 0; ; i++) {
          g(i);
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        int i;
        {
          i = 0;
          ec_ctrlbl_1_0: ;
          g(i);
          i++;
          goto ec_ctrlbl_1_0;
          ec_ctrlbl_1_1: ;
        }
        b();
      }
    |])
  , -- for loop - no incr expression{{{2
  ([paste|
      void foo() {
        a();
        int i;
        for (i = 0; i<23; ) {
          g(i);
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        int i;
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
  , -- continue and break {{{2
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
  , -- nested {{{2
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
  ]

test_short_circuiting :: Test -- {{{1
test_short_circuiting = enumTestGroup "boolean_shortcutting" $ map (runTest' (short_circuiting 1)) [
    -- no critical function {{{2
  ([],
  [paste|
    void foo() {
      if(g() || h()) ;
    }
  |], [paste|
    void foo() {
      if(g() || h()) ;
    }
  |])
  , -- critical function on left hand side, or expression {{{2
  (["g"],
  [paste|
    void foo() {
      if(g() || h()) ;
    }
  |], [paste|
    void foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!g();
        if (! ec_bool_1_0) {
          ec_bool_1_0 = !!h();
        }
        if (ec_bool_1_0) ;
      }
    }
  |])
  , -- critical function on right hand side, and expression {{{2
  (["h"],
  [paste|
    void foo() {
      if(g() && h()) ;
    }
  |], [paste|
    void foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!g();
        if (ec_bool_1_0) {
          ec_bool_1_0 = !!h();
        }
        if (ec_bool_1_0) ;
      }
    }
  |])
  , -- expression statement {{{2
  (["g"],
  [paste|
    void foo() {
      g() || h();
    }
  |], [paste|
    void foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!g();
        if (! ec_bool_1_0) {
          ec_bool_1_0 = !!h();
        }
        ec_bool_1_0;
      }
    }
  |])
  , -- switch statement {{{2
  (["g"],
  [paste|
    void foo() {
      switch(g() || h()) ;
    }
  |], [paste|
    void foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!g();
        if (! ec_bool_1_0) {
          ec_bool_1_0 = !!h();
        }
        switch(ec_bool_1_0) ;
      }
    }
  |])
  , -- return statement {{{2
  (["g"],
  [paste|
    int foo() {
      return (g() || h()) ;
    }
  |], [paste|
    int foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!g();
        if (! ec_bool_1_0) {
          ec_bool_1_0 = !!h();
        }
        return ec_bool_1_0;
      }
    }
  |])
  , -- function call {{{2
  (["g"],
  [paste|
    void foo() {
      h(k() || g());
    }
  |], [paste|
    void foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!k();
        if (! ec_bool_1_0) {
          ec_bool_1_0 = !!g();
        }
        h(ec_bool_1_0);
      }
    }
  |])
  , -- within algebraic expression {{{2
  (["g"],
  [paste|
    void foo() {
      if ((g() || 1) + 3);
    }
  |], [paste|
    void foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!g();
        if (! ec_bool_1_0) {
          ec_bool_1_0 = !!1;
        }
        if (ec_bool_1_0 + 3);
      }
    }
  |])
  , -- containing algebraic expression {{{2
  (["g"],
  [paste|
    void foo() {
      if ((1+g()) || h());
    }
  |], [paste|
    void foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!(1 + g());
        if (! ec_bool_1_0) {
          ec_bool_1_0 = !!h();
        }
        if (ec_bool_1_0);
      }
    }
  |])
  , -- nested {{{2
  (["g1", "g2"],
  [paste|
    void foo() {
      if ((g1() || h1()) && (h2() || g2()));
    }
  |], [paste|
    void foo() {
      {
        int ec_bool_1_0;
        ec_bool_1_0 = !!g1();
        if (! ec_bool_1_0) {
          ec_bool_1_0 = !!h1();
        }
        int ec_bool_1_2;
        ec_bool_1_2 = !!ec_bool_1_0;
        if (ec_bool_1_2) {
          int ec_bool_1_1;
          ec_bool_1_1 = !!h2();
          if (!ec_bool_1_1) {
            ec_bool_1_1 = !!g2();
          }
          ec_bool_1_2 = !!ec_bool_1_1;
        }
        if (ec_bool_1_2);
      }
    }
  |])
  , -- generic case {{{2
  (["g", "h"],
  [paste|
      void foo() {
        int i;
        if ((g() || (i = x(), 1)) && h());
      }
  |], [paste|
      void foo() {
        int i;
        {
          int ec_bool_1_0;
          ec_bool_1_0 = !!g();
          if (! ec_bool_1_0) {
            ec_bool_1_0 = !! (i = x(), 1);
          }
          int ec_bool_1_1;
          ec_bool_1_1 = !!ec_bool_1_0;
          if (ec_bool_1_1) {
            ec_bool_1_1 = !!h();
          }
          if (ec_bool_1_1);
        }
      }
  |])
  ]
test_wrap_dangling_statements :: Test -- {{{1
test_wrap_dangling_statements = enumTestGroup "wrap_dangling_statements" $ map (runTest wrap_dangling_statements) [
-- generic case {{{2
    ([paste|
      void foo(int i) {
        if (1) ;
        else if (2) ;
        switch (i)
          default: ;
      }
  |], [paste|
      void foo(int i) {
        if (1) {
          ;
        } else {
          if (2) {
            ;
          }
        }
        switch (i) {
          default: ;
        }
      }
  |])
-- nested {{{2
    , ([paste|
        void foo() {
          { if (1) ;
          else if (2) ; }
          { switch (i)
              case 23: ;
          }
        }
    |], [paste|
        void foo() {
          { if (1) {
            ;
          } else {
            if (2) {
              ;
            }
          } }
          { switch (i) {
            case 23: ;
          } }
        }
    |])
  ]


test_unlist_declarations :: Test -- {{{1
test_unlist_declarations = enumTestGroup "unlist_declarations" $ map (runTest unlist_declarations) [
    -- two ints {{{2
  ([paste|
      void foo() {
        int i, j;
      }
  |], [paste|
      void foo() {
        int i;
        int j;
      }
  |])
  , -- nested scope-- {{{2
  ([paste|
    void foo() {
      {
        int i, j;
      }
    }
  |], [paste|
    void foo() {
      {
        int i;
        int j;
      }
    }
  |])
  ]

test_unique_identifiers :: Test -- {{{1
test_unique_identifiers = enumTestGroup "unique_identifiers" $ map runTest'' [
    -- nothing to do {{{2
  ([paste|
    void foo() {
      int i;
      i++;
    }
  |], [paste|
    void foo() {
      int i;
      i++;
    }
  |], [
    ("i", "i")
  ])
  , -- nested scope {{{2
  ([paste|
    void foo() {
      int i;
      {
        int i;
        i++;
      }
      ++i;
    }
  |], [paste|
    void foo() {
      int i;
      {
        int ec_shadow_i_0;
        ec_shadow_i_0++;
      }
      ++i;
    }
  |], [
      ("i", "i")
    , ("i", "ec_shadow_i_0")
  ])
  , -- with initializer {{{2
  ([paste|
    void foo() {
      int i = 23;
      i++;
    }
  |], [paste|
    void foo() {
      int i = 23;
      i++;
    }
  |], [
    ("i", "i")
  ])
  ]
  where
    runTest'' :: (String, String, [(Symbol, Symbol)]) -> Assertion  -- {{{2
    runTest'' (inputCode, expectedCode, expectedVarmap) =
      let
        (CTranslUnit [CFDefExt fd] ni) = fmap enrich_node_info $ enrich inputCode
        fd' = unique_identifiers fd
        resultCode = (reduce . fmap nodeInfo) (CTranslUnit [CFDefExt fd'] ni)
        expectedCode' = reduce $ (enrich expectedCode :: CTranslUnit) :: String
        subst = everything (++) (mkQ [] extract) fd'
        fname = symbol fd
        resultVarmap = map ((,) <$> substTVar <*> substEVar) subst
      in do
        assertBool "wrong function name" $ and $ map ((fname==) . substFunc) subst
        expectedCode' @=? resultCode
        expectedVarmap @=? resultVarmap
      where
        extract :: CDecl' -> [Substitution]
        extract (CDecl _ _ ni) = enSubst ni

test_critical_statemtents :: Test -- {{{1
test_critical_statemtents = enumTestGroup "critical_statements" $ map runTest'' [
    -- return statement {{{2
  ([paste|
    int foo() { return 23; }
    int bar() { return foo(); }
  |], [paste|
    int foo() { return 23; }
    int bar() {
      int ec_crit_1_0 = foo();
      return ec_crit_1_0;
    }
  |])
  , -- nested expression {{{2
  ([paste|
    int foo() { return 23; }
    void bar() {
      int i;
      i = foo() + 42;
    }
  |], [paste|
    int foo() { return 23; }
    void bar() {
      int i;
      int ec_crit_1_0 = foo();
      i = ec_crit_1_0 + 42;
    }
  |])
  , -- if statement {{{2
  ([paste|
    int foo() { return 23; }
    void bar() {
      if (foo() == 23)  { }
    }
   |], [paste|
    int foo() { return 23; }
    void bar() {
      int ec_crit_1_0 = foo();
      if (ec_crit_1_0 == 23) { }
    }
  |])
  ]
  where
  runTest'' :: (String, String) -> Assertion
  runTest'' (code, expected) =
    let
      ast = enrich code
      ast'@(CTranslUnit fds ni) = fmap enrich_node_info ast
      fds' = map (CFDefExt . critical_statements 1 cf ast' . (\(CFDefExt fd) -> fd)) fds
      cf = Set.fromList $ map symbol fds
      result = (reduce $ fmap nodeInfo $ CTranslUnit fds' ni) :: String
      expected' = reduce $ (enrich expected :: CTranslUnit)
    in
      expected' @=? result

test_defer_criticial_initialization :: Test -- {{{1
test_defer_criticial_initialization = enumTestGroup "defer_critical_initialization" $ map (runTest' defer_critical_initialization) [
    -- critical initialization {{{2
  (["g"],
   [paste|
    void foo() {
      int i = g(); 
    }
  |], [paste|
    void foo() {
      int i;
      i = g();
    } 
  |])
  ]


runTest :: (CFunDef' -> CFunDef') -> (String, String) -> Assertion -- {{{1
runTest f (code, expected) =
  let
    (CTranslUnit [CFDefExt fd] ni) = fmap enrich_node_info $ enrich code
    result = (reduce $ fmap nodeInfo $ CTranslUnit [CFDefExt (f fd)] ni) :: String
    expected' = reduce $ (enrich expected :: CTranslUnit)
  in
    expected' @=? result

runTest' :: (Set.Set Symbol -> CFunDef' -> CFunDef') -> ([String], String, String) -> Assertion -- {{{1
runTest' f (cf, code, expected) =
  let
    cf' = Set.fromList cf
    (CTranslUnit [CFDefExt fd] ni) = fmap enrich_node_info $ enrich code
    result = (reduce $ fmap nodeInfo $ CTranslUnit [CFDefExt (f cf' fd)] ni) :: String
    expected' = reduce $ (enrich expected :: CTranslUnit)
  in
    expected' @=? result


