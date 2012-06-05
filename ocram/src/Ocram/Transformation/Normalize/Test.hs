module Ocram.Transformation.Normalize.Test
-- exports {{{1
(
  tests
) where

-- imports {{{1
import Language.C.Data.Node (nodeInfo)
import Language.C.Syntax.AST
import Ocram.Debug (enrichNodeInfo, ENodeInfo)
import Ocram.Test.Lib (enumTestGroup, paste, enrich, reduce)
import Ocram.Transformation.Normalize.Internal
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), Assertion)

tests :: Test -- {{{1
tests = testGroup "Normalize" [
    test_desugar_control_structures
--    test_explicit_return
--  , test_wrap_dangling_statements
--  , test_unlist_declarations
  ]

test_desugar_control_structures :: Test -- {{{1
test_desugar_control_structures = enumTestGroup "desugar_control_structures" $ map (runTest desugar_control_structures) [
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
          ec_ctrlbl_0: ;
          if (! 1) { goto ec_ctrlbl_1; }
          g();
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
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
          ec_ctrlbl_0: ;
          g();
          if (1) {goto ec_ctrlbl_0;}
          ec_ctrlbl_1: ;
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
          ec_ctrlbl_0: ;
          if (! (i<23)) { goto ec_ctrlbl_1; }
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
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
          ec_ctrlbl_0: ;
          if (! (i<23)) { goto ec_ctrlbl_1; }
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
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
          ec_ctrlbl_0: ;
          if (! (i<23)) { goto ec_ctrlbl_1; }
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
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
          ec_ctrlbl_0: ;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
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
          ec_ctrlbl_0: ;
          if (! (i<23)) { goto ec_ctrlbl_1; }
          g(i);
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
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
          ec_ctrlbl_0: ;
          goto ec_ctrlbl_0;
          g();
          goto ec_ctrlbl_1;
          if (1) {goto ec_ctrlbl_0;}
          ec_ctrlbl_1: ;
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
          ec_ctrlbl_2: ;
          if (!1) {goto ec_ctrlbl_3;}
          b();
          goto ec_ctrlbl_2;
          c();
          {
            ec_ctrlbl_0: ;
            d();
            goto ec_ctrlbl_0;
            e();
            goto ec_ctrlbl_1;
            f();
            if (23) { goto ec_ctrlbl_0; }
            ec_ctrlbl_1: ;
          }
          g();
          goto ec_ctrlbl_3;
          h();
          goto ec_ctrlbl_2;
          ec_ctrlbl_3: ; 
        }
        i(); 
      }
    |])
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
test_wrap_dangling_statements :: Test -- {{{1
test_wrap_dangling_statements = enumTestGroup "wrap_dangling_statements" $ map (runTest wrap_dangling_statements) [
-- generic case {{{2
    ([paste|
      void foo() {
        if (1) ;
        else if (2) ;
        while(1) block();
        for (;;) block();
      }
  |], [paste|
      void foo() {
        if (1) {
          ;
        } else {
          if (2) {
            ;
          }
        }
        while(1) {
          block();
        }
        for(;;) {
          block();
        }
      }
  |])
-- nested {{{2
    , ([paste|
        void foo() {
          { if (1) ;
          else if (2) ; }
          { while(1) block(); }
          { for (;;) block(); }
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
          { while(1) {
            block();
          } }
          { for(;;) {
            block();
          } }
        }
    |])
  ]


test_unlist_declarations :: Test -- {{{1
test_unlist_declarations = enumTestGroup "unlist_declarations" $ map (runTest unlist_declarations) [
-- unlist declarations -- {{{2
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
-- unlist declarations - nested scope-- {{{2
  , ([paste|
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


runTest :: (CFunctionDef ENodeInfo -> CFunctionDef ENodeInfo) -> (String, String) -> Assertion -- {{{1
runTest f (code, expected) =
  let
    (CTranslUnit [CFDefExt (fd)] ni) = fmap enrichNodeInfo $ enrich code
    result = (reduce $ fmap nodeInfo $ CTranslUnit [CFDefExt (f fd)] ni) :: String
    expected' = reduce $ (enrich expected :: CTranslUnit)
  in
    expected' @=? result

