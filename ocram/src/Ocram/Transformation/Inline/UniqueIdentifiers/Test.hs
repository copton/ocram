module Ocram.Transformation.Inline.UniqueIdentifiers.Test (tests) where

-- imports {{{1
import Control.Monad.Writer (runWriter)
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.Data.Node (nodeInfo)
import Ocram.Analysis (analysis)
import Ocram.Test.Lib (enumTestGroup, paste, enrich, reduce)
import Ocram.Text (show_errors)
import Ocram.Transformation.Inline (enodify)
import Ocram.Transformation.Inline.UniqueIdentifiers (unique_identifiers)
import Test.Framework (Test)
import Test.HUnit (Assertion, assertFailure, (@=?))

tests :: Test
tests = enumTestGroup "UniqueIdentifiers" $ map runTest [ -- {{{1
-- simple decl -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    int k;
    __attribute__((tc_run_thread)) void start() {
      int k;
      block(k);
    }
  |], [paste|
    __attribute__((tc_blocking)) void block(int i);
    int k;
    __attribute__((tc_run_thread)) void start() {
      int k_0;
      block(k_0);
    }
  |]),
-- trans critical functions -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    int k;
    void crit() {
      int k;
      block(k);
    }
    __attribute__((tc_run_thread)) void start() {
      int k;
      crit(k);
    }
  |], [paste|
    __attribute__((tc_blocking)) void block(int i);
    int k;
    void crit() {
      int k_0;
      block(k_0);
    }
    __attribute__((tc_run_thread)) void start() {
      int k_0;
      crit(k_0);
    }
  |]),
-- non-critical functions -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    int k;
    int foo() {
      int k;
      return k;
    }
    __attribute__((tc_run_thread)) void start() {
      int k;
      k = foo();
      block(k);
    }
  |], [paste|
    __attribute__((tc_blocking)) void block(int i);
    int k;
    int foo() {
      int k;
      return k;
    }
    __attribute__((tc_run_thread)) void start() {
      int k_0;
      k_0 = foo();
      block(k_0);
    }
  |]),
-- for loop -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    int k;
    __attribute__((tc_run_thread)) void start() {
      for (int k=0; k<1; k++) {
        block(k);
      }
    }
  |], [paste|
    __attribute__((tc_blocking)) void block(int i);
    int k;
    __attribute__((tc_run_thread)) void start() {
      for (int k_0=0; k_0<1; k_0++) {
        block(k_0);
      }
    }
  |]),
-- nested scope -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    int k;
    __attribute__((tc_run_thread)) void start() {
      int k;
      {
        int k;
        block(k);
      }
      block(k);
    }
  |], [paste|
    __attribute__((tc_blocking)) void block();
    int k;
    __attribute__((tc_run_thread)) void start() {
      int k_0;
      {
        int k_1; 
        block(k_1); 
      }
      block(k_0);
    }
  |]),
-- left scope -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    int k;
    __attribute__((tc_run_thread)) void start() {
      {
        int k;
        block(k);
      }
      int k;
      block(k);
    }
  |], [paste|
    __attribute__((tc_blocking)) void block();
    int k;
    __attribute__((tc_run_thread)) void start() {
      {
        int k_0; 
        block(k_0); 
      }
      int k_1;
      block(k_1);
    }
  |]),
-- multi decl -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    int h;
    int k;
    __attribute__((tc_run_thread)) void start() {
      int k, j;
      int a, b;
      for(int i=0, h=0;;) {
        block(h);
      }
    }
  |], [paste|
    __attribute__((tc_blocking)) void block();
    int h;
    int k;
    __attribute__((tc_run_thread)) void start() {
      int k_0, j;
      int a, b;
      for(int i=0, h_0=0;;) {
        block(h_0);
      }
    }
  |]),
-- function defintion -- {{{2
  ([paste|
    __attribute__((tc_blocking)) int block();
    int critical() {
      return block();
    }
    __attribute__((tc_run_thread)) void start() {
      critical();
    }
  |], [paste|
    __attribute__((tc_blocking)) int block();
    int critical() {
      return block();
    }
    __attribute__((tc_run_thread)) void start() {
      critical();
    }
  |]),
-- reserved identifiers -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
      block(__RESERVED__);
    }
  |], [paste|
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
      block(__RESERVED__);
    }
  |]),
-- extra function declarations -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    void critical();
    void critical() {
      block(23);
    }
    __attribute__((tc_run_thread)) void start() {
      critical();
    }
  |], [paste|
    __attribute__((tc_blocking)) void block(int i);
    void critical();
    void critical() {
      block(23);
    }
    __attribute__((tc_run_thread)) void start() {
      critical();
    }
  |]),
-- enum -- {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    enum E { A, B, C };
    __attribute__((tc_run_thread)) void start() {
      block(B);
    }
  |], [paste|
    __attribute__((tc_blocking)) void block(int i);
    enum E { A, B, C };
    __attribute__((tc_run_thread)) void start() {
      block(B);
    }
  |])
-- cast
  ,([paste|
    __attribute__((tc_blocking)) void block(char* c);
    __attribute__((tc_run_thread)) void start() {
      int i;
      block((char*)&i);
    }
  |], [paste|
    __attribute__((tc_blocking)) void block(char* c);
    __attribute__((tc_run_thread)) void start() {
      int i;
      block((char*)&i);
    }
  |])
  ]
 
runTest :: (String, String) -> Assertion -- {{{1
runTest (code, expected) =
  let ast = enrich code in
  case analysis ast of
    Left es -> assertFailure $ show_errors "analysis" es
    Right (cg, _) ->
      let
        result = reduce $ fmap nodeInfo $ fst $ runWriter $ unique_identifiers cg $ enodify ast
        expected' = (reduce $ (enrich expected :: CTranslUnit) :: String)
      in
        expected' @=? result
