module Ocram.Test.Tests.Transformation.UniqueIdentifiers 
-- export {{{1
(
  tests
) where

-- imports {{{1
import Control.Monad.Writer (runWriter)
import Ocram.Types
import Ocram.Test.Lib
import Ocram.Transformation.Inline.UniqueIdentifiers (unique_identifiers)
import Ocram.Analysis (analysis)
import Ocram.Text (show_errors)
import Test.HUnit

tests = TestLabel "UniqueIdentifiers" $ TestList $ map runTest [ -- {{{1
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
  where
    runTest (code, expected) = TestCase $
      let ast = enrich code in
      case analysis ast of
        Left es -> assertFailure $ show_errors "analysis" es
        Right cg ->
          let
            result = reduce $ fst $ runWriter (unique_identifiers cg ast)
            expected' = (reduce $ (enrich expected :: Ast) :: String)
          in
            expected' @=? result
