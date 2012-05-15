module Ocram.Transformation.Inline.Normalize.Test (tests) where

-- imports {{{1
import Control.Monad.Writer (runWriter)
import Language.C.Syntax.AST (CTranslUnit)
import Ocram.Analysis (analysis)
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, paste)
import Ocram.Text (show_errors)
import Ocram.Transformation.Inline.Normalize (normalize)
import Test.Framework (Test)
import Test.HUnit (Assertion, assertFailure, (@=?))

tests :: Test
tests = enumTestGroup "Normalize" $ map runTest [ -- {{{1
-- unlist declarations -- {{{2
    ([paste|
      int a, b;
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i, j;
        block();
      }
    |], [paste|
      int a;
      int b;
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i;
        int j;
        block();
        return;
      }
    |]),
-- unlist declarations - nested scope-- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        {
          int i, j;
          block();
        }
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        {
          int i;
          int j;
          block();
        }
        return;
      }
    |]),
-- dangling statements -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        if (1) ;
        else if (2) ;
        while(1) block();
        for (;;) block();
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
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
        return;
      }
    |]),
-- dangling statements - nested -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        { if (1) ;
        else if (2) ; }
        { while(1) block(); }
        { for (;;) block(); }
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
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
        return;
      }
    |]),
-- critical call in initialization -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i = block();
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i;
        i = block();
        return;
      }
    |]),
-- critical call in initialization - nested -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        {
          int i = block();
        }
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        {
          int i;
          i = block();
        }
        return;
      }
    |]),
-- critical call in if condition -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        if (block()) ;
        if (block() < 23) ;
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int ec_tmp_0 = block();
        if (ec_tmp_0) {
          ;
        }

        int ec_tmp_1 = block();
        if (ec_tmp_1 < 23) {
          ;
        }
        return;
      }
    |]),
-- critical call in if condition - nested -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        { if (block()) ; }
        { if (block() < 23) ; }
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        { int ec_tmp_0 = block();
        if (ec_tmp_0) {
          ;
        } }

        { int ec_tmp_1 = block();
        if (ec_tmp_1 < 23) {
          ;
        } }
        return;
      }
    |]),
-- critical call in nested expression -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i, *j;
        i = block() < 23;
        i = j[block()];
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i;
        int *j;
        int ec_tmp_0 = block();
        i = ec_tmp_0 < 23;
        int ec_tmp_1 = block();
        i = j[ec_tmp_1];
        return; 
      }
    |]),
-- critical call in condition of while loop -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        while (block() < 23) ;
      } 
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        while (1) {
          int ec_tmp_0 = block();
          if (! (ec_tmp_0 < 23)) {
            break;
          }
          ;
        }
        return; 
      } 
    |]),
-- critical call in condition of do loop -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        do { ; } while(block() < 23);
      } 
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        do {
          ;
          int ec_tmp_0 = block();
          if (! (ec_tmp_0 < 23)) {
            break;
          }
        } while(1);
        return; 
      } 
    |]),
-- critical call in condition of for loop -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        for (; block() < 23; ) ;
      } 
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        for (;;) {
          ;
          int ec_tmp_0 = block();
          if (! (ec_tmp_0 < 23)) {
            break;
          }
        }
        return; 
      } 
    |]),
-- critical call in initial expression of for loop -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i;
        for (i = block(); ; ) ;
        for (i = block() + 23; ;) ;
      } 
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i;
        i = block();
        for (;;) {;}
        int ec_tmp_0 = block();
        i = ec_tmp_0 + 23;
        for (;;) {;}
        return; 
      } 
    |]),
-- critical call in declaration of for loop -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        for (int i = block(); ; ) ;
        for (int j=0, k=block(); ;) ;
      } 
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int i;
        i = block();
        for (;;) {;}
        int j=0;
        int k;
        k = block();
        for (;;) {;}
        return; 
      } 
    |]),
-- interactions -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int *j;
        j[block()] = 23;
        while (block() < 23) {
          j[block()] = 23;
        }
      } 
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int *j;
        int ec_tmp_0 = block();
        j[ec_tmp_0] = 23;
        while (1) {
          int ec_tmp_2 = block();
          if (! (ec_tmp_2 < 23)) {
            break;
          }
          int ec_tmp_1 = block();
          j[ec_tmp_1] = 23;
        }
        return; 
      } 
    |]),
-- critical call in l-value of 2nd normal form
  ([paste|
    __attribute__((tc_blocking)) int* block();
    __attribute__((tc_run_thread)) void start() {
      *block() = *block();
    }
  |], [paste|
    __attribute__((tc_blocking)) int* block();
    __attribute__((tc_run_thread)) void start() {
      int* ec_tmp_1 = block();
      int* ec_tmp_0 = block();
      *ec_tmp_0 = *ec_tmp_1;
      return;
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
        result = reduce $ fst $ runWriter (normalize cg ast)
        expected' = (reduce $ (enrich expected :: CTranslUnit) :: String)
      in
        expected' @=? result
