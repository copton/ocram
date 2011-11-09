module Ocram.Test.Tests.Transformation.Normalize
-- export {{{1
(
  tests
) where

-- imports {{{1
import Control.Monad.Writer (runWriter)
import Ocram.Types
import Ocram.Test.Lib
import Ocram.Transformation.Inline.Normalize (normalize)
import Ocram.Analysis (analysis)
import Ocram.Text (show_errors)
import Test.HUnit

tests = TestLabel "Normalize" $ TestList $ map runTest [ -- {{{1
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
      }
    |]),
-- critical call in if condition -- {{{2
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        if (block()) ;
        else ;

        if (block() < 23) ;
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int ec_tmp_0 = block();
        if (ec_tmp_0) {
          ;
        } else {
          ;
        }

        int ec_tmp_1 = block();
        if (ec_tmp_1 < 23) {
          ;
        }
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
        int i, *j;
        int ec_tmp_0 = block();
        i = ec_tmp_0 < 23;
        int ec_tmp_1 = block();
        i = j[ec_tmp_1];
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
        int i = block();
        for (;;) {;}
        int j=0, k=block();
        for (;;) {;}
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
            result = reduce $ fst $ runWriter (normalize cg ast)
            expected' = (reduce $ (enrich expected :: Ast) :: String)
          in
            expected' @=? result
