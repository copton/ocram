module Ocram.Test.Tests.Transformation.UniqueIdentifiers 
-- export {{{1
(
  tests
) where

-- imports {{{1
import Ocram.Types
import Ocram.Test.Lib
import Ocram.Transformation.Inline.UniqueIdentifiers (unique_identifiers)
import Ocram.Transformation.Inline.Types (execWR)
import Ocram.Analysis (analysis)
import Ocram.Text (show_errors)
import Test.HUnit

tests = runTests [ -- {{{1
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
  |])
  ]

runTests :: [(String, String)] -> Test -- {{{2
runTests cases = TestLabel "UniqueIdentifiers" $ TestList $ map runTest cases
  where
    runTest (code, expected) = TestCase $
      let ast = enrich code in
      case analysis ast of
        Left es -> assertFailure $ show_errors "analysis" es
        Right cg ->
          let
            result = reduce $ fst $ execWR cg (unique_identifiers ast)
            expected' = (reduce $ (enrich expected :: Ast) :: String)
          in
            expected' @=? result
