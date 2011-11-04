module Ocram.Test.Tests.Transformation.Normalize
-- export {{{1
(
  tests
) where

-- imports {{{1
import Ocram.Types
import Ocram.Test.Lib
import Ocram.Transformation.Inline.Normalize (normalize)
import Ocram.Transformation.Inline.Types (execWR)
import Ocram.Analysis (analysis)
import Ocram.Text (show_errors)
import Test.HUnit

tests = TestLabel "Normalize" $ TestList $ map runTest [ -- {{{1
    ([paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        if (block()) {
          return;
        } else {
          return;
        }  
      }
    |], [paste|
      __attribute__((tc_blocking)) int block();
      __attribute__((tc_run_thread)) void start() {
        int ec_tmp_0 = block();
        if (ec_tmp_0) {
          return;
        } else {
          return;
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
            result = reduce $ fst $ execWR cg (normalize ast)
            expected' = (reduce $ (enrich expected :: Ast) :: String)
          in
            expected' @=? result
