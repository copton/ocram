{-# LANGUAGE QuasiQuotes #-}
module Ocram.Backend.Test (tests) where

-- imports {{{1
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Analysis (analysis, Analysis(..))
import Ocram.Backend.BlockingFunctionDeclaration
import Ocram.Backend.EStack
import Ocram.Backend.ThreadExecutionFunction
import Ocram.Backend.TStack
import Ocram.Intermediate (ast_2_ir)
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, paste)
import Ocram.Text (show_errors)
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, (@=?))

tests :: Test -- {{{1
tests = testGroup "Backend" [test_create_tstacks]

test_create_tstacks :: Test  -- {{{1
test_create_tstacks = enumTestGroup "create_tstacks" $ map runTest [
  -- , 01 - minimal case {{{2
  ([paste| 
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    }
  |], [paste|
    typedef struct {
      void * ec_cont;
    } ec_tframe_block_t;

    typedef struct {
      union {
        ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;
  |])
  , -- 02 - blocking function with parameters
  ([paste| 
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
      block();
    }
  |], [paste|
    typedef struct {
      void * ec_cont;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      union {
        ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;
  |])
  , -- 02 - critical function
  ([paste| 
    __attribute__((tc_blocking)) void block(int i);
    void crit(int k) { block(k); }
    __attribute__((tc_run_thread)) void start() {
      crit(23);
    }
  |], [paste|
    typedef struct {
      void * ec_cont;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      void* ec_cont;
      union {
        ec_tframe_block_t block;
      } ec_frames;
      int k;
    } ec_tframe_crit_t;

    typedef struct {
      union {
        ec_tframe_crit_t crit;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;
  |])
  -- end {{{2
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{2
    runTest (inputCode, expectedDecls) =
      let
        ast = enrich inputCode :: CTranslUnit
        ana = case analysis ast of
          Left es -> error $ show_errors "test" es 
          Right x -> x
        ir = ast_2_ir (anaCritical ana)
        (frames, stacks) = create_tstacks (anaCallgraph ana) (anaBlocking ana) ir
        outputDecls = reduce (CTranslUnit (map CDeclExt (frames ++ stacks)) undefNode) :: String
        expectedDecls' = (reduce (enrich expectedDecls :: CTranslUnit)) :: String
      in
        expectedDecls' @=? outputDecls
