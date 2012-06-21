{-# LANGUAGE QuasiQuotes #-}
module Ocram.Transformation.Translate.Test
-- export {{{1
(
  tests
) where

-- imports {{{1
import Language.C.Syntax.AST
import Language.C.Data.Node (nodeInfo)
import Ocram.Analysis (analysis, CallGraph)
import Ocram.Debug (enrichNodeInfo, CTranslUnit')
import Ocram.Text (show_errors)
import Ocram.Test.Lib (enumTestGroup, paste, enrich, reduce)
import Ocram.Transformation.Translate.Internal
import Ocram.Transformation.Translate.ThreadFunctions
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), Assertion)

tests :: Test -- {{{1
tests = testGroup "Translate" [
    test_remove_critical_functions
  , test_add_blocking_function_decls
  , test_add_tstacks
  , test_add_thread_functions
  ]

test_remove_critical_functions :: Test -- {{{1
test_remove_critical_functions = enumTestGroup "remove_critical_functions" $ map (runTest remove_critical_functions) [
    -- minimal case {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    } 
    void non_critical() { }
  |], [paste|
    void non_critical() { }
  |])
  , -- with declarations {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    void foo();
    void foo() { block(); }
    void bar();
    void bar() { }
    __attribute__((tc_run_thread)) void start() {
      foo();
    } 
  |], [paste|
    void bar();
    void bar() { }
  |])
  , -- multiple global declarations {{{2
  ([paste|
    int i, j;
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    } 
    void non_critical() { }
  |], [paste|
    int i, j;
    void non_critical() { }
  |])
  ]

test_add_blocking_function_decls :: Test -- {{{1
test_add_blocking_function_decls = enumTestGroup "add_blocking_function_decls" $ map runTest' [
    -- minimal case {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();  
  |],
  "typedef struct { } ec_frame_block_t;\n",
  [paste|
    __attribute__((tc_blocking)) void block();  
    void block(ec_frame_block_t*);
  |])
  , -- multiple global declarations {{{2
  ([paste|
    int i, k;
    __attribute__((tc_blocking)) void block();  
  |],
  "typedef struct { } ec_frame_block_t;\n",
  [paste|
    int i, k;
    __attribute__((tc_blocking)) void block();  
    void block(ec_frame_block_t*);
  |])
  ]
  where
  runTest' (code, decl, expected) = -- {{{2
    let
      result = (reduce $ fmap nodeInfo $ add_blocking_function_decls $ fmap enrichNodeInfo $ enrich code) :: String
      expected' = reduce $ (enrich (decl ++ expected) :: CTranslUnit)
    in
      expected' @=? decl ++ result
    
test_add_tstacks :: Test -- {{{1
test_add_tstacks = enumTestGroup "add_tstacks" $ map (runTest add_tstacks) [
    -- minimal case {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    }
  |], [paste|
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start()
    {
        block();
    }
    typedef struct {
      void * ec_cont;
    } ec_frame_block_t;
    typedef struct {
      union {
        ec_frame_block_t block;
      } ec_frames;
    } ec_frame_start_t;
    ec_frame_start_t ec_stack_start;
  |])
  , -- multiple global declarations {{{2
  ([paste|
    int i,k;
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    }
  |], [paste|
    int i,k;
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start()
    {
        block();
    }
    typedef struct {
      void * ec_cont;
    } ec_frame_block_t;
    typedef struct {
      union {
        ec_frame_block_t block;
      } ec_frames;
    } ec_frame_start_t;
    ec_frame_start_t ec_stack_start;
  |])
  ]

test_add_thread_functions :: Test -- {{{1
test_add_thread_functions = enumTestGroup "add_thread_functions" $ map (runTest add_thread_functions) [
    -- minimal case {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    }
  |], [paste|
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start()
    {
        block();
    }
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
        block(&ec_stack_start.ec_frames.block);
        return;
    ec_label_start_1:
        ;
    }
  |])
  , -- multiple global declarations {{{2
  ([paste|
    int i, k;
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    }
  |], [paste|
    int i, k;
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start()
    {
        block();
    }
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
        block(&ec_stack_start.ec_frames.block);
        return;
    ec_label_start_1:
        ;
    }
  |])
  ]

runTest :: (CallGraph -> CTranslUnit' -> CTranslUnit') -> (String, String) -> Assertion -- {{{2
runTest f (code, expected) =
  let
    ast = enrich code :: CTranslUnit
    cg = case analysis ast of
      Left es -> error $ show_errors "test" es
      Right (x, _) -> x
    expected' = (reduce (enrich expected :: CTranslUnit)) :: String
    result = (reduce . fmap nodeInfo . f cg . fmap enrichNodeInfo) ast
  in
    expected' @=? result
