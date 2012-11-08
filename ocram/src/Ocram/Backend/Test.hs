{-# LANGUAGE QuasiQuotes #-}
module Ocram.Backend.Test (tests) where

-- imports {{{1
import Language.C.Data.Node (undefNode, nodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis (analysis, Analysis(..))
import Ocram.Backend.EStack
import Ocram.Backend.ThreadExecutionFunction
import Ocram.Backend.TStack
import Ocram.Backend
import Ocram.Intermediate (ast_2_ir)
import Ocram.Print (render)
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, paste, lpaste, TVarMapEntry)
import Ocram.Text (show_errors)
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, (@=?), assertEqual)

import qualified Data.Map as M

tests :: Test -- {{{1
tests = testGroup "Backend" [
    test_create_tstacks
  , test_create_estacks
  , test_thread_execution_functions
  , test_tcode_2_ecode
  ]

test_create_tstacks :: Test  -- {{{1
test_create_tstacks = enumTestGroup "create_tstacks" $ map runTest [
  -- , 01 - minimal case {{{2
  ([paste| 
    __attribute__((tc_api)) void block();
    __attribute__((tc_thread)) void start() {
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
  , -- 02 - blocking function with parameters {{{2
  ([paste| 
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() {
      block(23);
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
  , -- 03 - critical function {{{2
  ([paste| 
    __attribute__((tc_api)) void block(int i);
    int crit(int k) { block(k); return k;}
    __attribute__((tc_thread)) void start() {
      crit(23);
    }
  |], [paste|
    typedef struct {
      void * ec_cont;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      void* ec_cont;
      int ec_result;
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
  , -- 04 - two threads {{{2
  ([paste|
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() {
      block(23);
    }
    __attribute__((tc_thread)) void run() {
      block(42);
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

    typedef struct {
      union {
        ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_run_t;

    ec_tframe_start_t ec_tstack_start;
    ec_tframe_run_t ec_tstack_run;
  |])
  , -- 05 - reentrance {{{2
  ([paste|
    __attribute__((tc_api)) void block(int i);
    int crit(int k) { block(k); return k; }
    __attribute__((tc_thread)) void start() {
      crit(23);
    }
    __attribute__((tc_thread)) void run() {
      crit(42);
    }
  |], [paste|
    typedef struct {
      void * ec_cont;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      void* ec_cont;
      int ec_result;
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

    typedef struct {
      union {
        ec_tframe_crit_t crit;
      } ec_frames;
    } ec_tframe_run_t;

    ec_tframe_start_t ec_tstack_start;
    ec_tframe_run_t ec_tstack_run;
  |])

  , --  06 - shadowing {{{2
  ([paste| 
    __attribute__((tc_api)) void block();
    __attribute__((tc_thread)) void start() {
      int i = 0;
      {
        int i = 1;
        block();
        i++;
      }
      i++;
    }
  |], [paste|
    typedef struct {
      void * ec_cont;
    } ec_tframe_block_t;

    typedef struct {
      union {
        ec_tframe_block_t block;
      } ec_frames;
      int ec_unique_i_0;
      int i;
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
        ir = ast_2_ir (anaBlocking ana) (anaCritical ana)
        (frames, stacks) = create_tstacks (anaCallgraph ana) (anaBlocking ana) ir
        outputDecls = reduce (CTranslUnit (map CDeclExt (map snd frames ++ stacks)) undefNode) :: String
        expectedDecls' = (reduce (enrich expectedDecls :: CTranslUnit)) :: String
      in
        expectedDecls' @=? outputDecls

test_create_estacks :: Test -- {{{1
test_create_estacks = enumTestGroup "create_estacks" $ map runTest [
  -- , 01 -  minimal case {{{2
  ([paste|
    __attribute__((tc_api)) void block();
    __attribute__((tc_thread)) void start() {
      block();
    }
  |], "", [("start", Nothing)])
  , -- 02 - uncritical variable {{{2
  ([paste|
    __attribute__((tc_api)) void block();
    __attribute__((tc_thread)) void start() {
      int i = 23;
      block(i);
    }
  |], [paste|
    typedef struct { 
      int i;
    } ec_eframe_start_t;
  |], [
      ("start", Just "union {\n    ec_eframe_start_t start;\n} ec_estack")
  ])
  , -- 03 - critical function {{{2
  ([paste| 
    __attribute__((tc_api)) void block(int i);
    int crit() { int k = 42; block(k);}
    __attribute__((tc_thread)) void start() {
      int i = 23;
      crit(i);
    }
  |], [paste|
    typedef struct {
      int k;
    } ec_eframe_crit_t;

    typedef struct {
      int i;
    } ec_eframe_start_t; 
  |], [
      ("start", Just "union {\n    ec_eframe_start_t start; ec_eframe_crit_t crit;\n} ec_estack")
  ]) 
  , -- 04 - two threads {{{2
  ([paste|
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() {
      int i = 23;
      block(i);
    }
    __attribute__((tc_thread)) void run() {
      int j = 42;
      block(j);
    }
  |], [paste|
    typedef struct {
      int i;
    } ec_eframe_start_t;

    typedef struct {
      int j;
    } ec_eframe_run_t;
  |], [
      ("run"  , Just "union {\n    ec_eframe_run_t run;\n} ec_estack")
    , ("start", Just "union {\n    ec_eframe_start_t start;\n} ec_estack")
  ])
  , -- 04 - reentrance {{{2
  ([paste|
    __attribute__((tc_api)) void block(int i);
    int crit() { int k = 23; block(k); }
    __attribute__((tc_thread)) void start() {
      int i = 42;
      crit(i);
    }
    __attribute__((tc_thread)) void run() {
      int j = 0xdeadbeef;
      crit(j);
    }
  |], [paste|
    typedef struct {
      int k; 
    } ec_eframe_crit_t;

    typedef struct {
      int i;
    } ec_eframe_start_t;

    typedef struct {
      int j;
    } ec_eframe_run_t;
  |], [
      ("run"  , Just "union {\n    ec_eframe_run_t run; ec_eframe_crit_t crit;\n} ec_estack")
    , ("start", Just "union {\n    ec_eframe_start_t start; ec_eframe_crit_t crit;\n} ec_estack")
  ])
    
  , -- 05 - shadowing {{{2
  ([paste|
    __attribute__((tc_api)) void block();
    __attribute__((tc_thread)) void start() {
      int i = 0;
      {
        int i = 1;
        i++;
      }
      i++;
      block();
    }
  |], [paste|
    typedef struct {
      int ec_unique_i_0;
      int i;
    } ec_eframe_start_t;
  |], [("start", Just "union {\n    ec_eframe_start_t start;\n} ec_estack")]
  )
  
  -- end {{{2
  ]
  where
    runTest :: (String, String, [(String, Maybe String)]) -> Assertion -- {{{2
    runTest (inputCode, expectedFrames, expectedStacks) =
      let
        ast = enrich inputCode :: CTranslUnit
        ana = case analysis ast of
          Left es -> error $ show_errors "test" es 
          Right x -> x
        ir = ast_2_ir (anaBlocking ana) (anaCritical ana)
        (frames, stacks) = create_estacks (anaCallgraph ana) ir        
        expectedFrames' = reduce (enrich expectedFrames :: CTranslUnit) :: String
        outputFrames = reduce (CTranslUnit (map CDeclExt frames) undefNode) :: String
        outputStacks = M.toList . M.map (fmap render) $ stacks 
      in do
        assertEqual "frames" expectedFrames' outputFrames
        assertEqual "stacks" expectedStacks outputStacks
        

test_thread_execution_functions :: Test -- {{{1
test_thread_execution_functions = enumTestGroup "thread_execution_functions" $ map runTest [
  -- , 01 - minimal case {{{2
  ([paste|
    __attribute__((tc_api)) void block();
    __attribute__((tc_thread)) void start() {
      block();
    }
  |], [paste|
    void ec_thread_0(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
      block(&ec_tstack_start.ec_frames.block);
      return;
      ec_contlbl_L2_start: ;
      return;
    }
  |], [])
  , -- 02 - blocking function with parameter and return value {{{2
  ([lpaste| 
    __attribute__((tc_api)) int block(int i);
02: __attribute__((tc_thread)) void start() {
      int j = block(23);
04: }
  |], [paste|
    typedef struct {
      int j;
    } ec_eframe_start_t;

    void ec_thread_0(void* ec_cont) {
      union {
          ec_eframe_start_t start;
      } ec_estack;

      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.ec_frames.block.i = 23;
      ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
      block(&ec_tstack_start.ec_frames.block);
      return;
      ec_contlbl_L2_start: ;
      ec_estack.start.j = ec_tstack_start.ec_frames.block.ec_result;
      return;
    }
  |], [
      ("j", 2, 4, Just 0, "ec_estack.start.j")
  ])
  , -- 03 - critical function {{{2
  ([lpaste| 
    __attribute__((tc_api)) void block(int i);
02: int crit(int k) { block(k); return k;}
    __attribute__((tc_thread)) void start() {
      crit(23);
    }
  |], [paste|
    void ec_thread_0(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.ec_frames.crit.k = 23;
      ec_tstack_start.ec_frames.crit.ec_cont = &&ec_contlbl_L2_start;
      goto ec_contlbl_L1_crit;

      ec_contlbl_L2_start: ;
      return;

      ec_contlbl_L1_crit: ;
      ec_tstack_start.ec_frames.crit.ec_frames.block.i = ec_tstack_start.ec_frames.crit.k;
      ec_tstack_start.ec_frames.crit.ec_frames.block.ec_cont = &&ec_contlbl_L2_crit;
      block(&ec_tstack_start.ec_frames.crit.ec_frames.block);
      return;

      ec_contlbl_L2_crit: ;
      ec_tstack_start.ec_frames.crit.ec_result = ec_tstack_start.ec_frames.crit.k;
      goto *ec_tstack_start.ec_frames.crit.ec_cont;
    }
  |], [
      ("k", 2, 2, Just 0, "ec_tstack_start.ec_frames.crit.k")
  ])
  , -- 04 - two threads {{{2
  ([paste|
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() {
      block(23);
    }
    __attribute__((tc_thread)) void run() {
      block(42);
    }
  |], [paste|
    void ec_thread_0(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.ec_frames.block.i = 23;
      ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
      block(&ec_tstack_start.ec_frames.block);
      return;

      ec_contlbl_L2_start: ;
      return;
    } 

    void ec_thread_1(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_run: ;
      ec_tstack_run.ec_frames.block.i = 42;
      ec_tstack_run.ec_frames.block.ec_cont = &&ec_contlbl_L2_run;
      block(&ec_tstack_run.ec_frames.block);
      return;

      ec_contlbl_L2_run: ;
      return;
    } 
  |], [])
  , -- 05 - reentrance {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
02: int crit(int k) { block(k); return k; }
    __attribute__((tc_thread)) void start() {
      crit(23);
    }
    __attribute__((tc_thread)) void run() {
      crit(42);
    }
  |], [paste|
    void ec_thread_0(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.ec_frames.crit.k = 23;
      ec_tstack_start.ec_frames.crit.ec_cont = &&ec_contlbl_L2_start;
      goto ec_contlbl_L1_crit;

      ec_contlbl_L2_start: ;
      return;

      ec_contlbl_L1_crit: ;
      ec_tstack_start.ec_frames.crit.ec_frames.block.i = ec_tstack_start.ec_frames.crit.k;
      ec_tstack_start.ec_frames.crit.ec_frames.block.ec_cont = &&ec_contlbl_L2_crit;
      block(&ec_tstack_start.ec_frames.crit.ec_frames.block);
      return;

      ec_contlbl_L2_crit: ;
      ec_tstack_start.ec_frames.crit.ec_result = ec_tstack_start.ec_frames.crit.k;
      goto *ec_tstack_start.ec_frames.crit.ec_cont;
    }

    void ec_thread_1(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_run: ;
      ec_tstack_run.ec_frames.crit.k = 42;
      ec_tstack_run.ec_frames.crit.ec_cont = &&ec_contlbl_L2_run;
      goto ec_contlbl_L1_crit;

      ec_contlbl_L2_run: ;
      return;

      ec_contlbl_L1_crit: ;
      ec_tstack_run.ec_frames.crit.ec_frames.block.i = ec_tstack_run.ec_frames.crit.k;
      ec_tstack_run.ec_frames.crit.ec_frames.block.ec_cont = &&ec_contlbl_L2_crit;
      block(&ec_tstack_run.ec_frames.crit.ec_frames.block);
      return;

      ec_contlbl_L2_crit: ;
      ec_tstack_run.ec_frames.crit.ec_result = ec_tstack_run.ec_frames.crit.k;
      goto *ec_tstack_run.ec_frames.crit.ec_cont;
    }
  |], [
      ("k", 2, 2, Just 0, "ec_tstack_start.ec_frames.crit.k")
    , ("k", 2, 2, Just 1, "ec_tstack_run.ec_frames.crit.k")
  ])
  , -- 06 - static variable {{{2
  ([paste|
    __attribute__((tc_api)) void block(int b);

    __attribute__((tc_thread)) void start() { 
        static int s = 23;
        while (1) {
          block(s);
        }
    }
  |], [paste|
    void ec_thread_0(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_ctrlbl_0_start: ;
      if (!1) {
        return;
      } else {
        ec_tstack_start.ec_frames.block.b = ec_static_start_s;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_ctrlbl_0_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      }
    }
  |], [])
  , -- 07 - regression test {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int b);
02: __attribute__((tc_thread)) void start() { 
      int s = 23;
      while (1) {
        block(s);
      }
07: }
  |], [paste|
    void ec_thread_0(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.s = 23;
      goto ec_ctrlbl_0_start;

      ec_ctrlbl_0_start: ;
      if (!1) {
        return;
      } else {
        ec_tstack_start.ec_frames.block.b = ec_tstack_start.s;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_ctrlbl_0_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      }
    }
  |], [
      ("s", 2, 7, Just 0, "ec_tstack_start.s")
  ])
  -- end {{{2
  ]
  where
    runTest :: (String, String, [TVarMapEntry]) -> Assertion -- {{{2
    runTest (inputCode, expectedCode, expectedVarMap) =
      let
        ast                  = enrich inputCode :: CTranslUnit
        ana                  = case analysis ast of
          Left es           -> error $ show_errors "test" es 
          Right x           -> x
        ir                   = ast_2_ir (anaBlocking ana) (anaCritical ana)
        (frames, stacks)     = create_estacks (anaCallgraph ana) ir
        (funs, outputVarMap) = thread_execution_functions (anaCallgraph ana) (anaBlocking ana) ir stacks
        outputCode           = reduce (CTranslUnit ((map CDeclExt frames) ++ (map (CFDefExt . fmap (const undefNode)) funs)) undefNode) :: String
        expectedCode'        = (reduce (enrich expectedCode :: CTranslUnit) :: String)
      in do
        expectedCode'  @=? outputCode
        expectedVarMap @=? reduce outputVarMap
  
test_tcode_2_ecode :: Test -- {{{1
test_tcode_2_ecode = enumTestGroup "tcode_2_ecode" $ map runTest [
  -- , 01 - setup {{{2
  ([paste|
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() { 
      block(23);
    }
  |],[paste|
    typedef struct {
        void* ec_cont;
        int i;
    } ec_tframe_block_t;

    typedef struct {
        union {
            ec_tframe_block_t block;
        } ec_frames;
    } ec_tframe_start_t;
    
    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      if (ec_cont)
        goto *ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.block.i = 23;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      ec_contlbl_L2_start: ;
        return; 
    }
  |], [])
  , -- 02 - setup - returning a pointer {{{2
  ([paste|
    __attribute__((tc_api)) int* block(int i);
    __attribute__((tc_thread)) void start() { 
      block(23);
    }
  |],[paste|
    typedef struct {
        void* ec_cont;
        int* ec_result;
        int i;
    } ec_tframe_block_t;

    typedef struct {
        union {
            ec_tframe_block_t block;
        } ec_frames;
    } ec_tframe_start_t;
    
    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      if (ec_cont)
        goto *ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.block.i = 23;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      ec_contlbl_L2_start: ;
        return; 
    }
  |], [])
  , -- 03 - setup - returning a void pointer {{{2
  ([paste|
    __attribute__((tc_api)) void* block(int i);
    __attribute__((tc_thread)) void start() { 
      block(23);
    }
  |],[paste|
    typedef struct {
        void* ec_cont;
        void* ec_result;
        int i;
    } ec_tframe_block_t;

    typedef struct {
        union {
            ec_tframe_block_t block;
        } ec_frames;
    } ec_tframe_start_t;
    
    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      if (ec_cont)
        goto *ec_cont;
    
      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.block.i = 23;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      ec_contlbl_L2_start: ;
        return; 
    }
  |], [])
  , -- 04 - local critical variable {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
02: __attribute__((tc_thread)) void start() {
      int i = 23;
      block(i);
      i++;
06: }
  |],[paste|
    typedef struct {
      void* ec_cont;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      union {
          ec_tframe_block_t block;
      } ec_frames;
      int i;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      if (ec_cont)
        goto *ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.i = 23;
        ec_tstack_start.ec_frames.block.i = ec_tstack_start.i;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      ec_contlbl_L2_start: ;
        ec_tstack_start.i++;
        return; 
    }
  |], [
      ("i", 2, 6, Just 0, "ec_tstack_start.i")
  ])
  , -- 05 - local non-critical variable {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
02: __attribute__((tc_thread)) void start() {
      int i = 23;
      block(i);
05: }
  |],[paste|
    typedef struct {
      void* ec_cont;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      union {
          ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    typedef struct {
      int i;
    } ec_eframe_start_t;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      union {
        ec_eframe_start_t start;
      } ec_estack;
      if (ec_cont)
        goto *ec_cont;

      ec_contlbl_L1_start: ;
        ec_estack.start.i = 23;
        ec_tstack_start.ec_frames.block.i = ec_estack.start.i;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      ec_contlbl_L2_start: ;
        return; 
    }
  |], [
      ("i", 2, 5, Just 0, "ec_estack.start.i")
  ])
  , -- 06 - function static variable {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
02: __attribute__((tc_thread)) void start() {
      static int i = 0;
      block(i);
05: }
  |],[paste|
    typedef struct {
      void* ec_cont;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      union {
          ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t*);

    int ec_static_start_i = 0;

    void ec_thread_0(void* ec_cont)
    {
      if (ec_cont)
        goto *ec_cont;
    
      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.block.i = ec_static_start_i;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      ec_contlbl_L2_start: ;
        return; 
    }
  |], [
      ("i", 2, 5, Nothing, "ec_static_start_i")
  ])
  , -- 07 - global variable {{{2
  ([lpaste|
    int k;
    __attribute__((tc_api)) void block(int i1, int i2);
03: __attribute__((tc_thread)) void start() {
      int j = 23;
      block(j, k);
06: }
  |],[paste|
    int k;

    typedef struct {
      void* ec_cont;
      int i1;
      int i2;
    } ec_tframe_block_t;

    typedef struct {
      union {
          ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    typedef struct {
      int j;
    } ec_eframe_start_t;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      union {
        ec_eframe_start_t start;
      } ec_estack;
      if (ec_cont)
        goto *ec_cont;

      ec_contlbl_L1_start: ;
        ec_estack.start.j = 23;
        ec_tstack_start.ec_frames.block.i1 = ec_estack.start.j;
        ec_tstack_start.ec_frames.block.i2 = k;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      ec_contlbl_L2_start: ;
        return; 
    }
  |], [
      ("j", 3, 6, Just 0, "ec_estack.start.j")
  ])
  , -- 08 - loop {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int j);
02: __attribute__((tc_thread)) void start() { 
      int i;
      i = 0;
      while (i<10) {
        i++;
        block(i);
        i++;
      }
      i = 0;
11: }
  |],[paste|
    typedef struct {
        void* ec_cont;
        int j;
    } ec_tframe_block_t;

    typedef struct {
        union {
            ec_tframe_block_t block;
        } ec_frames;
        int i;
    } ec_tframe_start_t;
    
    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      if (ec_cont)
        goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.i = 0;
      goto ec_ctrlbl_0_start; 
      ec_ctrlbl_0_start: ;
      if (!(ec_tstack_start.i < 10)) {
        ec_tstack_start.i = 0;
        return; 
      } else {
        ec_tstack_start.i++;
        ec_tstack_start.ec_frames.block.j = ec_tstack_start.i;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L4_start;
        block(&ec_tstack_start.ec_frames.block);
        return;
      }
      ec_contlbl_L4_start: ;
      ec_tstack_start.i++;
      goto ec_ctrlbl_0_start;
    }
  |], [
      ("i", 2, 11, Just 0, "ec_tstack_start.i")
  ])
  , -- 09 - critical function {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int b);

03: void critical(int c) {
      block(c+1); 
05: }

    int non_critical(int n) {
      return n+1;
    }

11: __attribute__((tc_thread)) void start() { 
        int s1;
        int s2;
        s2 = non_critical(s1);
        critical(s2);
16: }
  |],[paste|
    int non_critical(int n) {
      return n+1;
    }

    typedef struct {
        void* ec_cont;
        int b;
    } ec_tframe_block_t;

    typedef struct {
      void* ec_cont;
      union {
        ec_tframe_block_t block;
      } ec_frames;
      int c;  
    } ec_tframe_critical_t;

    typedef struct {
        union {
            ec_tframe_critical_t critical;
        } ec_frames;
    } ec_tframe_start_t;
    
    ec_tframe_start_t ec_tstack_start;

    typedef struct {
      int s2;
      int s1;
    } ec_eframe_start_t;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      union {
        ec_eframe_start_t start;
      } ec_estack;

      if (ec_cont)
        goto *ec_cont;

      ec_contlbl_L1_start: ;
        ec_estack.start.s2 = non_critical(ec_estack.start.s1);
        ec_tstack_start.ec_frames.critical.c = ec_estack.start.s2;
        ec_tstack_start.ec_frames.critical.ec_cont = &&ec_contlbl_L2_start;
        goto ec_contlbl_L1_critical;
      ec_contlbl_L2_start: ;
        return; 
      
      ec_contlbl_L1_critical: ;
        ec_tstack_start.ec_frames.critical.ec_frames.block.b = ec_tstack_start.ec_frames.critical.c + 1;
        ec_tstack_start.ec_frames.critical.ec_frames.block.ec_cont = &&ec_contlbl_L2_critical;
        block(&ec_tstack_start.ec_frames.critical.ec_frames.block);
        return;
      ec_contlbl_L2_critical: ;
        goto *ec_tstack_start.ec_frames.critical.ec_cont;
    }
  |], [
      ("s2", 11, 16, Just 0, "ec_estack.start.s2")
    , ("s1", 11, 16, Just 0, "ec_estack.start.s1")
    , ("c" ,  3,  5, Just 0, "ec_tstack_start.ec_frames.critical.c")
  ])
  , -- 10  - critical function, chained return {{{2
  ([lpaste|
    __attribute__((tc_api)) int block(int b);

03: int critical(int c) {
      return block(c+1);  
05: }

07: __attribute__((tc_thread)) void start() { 
        critical(23);
09: }
  |],[paste|
    typedef struct {
        void* ec_cont;
        int ec_result;
        int b;
    } ec_tframe_block_t;

    typedef struct {
      void* ec_cont;
      int ec_result;
      union {
        ec_tframe_block_t block;
      } ec_frames;
      int c;  
    } ec_tframe_critical_t;

    typedef struct {
        union {
            ec_tframe_critical_t critical;
        } ec_frames;
    } ec_tframe_start_t;
    
    ec_tframe_start_t ec_tstack_start;

    typedef struct {
      int ec_crit_0;
    } ec_eframe_critical_t;

    void block(ec_tframe_block_t*);

    void ec_thread_0(void* ec_cont)
    {
      union {
        ec_eframe_critical_t critical;
      } ec_estack;

      if (ec_cont)
        goto *ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.critical.c = 23;
        ec_tstack_start.ec_frames.critical.ec_cont = &&ec_contlbl_L2_start;
        goto ec_contlbl_L1_critical;
      ec_contlbl_L2_start: ;
        return; 
      
      ec_contlbl_L1_critical: ;
        ec_tstack_start.ec_frames.critical.ec_frames.block.b = ec_tstack_start.ec_frames.critical.c + 1;
        ec_tstack_start.ec_frames.critical.ec_frames.block.ec_cont = &&ec_contlbl_L2_critical;
        block(&ec_tstack_start.ec_frames.critical.ec_frames.block);
        return;

      ec_contlbl_L2_critical: ;
        ec_estack.critical.ec_crit_0 = ec_tstack_start.ec_frames.critical.ec_frames.block.ec_result;
        ec_tstack_start.ec_frames.critical.ec_result = ec_estack.critical.ec_crit_0;
        goto *ec_tstack_start.ec_frames.critical.ec_cont;
    }
  |], [
      ("c", 3, 5, Just 0, "ec_tstack_start.ec_frames.critical.c")
  ])
  , -- 11 - two threads {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int b);
02: __attribute__((tc_thread)) void start() { 
        int s = 23;
        while (1) {
          block(s);
        }
07: }

09: __attribute__((tc_thread)) void run() { 
        int r = 42;
        while (1) {
          block(r);
        }
14: }
  |],[paste|
    typedef struct {
      void * ec_cont;
      int b;
    } ec_tframe_block_t;

    typedef struct {
      union {
          ec_tframe_block_t block;
      } ec_frames;
      int s;
    } ec_tframe_start_t;

    typedef struct {
      union {
          ec_tframe_block_t block;
      } ec_frames;
      int r;
    } ec_tframe_run_t;

    ec_tframe_start_t ec_tstack_start;
    ec_tframe_run_t ec_tstack_run;

    void block(ec_tframe_block_t *);

    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont) goto * ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.s = 23;
        goto ec_ctrlbl_0_start;

      ec_ctrlbl_0_start: ;
        if (!1) {
          return;
        } else {
          ec_tstack_start.ec_frames.block.b = ec_tstack_start.s;
          ec_tstack_start.ec_frames.block.ec_cont = &&ec_ctrlbl_0_start;
          block(&ec_tstack_start.ec_frames.block);
          return;
        }
    }

    void ec_thread_1(void * ec_cont)
    {
        if (ec_cont) goto * ec_cont;

      ec_contlbl_L1_run: ;
        ec_tstack_run.r = 42;
        goto ec_ctrlbl_0_run;

      ec_ctrlbl_0_run: ;
        if (!1) {
          return;
        } else {
          ec_tstack_run.ec_frames.block.b = ec_tstack_run.r;
          ec_tstack_run.ec_frames.block.ec_cont = &&ec_ctrlbl_0_run;
          block(&ec_tstack_run.ec_frames.block);
          return;
        }
    }
  |], [
      ("s", 2,  7, Just 0, "ec_tstack_start.s")
    , ("r", 9, 14, Just 1, "ec_tstack_run.r")
  ])
  , -- 12 - reentrance {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int b);

03: void critical(int c) {
      block(c+1); 
05: }

07: __attribute__((tc_thread)) void run() { 
        critical(1);
09: }

11: __attribute__((tc_thread)) void start() { 
        critical(2);
13: }
  |],[paste|
    typedef struct {
      void * ec_cont;
      int b;
    } ec_tframe_block_t;

    typedef struct {
      void * ec_cont;
      union {
          ec_tframe_block_t block;
      } ec_frames;
      int c;
    } ec_tframe_critical_t;

    typedef struct {
      union {
          ec_tframe_critical_t critical;
      } ec_frames;
    } ec_tframe_run_t;

    typedef struct {
      union {
          ec_tframe_critical_t critical;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_run_t ec_tstack_run;
    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t *);

    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont) goto * ec_cont;

      ec_contlbl_L1_run: ;
        ec_tstack_run.ec_frames.critical.c = 1;
        ec_tstack_run.ec_frames.critical.ec_cont = &&ec_contlbl_L2_run;
        goto ec_contlbl_L1_critical;

      ec_contlbl_L2_run: ;
        return;

      ec_contlbl_L1_critical: ;
        ec_tstack_run.ec_frames.critical.ec_frames.block.b = ec_tstack_run.ec_frames.critical.c + 1;
        ec_tstack_run.ec_frames.critical.ec_frames.block.ec_cont = &&ec_contlbl_L2_critical;
        block(&ec_tstack_run.ec_frames.critical.ec_frames.block);
        return;

      ec_contlbl_L2_critical: ;
        goto * (ec_tstack_run.ec_frames.critical.ec_cont);
    }

    void ec_thread_1(void * ec_cont)
    {
        if (ec_cont) goto * ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.critical.c = 2;
        ec_tstack_start.ec_frames.critical.ec_cont = &&ec_contlbl_L2_start;
        goto ec_contlbl_L1_critical;

      ec_contlbl_L2_start: ;
        return;

      ec_contlbl_L1_critical: ;
        ec_tstack_start.ec_frames.critical.ec_frames.block.b = ec_tstack_start.ec_frames.critical.c + 1;
        ec_tstack_start.ec_frames.critical.ec_frames.block.ec_cont = &&ec_contlbl_L2_critical;
        block(&ec_tstack_start.ec_frames.critical.ec_frames.block);
        return;

      ec_contlbl_L2_critical: ;
        goto * (ec_tstack_start.ec_frames.critical.ec_cont);
    }
  |], [
      ("c", 3, 5, Just 0, "ec_tstack_run.ec_frames.critical.c")
    , ("c", 3, 5, Just 1, "ec_tstack_start.ec_frames.critical.c")
  ])
  , -- 13 - return value {{{2
  ([lpaste|
    __attribute__((tc_api)) int block(int i);
    __attribute__((tc_thread)) void start() 
03: {
      int i;
      i = block(i);
06: }
  |],[paste|
    typedef struct {
      void * ec_cont;
      int ec_result;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      union {
          ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    typedef struct {
        int i;
    } ec_eframe_start_t;

    void block(ec_tframe_block_t *);

    void ec_thread_0(void * ec_cont)
    {
        union {
            ec_eframe_start_t start;
        } ec_estack;

        if (ec_cont) goto * ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.block.i = ec_estack.start.i;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;

      ec_contlbl_L2_start: ;
        ec_estack.start.i = ec_tstack_start.ec_frames.block.ec_result;
        return;
    }
  |], [
      ("i", 3, 6, Just 0, "ec_estack.start.i")
  ])
  , -- 14 - multiple declarations with critical initialization {{{2
  ([lpaste|
    __attribute__((tc_api)) int block(int i);

03: __attribute__((tc_thread)) void start() {
      int i, j=block(1) + 3, k=23;
05: }
  |],[paste|
    typedef struct {
      void * ec_cont;
      int ec_result;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      union {
        ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    typedef struct {
      int i;
      int j;
      int k;
      int ec_crit_0;
    } ec_eframe_start_t;

    void block(ec_tframe_block_t *);

    void ec_thread_0(void * ec_cont)
    {
      union {
          ec_eframe_start_t start;
      } ec_estack;

      if (ec_cont) goto * ec_cont;

    ec_contlbl_L1_start: ;
      ec_tstack_start.ec_frames.block.i = 1;
      ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
      block(&ec_tstack_start.ec_frames.block);
      return;

    ec_contlbl_L2_start: ;
      ec_estack.start.ec_crit_0 = ec_tstack_start.ec_frames.block.ec_result;
      ec_estack.start.j = ec_estack.start.ec_crit_0 + 3;
      ec_estack.start.k = 23;
      return;
    }
  |], [
      ("i", 3, 5, Just 0, "ec_estack.start.i")
    , ("j", 3, 5, Just 0, "ec_estack.start.j")
    , ("k", 3, 5, Just 0, "ec_estack.start.k")
  ])
  , -- 15 - returns {{{2
  ([lpaste|
    __attribute__((tc_api)) int block(char* c);

03: int critical(int i) {
      if (i == 0) {
        return 0;
      } else {
        block(0);
        return 42;
      }
10: }

12: __attribute__((tc_thread)) void start() {
      critical(23);
14: }
  |],[paste|
    typedef struct {
      void * ec_cont;
      int ec_result;
      char * c;
    } ec_tframe_block_t;

    typedef struct {
      void * ec_cont;
      int ec_result;
      union {
        ec_tframe_block_t block;
      } ec_frames;
      int i;
    } ec_tframe_critical_t;

    typedef struct {
      union {
          ec_tframe_critical_t critical;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t *);

    void ec_thread_0(void * ec_cont)
    {
      if (ec_cont) goto * ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.critical.i = 23;
        ec_tstack_start.ec_frames.critical.ec_cont = &&ec_contlbl_L2_start;
        goto ec_contlbl_L1_critical;

      ec_contlbl_L2_start: ;
        return;

      ec_contlbl_L1_critical: ;
        if (ec_tstack_start.ec_frames.critical.i == 0) {
          ec_tstack_start.ec_frames.critical.ec_result = 0;
          goto * (ec_tstack_start.ec_frames.critical.ec_cont);
        } else {
          ec_tstack_start.ec_frames.critical.ec_frames.block.c = 0;
          ec_tstack_start.ec_frames.critical.ec_frames.block.ec_cont = &&ec_contlbl_L5_critical;
          block(&ec_tstack_start.ec_frames.critical.ec_frames.block);
          return;
        } 
        ec_contlbl_L5_critical: ;
        ec_tstack_start.ec_frames.critical.ec_result = 42;
        goto * (ec_tstack_start.ec_frames.critical.ec_cont);
    }
  |], [
      ("i", 3, 10, Just 0, "ec_tstack_start.ec_frames.critical.i")
  ])
  , -- 16 - struct {{{2
  ([lpaste|
    struct S {
      int i;
    };
    __attribute__((tc_api)) int block(struct S s);

06: __attribute__((tc_thread)) void start() {
      struct S s;
      block(s);
09: }
  |],[paste|
    struct S {
      int i;
    };

    typedef struct {
      void * ec_cont;
      int ec_result;
      struct S s;
    } ec_tframe_block_t;

    typedef struct {
      union {
          ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    typedef struct {
      struct S s;
    } ec_eframe_start_t;

    void block(ec_tframe_block_t *);

    void ec_thread_0(void * ec_cont)
    {
      union {
          ec_eframe_start_t start;
      } ec_estack;

      if (ec_cont) goto * ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.block.s = ec_estack.start.s;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;

      ec_contlbl_L2_start: ;
        return;
    }
  |], [
      ("s", 6, 9, Just 0, "ec_estack.start.s")
  ])
  , -- 17 - multiple global declarations {{{2
  ([paste|
    int i, k;
    __attribute__((tc_api)) int block(int i);

    __attribute__((tc_thread)) void start() 
    {
            i = block(k);
    }
  |],[paste|
    int i, k;
    typedef struct {
      void * ec_cont;
      int ec_result;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      union {
        ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t *);

    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont) goto * ec_cont;

    ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.block.i = k;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;

    ec_contlbl_L2_start: ;
        i = ec_tstack_start.ec_frames.block.ec_result;
        return;
    }
  |], [])
  , -- 18 - static variable {{{2
  ([lpaste|
    __attribute__((tc_api)) int block(int i);

    __attribute__((tc_thread)) void start() 
04: {
      static int i = 23;
      i = block(i);
07: }
  |],[paste|
    typedef struct {
      void * ec_cont;
      int ec_result;
      int i;
    } ec_tframe_block_t;

    typedef struct {
      union {
        ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    void block(ec_tframe_block_t *);

    int ec_static_start_i = 23;

    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont) goto * ec_cont;

    ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.block.i = ec_static_start_i;
        ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
        block(&ec_tstack_start.ec_frames.block);
        return;

    ec_contlbl_L2_start: ;
        ec_static_start_i = ec_tstack_start.ec_frames.block.ec_result;
        return;
    }
  |], [
      ("i", 4, 7, Nothing, "ec_static_start_i")
  ])
  , -- 19 - regression test - else if {{{2
  ([paste|
    int c;
    int next;

    __attribute__((tc_api)) void wait(int* c);
    __attribute__((tc_api)) void sleep(int t, int* c);
    void check();

    __attribute__((tc_thread)) void start() {
      while(1) {
        if (next == -1) {
          wait(&c);
        } else if (! sleep(next, &c)) {
          check();
        }
      }
    }
  |], [paste|
    int c;
    int next;

    void check();

    typedef struct {
      void * ec_cont;
      int t;
      int * c;
    } ec_tframe_sleep_t;

    typedef struct {
      void * ec_cont;
      int * c;
    } ec_tframe_wait_t;

    typedef struct {
      union {
          ec_tframe_wait_t wait; ec_tframe_sleep_t sleep;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    typedef struct {
      void ec_crit_0;
    } ec_eframe_start_t;

    void sleep(ec_tframe_sleep_t *);
    void wait(ec_tframe_wait_t *);

    void ec_thread_0(void * ec_cont)
    {
        union {
            ec_eframe_start_t start;
        } ec_estack;

        if (ec_cont) goto * ec_cont;

        ec_ctrlbl_0_start: ;
        if (!1) {
          return;
        } else {
          if (next == -1) {
            ec_tstack_start.ec_frames.wait.c = &c;
            ec_tstack_start.ec_frames.wait.ec_cont = &&ec_ctrlbl_0_start;
            wait(&ec_tstack_start.ec_frames.wait);
            return;
          } else {
            ec_tstack_start.ec_frames.sleep.t = next;
            ec_tstack_start.ec_frames.sleep.c = &c;
            ec_tstack_start.ec_frames.sleep.ec_cont = &&ec_contlbl_L6_start;
            sleep(&ec_tstack_start.ec_frames.sleep);
            return;
          }
        }

        ec_contlbl_L6_start: ;
        ec_estack.start.ec_crit_0 = ec_tstack_start.ec_frames.sleep.ec_result;
        if (!ec_estack.start.ec_crit_0) {
          check();
          goto ec_ctrlbl_0_start;
        } else {
          goto ec_ctrlbl_0_start;
        }
    }
  |], [])
  , -- 20 - regression test - estack access {{{2
  ([lpaste|
    __attribute__((tc_api)) void block();
02: void critical() {
      int i = 0;
      block();
05: }
    __attribute__((tc_thread)) void start() {
      critical();
    }
  |], [paste|
    typedef struct {
      void * ec_cont;
    } ec_tframe_block_t;

    typedef struct {
      void * ec_cont;
      union {
        ec_tframe_block_t block;
      } ec_frames;
    } ec_tframe_critical_t;

    typedef struct {
      union {
        ec_tframe_critical_t critical;
      } ec_frames;
    } ec_tframe_start_t;

    ec_tframe_start_t ec_tstack_start;

    typedef struct {
      int i;
    } ec_eframe_critical_t;

    void block(ec_tframe_block_t *);

    void ec_thread_0(void * ec_cont)
    {
        union {
          ec_eframe_critical_t critical;
        } ec_estack;

        if (ec_cont) goto * ec_cont;

      ec_contlbl_L1_start: ;
        ec_tstack_start.ec_frames.critical.ec_cont = &&ec_contlbl_L2_start;
        goto ec_contlbl_L1_critical;

      ec_contlbl_L2_start: ;
        return;

      ec_contlbl_L1_critical: ;
        ec_estack.critical.i = 0;
        ec_tstack_start.ec_frames.critical.ec_frames.block.ec_cont = &&ec_contlbl_L2_critical;
        block(&ec_tstack_start.ec_frames.critical.ec_frames.block);
        return;

      ec_contlbl_L2_critical: ;
        goto * (ec_tstack_start.ec_frames.critical.ec_cont);
    }
  |], [
      ("i", 2, 5, Just 0, "ec_estack.critical.i")
  ])
  -- end {{{2
  ]
  where
    runTest :: (String, String, [TVarMapEntry]) -> Assertion -- {{{2
    runTest (inputCode, expectedCode, expectedVarMap) =
      let
        tAst                    = enrich inputCode :: CTranslUnit
        ana                     = case analysis tAst of
          Left es              -> error $ show_errors "test" es 
          Right x              -> x
        ir                      = ast_2_ir (anaBlocking ana) (anaCritical ana)
        (eAst, _, outputVarMap) = tcode_2_ecode ana ir
        outputCode              = reduce $ fmap nodeInfo eAst
        expectedCode'           = (reduce (enrich expectedCode :: CTranslUnit) :: String)
      in do
        expectedCode'  @=? outputCode
        expectedVarMap @=? reduce outputVarMap
