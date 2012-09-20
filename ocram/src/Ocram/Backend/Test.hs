{-# LANGUAGE QuasiQuotes #-}
module Ocram.Backend.Test (tests) where

-- imports {{{1
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Language.C.Pretty (pretty)
import Ocram.Analysis (analysis, Analysis(..))
import Ocram.Backend.EStack
import Ocram.Backend.ThreadExecutionFunction
import Ocram.Backend.TStack
import Ocram.Backend
import Ocram.Intermediate (ast_2_ir)
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, paste)
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
  , -- 02 - blocking function with parameters {{{2
  ([paste| 
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
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
    __attribute__((tc_blocking)) void block(int i);
    int crit(int k) { block(k); return k;}
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
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
      block(23);
    }
    __attribute__((tc_run_thread)) void run() {
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
    __attribute__((tc_blocking)) void block(int i);
    int crit(int k) { block(k); return k; }
    __attribute__((tc_run_thread)) void start() {
      crit(23);
    }
    __attribute__((tc_run_thread)) void run() {
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
        outputDecls = reduce (CTranslUnit (map CDeclExt (frames ++ stacks)) undefNode) :: String
        expectedDecls' = (reduce (enrich expectedDecls :: CTranslUnit)) :: String
      in
        expectedDecls' @=? outputDecls

test_create_estacks :: Test -- {{{1
test_create_estacks = enumTestGroup "create_estacks" $ map runTest [
  -- , 01 -  minimal case {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    }
  |], "", [("start", Nothing)])
  , -- 02 - uncritical variable {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
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
    __attribute__((tc_blocking)) void block(int i);
    int crit() { int k = 42; block(k);}
    __attribute__((tc_run_thread)) void start() {
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
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
      int i = 23;
      block(i);
    }
    __attribute__((tc_run_thread)) void run() {
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
    __attribute__((tc_blocking)) void block(int i);
    int crit() { int k = 23; block(k); }
    __attribute__((tc_run_thread)) void start() {
      int i = 42;
      crit(i);
    }
    __attribute__((tc_run_thread)) void run() {
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
        outputStacks = M.toList . M.map (fmap (show . pretty)) $ stacks 
      in do
        assertEqual "frames" expectedFrames' outputFrames
        assertEqual "stacks" expectedStacks outputStacks
        

test_thread_execution_functions :: Test -- {{{1
test_thread_execution_functions = enumTestGroup "thread_execution_functions" $ map runTest [
  -- , 01 - minimal case {{{2
  ([paste|
    __attribute__((tc_blocking)) void block();
    __attribute__((tc_run_thread)) void start() {
      block();
    }
  |], [paste|
    void ec_thread_1(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
      block(&ec_tstack_start.ec_frames.block);
      return;
      ec_contlbl_L2_start: ;
      return;
    }
  |])
  , -- 02 - blocking function with parameter and return value {{{2
  ([paste| 
    __attribute__((tc_blocking)) int block(int i);
    __attribute__((tc_run_thread)) void start() {
      int j = block(23);
    }
  |], [paste|
    typedef struct {
      int j;
    } ec_eframe_start_t;

    void ec_thread_1(void* ec_cont) {
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
  |])
  , -- 03 - critical function {{{2
  ([paste| 
    __attribute__((tc_blocking)) void block(int i);
    int crit(int k) { block(k); return k;}
    __attribute__((tc_run_thread)) void start() {
      crit(23);
    }
  |], [paste|
    void ec_thread_1(void* ec_cont) {
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
  |])
  , -- 04 - two threads {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
      block(23);
    }
    __attribute__((tc_run_thread)) void run() {
      block(42);
    }
  |], [paste|
    void ec_thread_1(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_start: ;
      ec_tstack_start.ec_frames.block.i = 23;
      ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
      block(&ec_tstack_start.ec_frames.block);
      return;

      ec_contlbl_L2_start: ;
      return;
    } 

    void ec_thread_2(void* ec_cont) {
      if (ec_cont) goto *ec_cont;

      ec_contlbl_L1_run: ;
      ec_tstack_run.ec_frames.block.i = 42;
      ec_tstack_run.ec_frames.block.ec_cont = &&ec_contlbl_L2_run;
      block(&ec_tstack_run.ec_frames.block);
      return;

      ec_contlbl_L2_run: ;
      return;
    } 
  |])
  , -- 05 - reentrance {{{2
  ([paste|
    __attribute__((tc_blocking)) void block(int i);
    int crit(int k) { block(k); return k; }
    __attribute__((tc_run_thread)) void start() {
      crit(23);
    }
    __attribute__((tc_run_thread)) void run() {
      crit(42);
    }
  |], [paste|
    void ec_thread_1(void* ec_cont) {
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

    void ec_thread_2(void* ec_cont) {
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
  |])
  -- end {{{2
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{2
    runTest (inputCode, expectedCode) =
      let
        ast = enrich inputCode :: CTranslUnit
        ana = case analysis ast of
          Left es -> error $ show_errors "test" es 
          Right x -> x
        ir = ast_2_ir (anaBlocking ana) (anaCritical ana)
        (frames, stacks) = create_estacks (anaCallgraph ana) ir
        funs = thread_execution_functions (anaCallgraph ana) (anaBlocking ana) ir stacks
        outputCode = reduce (CTranslUnit ((map CDeclExt frames) ++ (map CFDefExt funs)) undefNode) :: String
        expectedCode' = (reduce (enrich expectedCode :: CTranslUnit) :: String)
      in
        expectedCode' @=? outputCode
  
test_tcode_2_ecode :: Test -- {{{1
test_tcode_2_ecode = enumTestGroup "tcode_2_ecode" $ map runTest [
  -- , 01 - setup {{{2
	([paste|
		__attribute__((tc_blocking)) void block(int i);
		__attribute__((tc_run_thread)) void start() { 
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

		void ec_thread_1(void* ec_cont)
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
	|])
  -- 02 - setup - returning a pointer {{{2
	, ([paste|
		__attribute__((tc_blocking)) int* block(int i);
		__attribute__((tc_run_thread)) void start() { 
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

		void ec_thread_1(void* ec_cont)
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
	|])
  -- 03 - setup - returning a void pointer {{{2
	, ([paste|
		__attribute__((tc_blocking)) void* block(int i);
		__attribute__((tc_run_thread)) void start() { 
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

		void ec_thread_1(void* ec_cont)
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
	|])
-- 04 - local critical variable {{{2
	, ([paste|
		__attribute__((tc_blocking)) void block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i = 23;
			block(i);
      i++;
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
			int i;
		} ec_tframe_start_t;

		ec_tframe_start_t ec_tstack_start;

		void block(ec_tframe_block_t*);

		void ec_thread_1(void* ec_cont)
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
	|])
-- 05 - local non-critical variable {{{2
	, ([paste|
		__attribute__((tc_blocking)) void block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i = 23;
			block(i);
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

    typedef struct {
      int i;
    } ec_eframe_start_t;

		void block(ec_tframe_block_t*);

		void ec_thread_1(void* ec_cont)
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
	|])
  -- 06 - function static variable {{{2
	, ([paste|
		__attribute__((tc_blocking)) void block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			static int i = 0;
			block(i);
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

    static int ec_static_start_i = 0;

		void ec_thread_1(void* ec_cont)
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
	|])
  -- end {{{2
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{2
    runTest (inputCode, expectedCode) =
      let
        ast = enrich inputCode :: CTranslUnit
        ana = case analysis ast of
          Left es -> error $ show_errors "test" es 
          Right x -> x
        ir = ast_2_ir (anaBlocking ana) (anaCritical ana)
        outputCode = reduce $ tcode_2_ecode ana ir
        expectedCode' = (reduce (enrich expectedCode :: CTranslUnit) :: String)
      in
        expectedCode' @=? outputCode
