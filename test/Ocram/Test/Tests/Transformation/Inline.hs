module Ocram.Test.Tests.Transformation.Inline 
-- export {{{1
(
	tests
) where
-- import {{{1
import Test.HUnit
import Ocram.Types
import Ocram.Test.Lib
import Ocram.Transformation.Inline (transformation)
import Ocram.Analysis (analysis)
import Ocram.Text (show_errors)

import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit))

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

tests = runTests [ -- {{{1
-- setup {{{2
	([paste|
		__attribute__((tc_blocking)) void block(int i);
		__attribute__((tc_run_thread)) void start() { 
			block(23);
		}
	|],[paste|
		typedef struct {
				void* ec_cont;
				int i;
		} ec_frame_block_t;

		typedef struct {
				union {
						ec_frame_block_t block;
				} ec_frames;
		} ec_frame_start_t;
		
		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_start.ec_frames.block.i = 23;
				ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
				block(&ec_stack_start.ec_frames.block);
				return;
			ec_label_start_1: ;
				return;	
		}
	|])
-- local variable {{{2
	, ([paste|
		__attribute__((tc_blocking)) void block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i;
			block(i);
		}
	|],[paste|
		typedef struct {
			void* ec_cont;
			int i;
		} ec_frame_block_t;

		typedef struct {
			union {
					ec_frame_block_t block;
			} ec_frames;
			int i;
		} ec_frame_start_t;

		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_start.ec_frames.block.i = ec_stack_start.i;
				ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
				block(&ec_stack_start.ec_frames.block);
				return;
			ec_label_start_1: ;
				return;	
		}
	|])
-- function static variable {{{2
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
		} ec_frame_block_t;

		typedef struct {
			union {
					ec_frame_block_t block;
			} ec_frames;
		} ec_frame_start_t;

		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

        static int i = 0;
				ec_stack_start.ec_frames.block.i = i;
				ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
				block(&ec_stack_start.ec_frames.block);
				return;
			ec_label_start_1: ;
				return;	
		}
	|])
-- global variable {{{2
	, ([paste|
		__attribute__((tc_blocking)) void block(int i1, int i2);

		int k;

		__attribute__((tc_run_thread)) void start() 
		{
			int j;
			block(j, k);
		}
	|],[paste|
		int k;

		typedef struct {
			void* ec_cont;
			int i1;
			int i2;
		} ec_frame_block_t;

		typedef struct {
			union {
					ec_frame_block_t block;
			} ec_frames;
			int j;
		} ec_frame_start_t;

		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_start.ec_frames.block.i1 = ec_stack_start.j;
				ec_stack_start.ec_frames.block.i2 = k;
				ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
				block(&ec_stack_start.ec_frames.block);
				return;
			ec_label_start_1: ;
				return;	
		}
	|])
-- loop {{{2
	,([paste|
		__attribute__((tc_blocking)) void block(int j);
		__attribute__((tc_run_thread)) void start() { 
			int i;
			i = 0;
			while (i<10) {
				i++;
				block(i);
				i++;
			}
			i = 0;
		}
	|],[paste|
		typedef struct {
				void* ec_cont;
				int j;
		} ec_frame_block_t;

		typedef struct {
				union {
						ec_frame_block_t block;
				} ec_frames;
				int i;
		} ec_frame_start_t;
		
		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_start.i = 0;
				while (ec_stack_start.i < 10) {
					ec_stack_start.i++;
					ec_stack_start.ec_frames.block.j = ec_stack_start.i;
					ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
					block(&ec_stack_start.ec_frames.block);
					return;
		ec_label_start_1: ;
					ec_stack_start.i++;
				}
				ec_stack_start.i = 0;
				return;	
		}
	|])
-- critical function {{{2
	,([paste|
		__attribute__((tc_blocking)) void block(int b);

		void critical(int c) {
			block(c+1);	
		}

		int non_critical(int n) {
			return n+1;
		}

		__attribute__((tc_run_thread)) void start() { 
				int s1;
				int s2;
				s2 = non_critical(s1);
				critical(s2);
		}
	|],[paste|
		int non_critical(int n) {
			return n+1;
		}

		typedef struct {
				void* ec_cont;
				int b;
		} ec_frame_block_t;

		typedef struct {
			void* ec_cont;
			union {
				ec_frame_block_t block;
			} ec_frames;
			int c;	
		} ec_frame_critical_t;

		typedef struct {
				union {
						ec_frame_critical_t critical;
				} ec_frames;
				int s1;
				int s2;
		} ec_frame_start_t;
		
		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_start.s2 = non_critical(ec_stack_start.s1);
				ec_stack_start.ec_frames.critical.c = ec_stack_start.s2;
				ec_stack_start.ec_frames.critical.ec_cont = &&ec_label_start_1;
				goto ec_label_critical_0;
			ec_label_start_1: ;
				return;	
			
			ec_label_critical_0: ;
				ec_stack_start.ec_frames.critical.ec_frames.block.b = ec_stack_start.ec_frames.critical.c + 1;
				ec_stack_start.ec_frames.critical.ec_frames.block.ec_cont = &&ec_label_critical_1;
				block(&ec_stack_start.ec_frames.critical.ec_frames.block);
				return;
			ec_label_critical_1: ;
				goto *ec_stack_start.ec_frames.critical.ec_cont;
		}
	|])
-- two threads {{{2
	,([paste|
		__attribute__((tc_blocking)) void block(int b);

		__attribute__((tc_run_thread)) void start() { 
				int s;
				while (1) {
					block(s);
				}
		}

		__attribute__((tc_run_thread)) void run() { 
				int r;
				while (1) {
					block(r);
				}
		}
	|],[paste|
		typedef struct {
				void* ec_cont;
				int b;
		} ec_frame_block_t;

		typedef struct {
				union {
						ec_frame_block_t block;
				} ec_frames;
				int r;
		} ec_frame_run_t;

		typedef struct {
				union {
						ec_frame_block_t block;
				} ec_frames;
				int s;
		} ec_frame_start_t;
		
		ec_frame_run_t ec_stack_run;
		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				while (1) {
				ec_stack_run.ec_frames.block.b = ec_stack_run.r;
				ec_stack_run.ec_frames.block.ec_cont = &&ec_label_run_1;
				block(&ec_stack_run.ec_frames.block);
				return;
			ec_label_run_1: ;
				}
				return;	
		}

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				while (1) {
				ec_stack_start.ec_frames.block.b = ec_stack_start.s;
				ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
				block(&ec_stack_start.ec_frames.block);
				return;
			ec_label_start_1: ;
				}
				return;	
		}
	|])
-- reentrance {{{2
	,([paste|
		__attribute__((tc_blocking)) void block(int b);

		void critical(int c) {
			block(c+1);	
		}

		__attribute__((tc_run_thread)) void run() { 
				critical(1);
		}

		__attribute__((tc_run_thread)) void start() { 
				critical(2);
		}
	|],[paste|
		typedef struct {
				void* ec_cont;
				int b;
		} ec_frame_block_t;

		typedef struct {
			void* ec_cont;
			union {
				ec_frame_block_t block;
			} ec_frames;
			int c;	
		} ec_frame_critical_t;

		typedef struct {
				union {
						ec_frame_critical_t critical;
				} ec_frames;
		} ec_frame_run_t;

		typedef struct {
				union {
						ec_frame_critical_t critical;
				} ec_frames;
		} ec_frame_start_t;
		
		ec_frame_run_t ec_stack_run;
		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_run.ec_frames.critical.c = 1;
				ec_stack_run.ec_frames.critical.ec_cont = &&ec_label_run_1;
				goto ec_label_critical_0;
			ec_label_run_1: ;
				return;	
			
			ec_label_critical_0: ;
				ec_stack_run.ec_frames.critical.ec_frames.block.b = ec_stack_run.ec_frames.critical.c + 1;
				ec_stack_run.ec_frames.critical.ec_frames.block.ec_cont = &&ec_label_critical_1;
				block(&ec_stack_run.ec_frames.critical.ec_frames.block);
				return;
			ec_label_critical_1: ;
				goto *ec_stack_run.ec_frames.critical.ec_cont;
		}

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_start.ec_frames.critical.c = 2;
				ec_stack_start.ec_frames.critical.ec_cont = &&ec_label_start_1;
			  goto ec_label_critical_0;
			ec_label_start_1: ;
				return;	
			
			ec_label_critical_0: ;
				ec_stack_start.ec_frames.critical.ec_frames.block.b = ec_stack_start.ec_frames.critical.c + 1;
				ec_stack_start.ec_frames.critical.ec_frames.block.ec_cont = &&ec_label_critical_1;
				block(&ec_stack_start.ec_frames.critical.ec_frames.block);
				return;
			ec_label_critical_1: ;
				goto *ec_stack_start.ec_frames.critical.ec_cont;
		}
	|])
-- return value {{{2
	, ([paste|
		__attribute__((tc_blocking)) int block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i;
			i = block(i);
		}
	|],[paste|
		typedef struct {
			void* ec_cont;
			int ec_result;
			int i;
		} ec_frame_block_t;

		typedef struct {
			union {
					ec_frame_block_t block;
			} ec_frames;
			int i;
		} ec_frame_start_t;

		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_start.ec_frames.block.i = ec_stack_start.i;
				ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
				block(&ec_stack_start.ec_frames.block);
				return;
			ec_label_start_1: ;
				ec_stack_start.i = ec_stack_start.ec_frames.block.ec_result;
				return;	
		}
	|])
-- multiple declarations {{{2
  , ([paste|
		__attribute__((tc_blocking)) int block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i, j;
			i = block(j);
		}
	|],[paste|
		typedef struct {
			void* ec_cont;
			int ec_result;
			int i;
		} ec_frame_block_t;

		typedef struct {
			union {
					ec_frame_block_t block;
			} ec_frames;
			int i;
      int j;
		} ec_frame_start_t;

		ec_frame_start_t ec_stack_start;

		void block(ec_frame_block_t*);

		void ec_thread_0(void* ec_cont)
		{
			if (ec_cont)
				goto *ec_cont;

				ec_stack_start.ec_frames.block.i = ec_stack_start.j;
				ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
				block(&ec_stack_start.ec_frames.block);
				return;
			ec_label_start_1: ;
				ec_stack_start.i = ec_stack_start.ec_frames.block.ec_result;
				return;	
		}
  |])
-- multiple declarations with initialization {{{2
  , ([paste|
    __attribute__((tc_blocking)) int block(int i);

    __attribute__((tc_run_thread)) void start() {
      int i, j=23;
      i = block(j);
    }
  |],[paste|
    typedef struct {
      void* ec_cont;
      int ec_result;
      int i;
    } ec_frame_block_t;

    typedef struct {
      union {
        ec_frame_block_t block;
      } ec_frames;
      int i;
      int j;
    } ec_frame_start_t;

    ec_frame_start_t ec_stack_start;

    void block(ec_frame_block_t*);

    void ec_thread_0(void* ec_cont) {
      if (ec_cont)
        goto *ec_cont;

        ec_stack_start.j = 23;
        ec_stack_start.ec_frames.block.i = ec_stack_start.j;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
        block(&ec_stack_start.ec_frames.block);
        return;
      ec_label_start_1: ;
        ec_stack_start.i = ec_stack_start.ec_frames.block.ec_result;
        return;
    }
    |])
-- multiple declarations with critical initialization {{{2
  , ([paste|
    __attribute__((tc_blocking)) int block(int i);

    __attribute__((tc_run_thread)) void start() {
      int i, j=block(1) + 3, k=23;
    }
  |],[paste|
    typedef struct {
      void* ec_cont;
      int ec_result;
      int i;
    } ec_frame_block_t;

    typedef struct {
      union {
        ec_frame_block_t block;
      } ec_frames;
      int ec_tmp_0;
      int i;
      int j;
      int k;
    } ec_frame_start_t;

    ec_frame_start_t ec_stack_start;

    void block(ec_frame_block_t*);

    void ec_thread_0(void* ec_cont) {
      if (ec_cont)
        goto *ec_cont;

        ec_stack_start.ec_frames.block.i = 1;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
        block(&ec_stack_start.ec_frames.block);
        return;
      ec_label_start_1: ;
        ec_stack_start.ec_tmp_0 = ec_stack_start.ec_frames.block.ec_result;
        ec_stack_start.j = ec_stack_start.ec_tmp_0 + 3;
        ec_stack_start.k = 23;
        return;
    }
  |])
-- cast operator {{{2
  , ([paste|
    __attribute__((tc_blocking)) int block(char* c);

    __attribute__((tc_run_thread)) void start() {
      int i=0;
      block((char*)&i); 
    }
  |],[paste|
    typedef struct {
      void* ec_cont;
      int ec_result;
      char* c;
    } ec_frame_block_t;

    typedef struct {
      union {
        ec_frame_block_t block;
      } ec_frames;
      int i;
    } ec_frame_start_t;

    ec_frame_start_t ec_stack_start;

    void block(ec_frame_block_t*);

    void ec_thread_0(void* ec_cont) {
      if (ec_cont)
        goto *ec_cont;

        ec_stack_start.i = 0;
        ec_stack_start.ec_frames.block.c = (char*)&ec_stack_start.i;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
        block(&ec_stack_start.ec_frames.block);
        return;
      ec_label_start_1: ;
        return;
    }
    |])
-- struct {{{2
  , ([paste|
    struct S {
      int i;
    };
    __attribute__((tc_blocking)) int block(struct S s);

    __attribute__((tc_run_thread)) void start() {
      struct S s;
      block(s);
    }
  |],[paste|
    struct S {
      int i;
    };
    typedef struct {
      void* ec_cont;
      int ec_result;
      struct S s;
    } ec_frame_block_t;

    typedef struct {
      union {
        ec_frame_block_t block;
      } ec_frames;
      struct S s;
    } ec_frame_start_t;

    ec_frame_start_t ec_stack_start;

    void block(ec_frame_block_t*);

    void ec_thread_0(void* ec_cont) {
      if (ec_cont)
        goto *ec_cont;

        ec_stack_start.ec_frames.block.s = ec_stack_start.s;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
        block(&ec_stack_start.ec_frames.block);
        return;
      ec_label_start_1: ;
        return;
    }
    |])
-- struct initialization -- TODO {{{2
--   , ([paste|
--     struct S {
--       int i;
--     };
--     __attribute__((tc_blocking)) int block(struct S s);

--     __attribute__((tc_run_thread)) void start() {
--       struct S s = {23};
--       block(s);
--     }
--   |],[paste|
--     struct S {
--       int i;
--     };
--     typedef struct {
--       void* ec_cont;
--       int ec_result;
--       struct S s;
--     } ec_frame_block_t;

--     typedef struct {
--       union {
--         ec_frame_block_t block;
--       } ec_frames;
--       struct S s;
--     } ec_frame_start_t;

--     ec_frame_start_t ec_stack_start;

--     void block(ec_frame_block_t*);

--     void ec_thread_0(void* ec_cont) {
--       if (ec_cont)
--         goto *ec_cont;

--         ec_stack_start.s.i = 23;
--         ec_stack_start.ec_frames.block.s = ec_stack_start.s;
--         ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
--         block(&ec_stack_start.ec_frames.block);
--         return;
--       ec_label_start_1: ;
--         return;
--     }
--     |])
	]

runTests :: [(String, String)] -> Test -- {{{2
runTests cases = TestLabel "Inline" $ TestList $ map runTest cases
  where
    runTest (code, expected) = TestCase $
      let ast = enrich code in
      case analysis ast of
        Left es -> assertFailure $ show_errors "analysis" es
        Right ana ->
          let
            result = reduce $ fst $ transformation ana ast
            expected' = (reduce $ (enrich expected :: Ast) :: String)
          in
            expected' @=? result

