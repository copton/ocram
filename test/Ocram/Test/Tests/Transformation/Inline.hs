module Ocram.Test.Tests.Transformation.Inline (
	tests
) where

-- imports {{{1
import Ocram.Types
import Ocram.Test.Lib (parse, paste)
import Ocram.Compiler (analysis')
import Ocram.Transformation.Inline (transformation)
import Ocram.Options (defaultOptions)

import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit))

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

-- tests {{{1
tests = runTests "Inline" [
-- setup {{{2
	([$paste|
		__attribute__((tc_blocking)) void block(int i);
		__attribute__((tc_run_thread)) void start() { 
			block(23);
		}
	|],[$paste|
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

		void block(ec_frame_block_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_start_0: ;
				ec_stack_start->ec_frames.block.i = 23;
				ec_stack_start->ec_frames.block.ec_cont = &ec_label_start_1;
				block(&ec_stack_start->ec_frames.block);
				return;
			ec_label_start_1: ;
				return;	
		}
	|])
-- local variable {{{2
	, ([$paste|
		__attribute__((tc_blocking)) void block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i;
			block(i);
		}
	|],[$paste|
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

		void block(ec_frame_block_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_start_0: ;
				ec_stack_start->ec_frames.block.i = ec_stack_start->i;
				ec_stack_start->ec_frames.block.ec_cont = &ec_label_start_1;
				block(&ec_stack_start->ec_frames.block);
				return;
			ec_label_start_1: ;
				return;	
		}
	|])
-- global variable {{{2
	, ([$paste|
		__attribute__((tc_blocking)) void block(int i1, int i2);

		int k;

		__attribute__((tc_run_thread)) void start() 
		{
			int j;
			block(j, k);
		}
	|],[$paste|
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

		void block(ec_frame_block_t* frame);

		int k;

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_start_0: ;
				ec_stack_start->ec_frames.block.i1 = ec_stack_start->j;
				ec_stack_start->ec_frames.block.i2 = k;
				ec_stack_start->ec_frames.block.ec_cont = &ec_label_start_1;
				block(&ec_stack_start->ec_frames.block);
				return;
			ec_label_start_1: ;
				return;	
		}
	|])
-- loop {{{2
	,([$paste|
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
	|],[$paste|
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

		void block(ec_frame_block_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

		ec_label_start_0: ;
				ec_stack_start->i = 0;
				while (ec_stack_start->i < 10) {
					ec_stack_start->i++;
					ec_stack_start->ec_frames.block.j = ec_stack_start->i;
					ec_stack_start->ec_frames.block.ec_cont = &ec_label_start_1;
					block(&ec_stack_start->ec_frames.block);
					return;
		ec_label_start_1: ;
					ec_stack_start->i++;
				}
				ec_stack_start->i = 0;
				return;	
		}
	|])
-- critical function {{{2
	,([$paste|
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
	|],[$paste|
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

		void block(ec_frame_block_t* frame);

		int non_critical(int n) {
			return n+1;
		}

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_start_0: ;
				ec_stack_start->s2 = non_critical(ec_stack_start->s1);
				ec_stack_start->ec_frames.critical.c = ec_stack_start->s2;
				ec_stack_start->ec_frames.critical.ec_cont = &ec_label_start_1;
				critical(&ec_stack_start->ec_frames.critical);
				return;
			ec_label_start_1: ;
				return;	
			
			ec_label_critical_0: ;
				ec_stack_start->ec_frames.critical.ec_frames.block.b = ec_stack_start->ec_frames.critical.c + 1;
				ec_stack_start->ec_frames.critical.ec_frames.block.ec_cont = &ec_label_critical_1;
				block(&ec_stack_start->ec_frames.critical.ec_frames.block);
				return;
			ec_label_critical_1: ;
				return;
		}
	|])
-- two threads {{{2
	,([$paste|
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
	|],[$paste|
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

		void block(ec_frame_block_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_run_0: ;
				while (1) {
				ec_stack_run->ec_frames.block.b = ec_stack_run->r;
				ec_stack_run->ec_frames.block.ec_cont = &ec_label_run_1;
				block(&ec_stack_run->ec_frames.block);
				return;
			ec_label_run_1: ;
				}
				return;	
		}

		void ec_thread_2(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_start_0: ;
				while (1) {
				ec_stack_start->ec_frames.block.b = ec_stack_start->s;
				ec_stack_start->ec_frames.block.ec_cont = &ec_label_start_1;
				block(&ec_stack_start->ec_frames.block);
				return;
			ec_label_start_1: ;
				}
				return;	
		}
	|])
-- reentrance {{{2
	,([$paste|
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
	|],[$paste|
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

		void block(ec_frame_block_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_run_0: ;
				ec_stack_run->ec_frames.critical.c = 1;
				ec_stack_run->ec_frames.critical.ec_cont = &ec_label_run_1;
				critical(&ec_stack_run->ec_frames.critical);
				return;
			ec_label_run_1: ;
				return;	
			
			ec_label_critical_0: ;
				ec_stack_run->ec_frames.critical.ec_frames.block.b = ec_stack_run->ec_frames.critical.c + 1;
				ec_stack_run->ec_frames.critical.ec_frames.block.ec_cont = &ec_label_critical_1;
				block(&ec_stack_run->ec_frames.critical.ec_frames.block);
				return;
			ec_label_critical_1: ;
				return;
		}

		void ec_thread_2(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_start_0: ;
				ec_stack_start->ec_frames.critical.c = 2;
				ec_stack_start->ec_frames.critical.ec_cont = &ec_label_start_1;
				critical(&ec_stack_start->ec_frames.critical);
				return;
			ec_label_start_1: ;
				return;	
			
			ec_label_critical_0: ;
				ec_stack_start->ec_frames.critical.ec_frames.block.b = ec_stack_start->ec_frames.critical.c + 1;
				ec_stack_start->ec_frames.critical.ec_frames.block.ec_cont = &ec_label_critical_1;
				block(&ec_stack_start->ec_frames.critical.ec_frames.block);
				return;
			ec_label_critical_1: ;
				return;
		}
	|])

-- return value {{{2
	, ([$paste|
		__attribute__((tc_blocking)) int block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i;
			i = block(i);
		}
	|],[$paste|
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

		void block(ec_frame_block_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_start_0: ;
				ec_stack_start->ec_frames.block.i = ec_stack_start->i;
				ec_stack_start->ec_frames.block.ec_cont = &ec_label_start_1;
				block(&ec_stack_start->ec_frames.block);
				return;
			ec_label_start_1: ;
				ec_stack_start->i = ec_stack_start->ec_frames.block.ec_result;
				return;	
		}
	|])
-- end {{{2
	]
-- util {{{1
-- runTests :: [(String, String)] -> Test {{{2
runTests :: String -> [(String, String)] -> Test
runTests label cases = TestLabel label $ TestList $ map runTest $ zip [1..] cases

-- runTest :: (Int, (String, String)) -> Test {{{2
runTest :: (Int, (String, String)) -> Test
runTest (number, (code, expected)) = TestCase $ assertEqual name expected' result
	where
		name = "test" ++ show number
		expected' = show $ pretty $ parse expected
		opt = defaultOptions
		(ana, ast) = case execER opt (analysis' (parse code)) of
			Left e -> error e
			Right x -> x
		(ast', _) = execWR (opt, ana) (transformation ast)
		result = show $ pretty ast'
