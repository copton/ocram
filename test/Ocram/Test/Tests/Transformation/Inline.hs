module Ocram.Test.Tests.Transformation.Inline (
	tests
) where

-- imports {{{1
import Ocram.Test.Lib (createContext, parse', paste)
import Ocram.Types (getAst, getOutputAst)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit))
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

-- tests {{{1
tests = runTests "Inline" [
-- setup {{{2
	([$paste|
		__attribute__((tc_blocking)) void foo(int i);
		__attribute__((tc_run_thread)) void bar() { 
			foo(23);
		}
	|],[$paste|
		typedef struct {
				void* ec_cont;
				int i;
		} ec_frame_foo_t;

		typedef struct {
				union {
						ec_frame_foo_t foo;
				} ec_frames;
		} ec_frame_bar_t;
		
		ec_frame_bar_t ec_stack_bar;

		void foo(ec_frame_foo_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_bar_0: ;
				ec_stack_bar->ec_frames.foo.i = 23;
				ec_stack_bar->ec_frames.foo.ec_cont = &ec_label_bar_1;
				foo(&ec_stack_bar->ec_frames.foo);
				return;
			ec_label_bar_1: ;
				return;	
		}
	|])
-- local variable {{{2
	, ([$paste|
		__attribute__((tc_blocking)) void foo(int i);

		__attribute__((tc_run_thread)) void bar() 
		{
			int i;
			foo(i);
		}
	|],[$paste|
		typedef struct {
			void* ec_cont;
			int i;
		} ec_frame_foo_t;

		typedef struct {
			union {
					ec_frame_foo_t foo;
			} ec_frames;
			int i;
		} ec_frame_bar_t;

		ec_frame_bar_t ec_stack_bar;

		void foo(ec_frame_foo_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_bar_0: ;
				ec_stack_bar->ec_frames.foo.i = ec_stack_bar->i;
				ec_stack_bar->ec_frames.foo.ec_cont = &ec_label_bar_1;
				foo(&ec_stack_bar->ec_frames.foo);
				return;
			ec_label_bar_1: ;
				return;	
		}
	|])
-- global variable {{{2
	, ([$paste|
		__attribute__((tc_blocking)) void foo(int i1, int i2);

		int k;

		__attribute__((tc_run_thread)) void bar() 
		{
			int j;
			foo(j, k);
		}
	|],[$paste|
		typedef struct {
			void* ec_cont;
			int i1;
			int i2;
		} ec_frame_foo_t;

		typedef struct {
			union {
					ec_frame_foo_t foo;
			} ec_frames;
			int j;
		} ec_frame_bar_t;

		ec_frame_bar_t ec_stack_bar;

		void foo(ec_frame_foo_t* frame);

		int k;

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_bar_0: ;
				ec_stack_bar->ec_frames.foo.i1 = ec_stack_bar->j;
				ec_stack_bar->ec_frames.foo.i2 = k;
				ec_stack_bar->ec_frames.foo.ec_cont = &ec_label_bar_1;
				foo(&ec_stack_bar->ec_frames.foo);
				return;
			ec_label_bar_1: ;
				return;	
		}
	|])
-- loop {{{2
	,([$paste|
		__attribute__((tc_blocking)) void foo(int j);
		__attribute__((tc_run_thread)) void bar() { 
			int i;
			while (i>0) {
				foo(i);
			}
		}
	|],[$paste|
		typedef struct {
				void* ec_cont;
				int j;
		} ec_frame_foo_t;

		typedef struct {
				union {
						ec_frame_foo_t foo;
				} ec_frames;
				int i;
		} ec_frame_bar_t;
		
		ec_frame_bar_t ec_stack_bar;

		void foo(ec_frame_foo_t* frame);

		void ec_thread_1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_bar_0: ;
				while (ec_stack_bar->i > 0) {
					ec_stack_bar->ec_frames.foo.j = ec_stack_bar->i;
					ec_stack_bar->ec_frames.foo.ec_cont = &ec_label_bar_1;
					foo(&ec_stack_bar->ec_frames.foo);
					return;
				ec_label_bar_1: ;
				}
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
	]
-- util {{{1
-- runTests :: [(String, String)] -> Test {{{2
runTests :: String -> [(String, String)] -> Test
runTests label cases = TestLabel label $ TestList $ map runTest $ zip [1..] cases

-- runTest :: (Int, (String, String)) -> Test {{{2
runTest :: (Int, (String, String)) -> Test
runTest (number, (code, expected)) = TestCase $ assertEqual name expected' result
	where
		expected' = show $ pretty $ getAst $ parse' $ expected
		name = "test" ++ show number
		result = show $ pretty $ getAst ast
		ast = case getOutputAst (createContext code Nothing) of
			Left e -> error e
			Right x -> x
