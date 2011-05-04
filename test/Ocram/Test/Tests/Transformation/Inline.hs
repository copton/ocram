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

		void ec_thread1(void* ec_cont)
		{
			if (ec_cont != null)
				goto *ec_cont;

			ec_label_bar_0: {
				ec_stack_bar->ec_frames.foo.i = 23;
				ec_stack_bar->ec_frames.foo.ec_cont = &ec_label_bar_1;
				foo(&ec_stack_bar->ec_frames.foo);
				} return;
			ec_label_bar_1: {
				} return;	
		}
	|])
-- local variable {{{2
	-- ,([$paste|
	--   __attribute__((tc_blocking)) void foo(int i);

	--   __attribute__((tc_run_thread)) int bar(char param) {
	--       int i;
	--       foo(i);
	--   }
	-- |],[$paste|
	--   typedef struct {
	--       ec_continuation_t ec_cont;
	--       int i;
	--   } ec_frame_foo_t;

	--   typedef struct {
	--       ec_continuation_t ec_cont;
	--       int ec_result;
	--       union {
	--           ec_frame_foo_t foo;
	--       } ec_frames;
	--       char param;
	--       int i;
	--   } ec_frame_bar_t;

	--   void foo(int i);

	--   int bar(char param) {
	--       foo(((ec_frame_bar_t*) ec_cont.frame)->i);
	--   }
	-- |])
-- global variable {{{2
	-- ,([$paste|
	--   __attribute__((tc_blocking)) void foo(int i, int j);

	--   int j;

	--   __attribute__((tc_run_thread)) int bar(char param) {
	--       int i;
	--       foo(i, j);
	--   }
	-- |],[$paste|
	--   typedef struct {
	--       ec_continuation_t ec_cont;
	--       int i;
	--       int j;
	--   } ec_frame_foo_t;

	--   typedef struct {
	--       ec_continuation_t ec_cont;
	--       int ec_result;
	--       union {
	--           ec_frame_foo_t foo;
	--       } ec_frames;
	--       char param;
	--       int i;
	--   } ec_frame_bar_t;

	--   void foo(int i, int j);

	--   int j;

	--   int bar(char param) {
	--       foo(((ec_frame_bar_t*) ec_cont.frame)->i, j);
	--   }
	-- |])
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
