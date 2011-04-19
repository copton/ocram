module Ocram.Test.Tests.Transformation.DataFlow (
	tests
) where

-- imports {{{1
import Ocram.Test.Lib (createContext, parse', paste)
import Ocram.Types (getAst, getStacklessAst)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit))

--- tests {{{1
tests = runTests [
-- setup {{{2
	([$paste|
		__attribute__((tc_blocking)) void foo(int i);
		__attribute__((tc_run_thread)) void bar() { 
			foo(23);
		}
	|],[$paste|
		typedef struct {
				ec_continuation_t ec_cont;
				int i;
		} ec_frame_foo_t;

		typedef struct {
				ec_continuation_t ec_cont;
				union {
						ec_frame_foo_t foo;
				} ec_frames;
		} ec_frame_bar_t;

		void foo(int i);

		void bar() { 
			foo(23);
		}
	|])
-- local variable {{{2
	,([$paste|
		__attribute__((tc_blocking)) void foo(int i);

		__attribute__((tc_run_thread)) int bar(char param) {
				int i;
				foo(i);
		}
	|],[$paste|
		typedef struct {
				ec_continuation_t ec_cont;
				int i;
		} ec_frame_foo_t;

		typedef struct {
				ec_continuation_t ec_cont;
				int ec_result;
				union {
						ec_frame_foo_t foo;
				} ec_frames;
				char param;
				int i;
		} ec_frame_bar_t;

		void foo(int i);

		int bar(char param) {
				foo(((ec_frame_bar_t*) ec_cont->frame)->i);
		}
	|])
-- global variable {{{2
	,([$paste|
		__attribute__((tc_blocking)) void foo(int i, int j);

		int j;

		__attribute__((tc_run_thread)) int bar(char param) {
				int i;
				foo(i, j);
		}
	|],[$paste|
		typedef struct {
				ec_continuation_t ec_cont;
				int i;
				int j;
		} ec_frame_foo_t;

		typedef struct {
				ec_continuation_t ec_cont;
				int ec_result;
				union {
						ec_frame_foo_t foo;
				} ec_frames;
				char param;
				int i;
		} ec_frame_bar_t;

		void foo(int i, int j);

		int j;

		int bar(char param) {
				foo(((ec_frame_bar_t*) ec_cont->frame)->i, j);
		}
	|])
	]

-- utils {{{1
runTests :: [(String, String)] -> Test
runTests cases = TestLabel "DataFlow" $ TestList $ map runTest $ zip [1..] cases

runTest :: (Int, (String, String)) -> Test
runTest (number, (code, expected)) = TestCase $ assertEqual name expected' result
	where
		expected' = show $ pretty $ strip $ getAst $ parse' $ bootstrap ++ expected
		name = "test" ++ show number
		result = show $ pretty $ getAst ast
		ast = case getStacklessAst (createContext code Nothing) of
			Left e -> error e
			Right x -> x
		strip(CTranslUnit (_:_:decls) ni) = CTranslUnit decls ni
		bootstrap = [$paste|
			typedef struct { 
				void* frame;
				void* label;
			} ec_continuation_t;
			ec_continuation_t* ec_cont;
		|]
