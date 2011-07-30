module Ocram.Test.Tests.Analysis.CallGraph (
	tests
) where

import Ocram.Test.Lib
import Ocram.Types
import Ocram.Symbols (symbol)
import Ocram.Analysis.CallGraph (call_graph)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

type CG = [(String, [String], [String])]

reduce :: (String, [String], [String]) -> ER CG
reduce (code, df, bf) = do
	let ast = parse code
	let bf' = Set.fromList bf
	let df' = Set.fromList df
	cg <- call_graph df' bf' ast
	return $ map decompose $ Map.toList cg
	where
		decompose (function, (Entry callers callees)) =
			(show function, map show (Set.toList callers), map show (Set.toList callees))

tests = runTests "CallGraph" reduce [
	(([$paste|
			void foo() { 
				bar();
			} 
			void bar() { }
		|], ["foo", "bar"], []), 
		[("foo", [], ["bar"]), ("bar", ["foo"], [])]
	),
	(([$paste|
			void foo() { 
				bar(); 
				baz(); 
			} 

			void bar() { }
			void baz();
		|], ["foo", "bar"], []),
		[("foo", [], ["bar", "baz"]), ("bar", ["foo"], [])]
	),
	(([$paste|
			__attribute__((tc_blocking)) void foo();
			void bar() {
				baz();
			}
			
			void baz() {
				foo();
				bar();
			}
		|], ["bar", "baz"], ["foo"]),
		[("foo", ["baz"], []), ("bar", ["baz"], ["baz"]), ("baz", ["bar"], ["foo", "bar"])]
	),
	(([$paste|
			__attribute__((tc_blocking)) void block(); 
			__attribute__((tc_run_thread)) void start() {
				rec();
			} 
			void rec() {
				block(); 
				start();
			}
		|], ["start", "rec"], ["block"]),
		[("block", ["rec"], []), ("start", ["rec"], ["rec"]), ("rec", ["block", "start"], ["start"])]
	),
	(([$paste|
			__attribute__((tc_blocking)) void foo(); 
			void bar(void*); 
			__attribute__((tc_run_thread)) void baz() { 
				bar(&foo); 
				foo();
			}
		|], ["baz"], ["foo"]),
		[("foo", ["baz"], []), ("bar", ["baz"], []), ("baz", [], ["bar", "foo"])]
	),
	(([$paste|
			__attribute__((tc_blocking)) int block(int i); 
			__attribute__((tc_run_thread)) void start() { 
				int i;
				i = block(i);
			}
		|], ["start"], ["block"]),
		[("block", ["start"], []), ("start", [], ["block"])])
	]
