module Ocram.Test.Tests.Analysis.CallGraph (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.CallGraph (call_graph)

type Input = (TDefinedFunctions, TBlockingFunctions)
type Output = TCallGraph

reduce :: Ast -> Input -> ER Output
reduce ast (df, bf) =
	return . reduce_cg =<< call_graph (enrich_df df) (enrich_bf bf) ast

setup :: TCase -> (Input, Output)
setup tc = ((tcDefinedFunctions tc, tcBlockingFunctions tc), tcCallGraph tc)

tests = runTests "CallGraph" reduce setup
--	(([$paste|
--			void foo() { 
--				bar();
--			} 
--			void bar() { }
--		|], ["foo", "bar"], []), 
--		[("foo", [], ["bar"]), ("bar", ["foo"], [])]
--	),
--	(([$paste|
--			void foo() { 
--				bar(); 
--				baz(); 
--			} 
--
--			void bar() { }
--			void baz();
--		|], ["foo", "bar"], []),
--		[("foo", [], ["bar", "baz"]), ("bar", ["foo"], [])]
--	),
--	(([$paste|
--			__attribute__((tc_blocking)) void foo();
--			void bar() {
--				baz();
--			}
--			
--			void baz() {
--				foo();
--				bar();
--			}
--		|], ["bar", "baz"], ["foo"]),
--		[("foo", ["baz"], []), ("bar", ["baz"], ["baz"]), ("baz", ["bar"], ["foo", "bar"])]
--	),
--	(([$paste|
--			__attribute__((tc_blocking)) void block(); 
--			__attribute__((tc_run_thread)) void start() {
--				rec();
--			} 
--			void rec() {
--				block(); 
--				start();
--			}
--		|], ["start", "rec"], ["block"]),
--		[("block", ["rec"], []), ("start", ["rec"], ["rec"]), ("rec", ["block", "start"], ["start"])]
--	),
--	(([$paste|
--			__attribute__((tc_blocking)) void foo(); 
--			void bar(void*); 
--			__attribute__((tc_run_thread)) void baz() { 
--				bar(&foo); 
--				foo();
--			}
--		|], ["baz"], ["foo"]),
--		[("foo", ["baz"], []), ("bar", ["baz"], []), ("baz", [], ["bar", "foo"])]
--	),
--	(([$paste|
--			__attribute__((tc_blocking)) int block(int i); 
--			__attribute__((tc_run_thread)) void start() { 
--				int i;
--				i = block(i);
--			}
--		|], ["start"], ["block"]),
--		[("block", ["start"], []), ("start", [], ["block"])])
--	]
