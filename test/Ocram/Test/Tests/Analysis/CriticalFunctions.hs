module Ocram.Test.Tests.Analysis.CriticalFunctions (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.CriticalFunctions (critical_functions)

type Input = (TCallGraph, TBlockingFunctions)
type Output = TCriticalFunctions

reduce :: Ast -> Input -> ER Output
reduce ast (cg, bf) =
	return . reduce_cf =<< critical_functions (enrich_cg cg) (enrich_bf bf) ast

setup :: TCase -> (Input, Output)
setup tc = ((tcCallGraph tc, tcBlockingFunctions tc), tcCriticalFunctions tc)

tests = runTests "CriticalFunctions" reduce setup
--	 ("void foo() { }", empty)
--	,("", empty)
--	,("void foo();", empty)
--	,("__attribute__((tc_blocking)) void foo();"
--		, fromList ["foo"])
--	,([$paste|
--		__attribute__((tc_blocking)) void foo(); 
--
--		void bar() { 
--			foo(); 
--		}
--		|], fromList ["foo", "bar"])
--	,([$paste|
--			__attribute__((tc_blocking)) void foo(); 
--		
--			void bar(); 
--			void baz() { 
--				bar(); 
--				foo(); 
--			}
--		|], fromList ["baz", "foo"])
--	,([$paste|
--			__attribute__((tc_blocking)) void D(); 
--			
--			void B() {
--				D();
--			} 
--
--			void C() {
--				D();
--			} 
--	
--			void A() {
--				B();
--				C();
--			}
--			|], fromList ["A", "B", "C", "D"])
--	]
