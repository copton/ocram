module Ocram.Test.Tests.Analysis.StartRoutines (
	tests
) where 

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.StartRoutines (start_routines)

type Input = TDefinedFunctions
type Output = TStartRoutines

reduce :: Ast -> Input -> ER Output
reduce ast df = return . reduce_sr =<< start_routines (enrich_df df) ast

setup :: TCase -> (Input, Output)
setup tc = (tcDefinedFunctions tc, tcStartRoutines tc)

tests = runTests "StartRoutines" reduce setup
--	[ (
--			(
--				"__attribute__((tc_run_thread)) void foo() { }", 
--				Set.singleton "foo"
--			),
--			Set.singleton "foo"
--		),
--	  (
--			(
--				"void __attribute__((tc_run_thread)) foo() { }", 
--				Set.singleton "foo"
--			),
--			Set.singleton "foo"
--		),
--	 (("void foo() { }", Set.singleton "foo"), Set.empty),
--	 (("", Set.empty), Set.empty),
--	 (("void foo() {}", Set.empty), Set.empty)
--	]
