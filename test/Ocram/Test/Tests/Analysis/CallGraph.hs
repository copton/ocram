module Ocram.Test.Tests.Analysis.CallGraph (
	tests
) where

import Ocram.Test.Lib (createContext, paste)
import Ocram.Types (getCallGraph)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Types (Entry(Entry))
import Ocram.Symbols (symbol)
import Data.Map (toList)
import Data.Set (elems)
import Data.List (sort)

data E = E String [String] [String]

instance Ord E where
	(E s _ _) <= (E s' _ _) =  s <= s'

instance Eq E where
	(E function callers callees) == (E function' callers' callees') =
		   function == function' 
		&& (sort callers) == (sort callers') 
		&& (sort callees) == (sort callees')

instance Show E where
	show (E function callers callees) = show function ++ " " ++ show (sort callers) ++ " " ++ show (sort callees)

data L = L [E]

instance Eq L where
	(L es) == (L es') = (sort es) == (sort es')

instance Show L where
	show (L es) = show $ sort es

reduce code = do
	let ctx = createContext code Nothing
	cg <- getCallGraph ctx
	return $ L.(map reduce_entry).toList $ cg
	where
		reduce_entry (fid, (Entry callers callees)) = E (symbol fid) (reduce_set callers) (reduce_set callees)
		reduce_set set = map symbol $ elems set

tests = runTests "CallGraph" reduce [
	([$paste|
		void foo() { 
			bar();
		} 
		void bar() { }
	|],  L [E "foo" [] ["bar"], E "bar" ["foo"] []])
	,([$paste|
		void foo() { 
			bar(); 
			baz(); 
		} 

		void bar() { }
		void baz();
	|], L [E "foo" [] ["bar"], E "bar" ["foo"] []])
	,([$paste|
		__attribute__((tc_blocking)) void foo();
		void bar() {
			baz();
		}
		
		void baz() {
			foo();
			bar();
		}
	|], L [E "foo" ["baz"] [], E "bar" ["baz"] ["baz"], E "baz" ["bar"] ["foo", "bar"]])
	,([$paste|
		__attribute__((tc_blocking)) void block(); 
		__attribute__((tc_run_thread)) void start() {
			rec();
		} 
		void rec() {
			block(); 
			start();
		}
	|], L [E "block" ["rec"] [], E "start" ["rec"] ["rec"], E "rec" ["start"] ["block", "start"]])
	,([$paste|
		__attribute__((tc_blocking)) void foo(); 
		void bar(void*); 
		__attribute__((tc_run_thread)) void baz() { 
			bar(&foo); 
			foo();
		}
	|], L [E "foo" ["baz"] [], E "baz" [] ["foo"]])
	,([$paste|
		__attribute__((tc_blocking)) int block(int i); 
		__attribute__((tc_run_thread)) void start() { 
			int i;
			i = block(i);
		}
	|], L [E "block" ["start"] [], E "start" [] ["block"]])
	]
