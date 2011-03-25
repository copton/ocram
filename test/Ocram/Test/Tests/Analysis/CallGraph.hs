module Ocram.Test.Tests.Analysis.CallGraph (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Analysis.Types (Entry(Entry))
import Ocram.Context (ctxCallGraph)
import Ocram.Symbols (symbol)
import Data.Map (toList)
import Data.Set (elems)
import Data.List (sort)

data E = E String [String] [String] deriving Show

instance Ord E where
	(E s _ _) <= (E s' _ _) =  s <= s'

instance Eq E where
	(E function callers callees) == (E function' callers' callees') =
		   function == function' 
		&& (sort callers) == (sort callers') 
		&& (sort callees) == (sort callees')

data L = L [E]

instance Eq L where
	(L es) == (L es') = (sort es) == (sort es')

instance Show L where
	show (L es) = show es


reduce = L.(map reduce_entry).toList.ctxCallGraph
	where
	reduce_entry (fid, (Entry callers callees)) = E (symbol fid) (reduce_set callers) (reduce_set callees)
	reduce_set set = map symbol $ elems set

tests = runTests "CallGraph" reduce [
	("void foo() { bar();        } void bar() { }",              L [E "foo" [] ["bar"], E "bar" ["foo"] []]),
	("void foo() { bar(); baz(); } void bar() { }; void baz();", L [E "foo" [] ["bar"], E "bar" ["foo"] []])
	]
