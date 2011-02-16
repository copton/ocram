module Tests.Analysis.CallGraph (
    tests
) where

import Tests.Analysis.Utils (runTests)

import Analysis.Types.CallGraph (Entry(Entry))
import Context (ctxCallGraph)
import Analysis.Types.FunctionMap (funId)
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
    reduce_entry (fid, (Entry callers callees)) = E (funId fid) (reduce_set callers) (reduce_set callees)
    reduce_set set = map funId $ elems set

tests = runTests "CallGraph" reduce [
    ("void foo() { bar(); } void bar() { }", L [E "foo" [] ["bar"], E "bar" ["foo"] []])
    ]
