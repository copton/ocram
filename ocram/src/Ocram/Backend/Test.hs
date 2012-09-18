module Ocram.Backend.Test (tests) where

-- imports {{{1
import Test.Framework (Test, testGroup)
import Test.HUnit (assertEqual, Assertion, (@=?))
import Ocram.Backend.CreateTStacks
import Ocram.Backend.CreateEStacks

tests :: Test -- {{{1
tests = testGroup "Backend" []
