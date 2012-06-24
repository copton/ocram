{-# LANGUAGE QuasiQuotes #-}
module Ruab.Frontend.Test (tests) where

-- imports {{{1
import Ruab.Test.Lib (enumTestGroup)
import Ruab.Frontend.Internal
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, (@=?))

tests :: Test  -- {{{1
tests = testGroup "Frontend" [test_render_info]

test_render_info :: Test
test_render_info = enumTestGroup "render_info" $ map runTest [
    ([(1, InfThread 1)], "1\n")
  , ([(1, InfBreakpoint 2)], " 2\n")
  , ([(1, InfHighlight)], "  #\n")
  , ([(1, InfHighlight), (1, InfThread 2), (1, InfBreakpoint 3)], "23#\n")
  , ([(2, InfThread 1)], "\n1\n")
  , ([(1, InfHighlight), (2, InfThread 2), (3, InfBreakpoint 3)], "  #\n2\n 3\n")
  ]
  where
  runTest :: ([InfoInstance], String) -> Assertion
  runTest (infos, expected) = expected @=? render_info infos
