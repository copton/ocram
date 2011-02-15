module Main where

import Tests
import Test.HUnit

main = runTestTT Tests.tests >> return ()
