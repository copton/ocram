{-# LANGUAGE QuasiQuotes #-}
module Ocram.Print.Test
-- export {{{1
(
  tests
) where

-- import {{{1
import Control.Arrow ((***))
import Data.Generics (everything, mkQ, extQ)
import Ocram.Debug (ENodeInfo(..), tlocation)
import Language.C.Data.Node (nodeInfo)
import Language.C.Syntax.AST (CTranslUnit, CTranslationUnit(..), annotation)
import Ocram.Analysis (analysis)
import Ocram.Print (print_with_log)
import Ocram.Test.Lib (enumTestGroup, paste, lpaste, enrich, reduce, TLocMap)
import Ocram.Text (show_errors)
import Ocram.Transformation (transformation)
import Ocram.Transformation.Types (CExpr', CStat')
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), Assertion, assertFailure, assertEqual)

import qualified Data.ByteString.Char8 as BS

tests :: Test -- {{{1
tests = testGroup "Print" [test_print_with_log]

test_print_with_log :: Test -- {{{1
test_print_with_log = enumTestGroup "print_with_log" $ map runTest [
-- setup {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() { 
03:   block(23);
    }
  |],[lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_frame_block_t;
    typedef struct {
                union {
                    ec_frame_block_t block;
                } ec_frames;
            } ec_frame_start_t;
    ec_frame_start_t ec_stack_start;
    void block(ec_frame_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
        ec_stack_start.ec_frames.block.i = 23;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
19:     block(&ec_stack_start.ec_frames.block);
        return;
    ec_label_start_1:
        ;
        return;
    }
    |], [
      (3, 19, Just 0)
    ])
    ]
runTest :: (String, String, TLocMap) -> Assertion -- {{{1
runTest (inputCode, expectedCode, expectedLocMap) =
  let ast = enrich inputCode in
  case analysis ast of
    Left es -> assertFailure $ show_errors "analysis" es
    Right (cg, _) ->
      let
        ast' = (\(a, _, _)->a) $ transformation cg ast
        (resultCode, resultLocMap) = (BS.unpack *** reduce) (print_with_log ast')
      in do
        expectedCode @=? resultCode
        let locs = everything (++) (mkQ [] traceLocationExpr `extQ` traceLocationStat) ast'
        let dbg = (\(CTranslUnit xs _) -> map nodeInfo xs) ast
        assertEqual (show locs ++ "\n" ++ show dbg) expectedLocMap resultLocMap

  where
    traceLocationExpr :: CExpr' -> [String]
    traceLocationExpr expr 
      | (enTraceLocation . annotation) expr = [show expr]
      | otherwise = []

    traceLocationStat :: CStat' -> [String]
    traceLocationStat stmt
      | (enTraceLocation . annotation) stmt = [show stmt]
      | otherwise = []
