{-# LANGUAGE QuasiQuotes #-}
module Ocram.Print.Test
-- export {{{1
(
  tests
) where

-- import {{{1
import Control.Arrow ((***))
import Data.Generics (everything, mkQ, extQ)
import Data.List (intercalate)
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
-- function static variable {{{2
  , ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
03:   static int i = 0;
04:   block(i);
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
17:     static int i = 0;
        ec_stack_start.ec_frames.block.i = i;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
20:     block(&ec_stack_start.ec_frames.block);
        return;
    ec_label_start_1:
        ;
        return;
    }
  |], [
      (3, 17, Just 0)
    , (4, 20, Just 0)
  ])
-- global variable {{{2
  , ([lpaste|
    int k;
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
04:   k = 23;
05:   block(k);
    }
  |],[lpaste|
    int k;
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
18:     k = 23;
        ec_stack_start.ec_frames.block.i = k;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
21:     block(&ec_stack_start.ec_frames.block);
        return;
    ec_label_start_1:
        ;
        return;
    }
  |], [
      (4, 18, Just 0)
    , (5, 21, Just 0)
  ])
-- non-critical function call {{{2
  ,([lpaste|
    int k;
    void f() {
03:   k = 23; 
    }
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
07:   f();
08:   block(k);
    }
  |], [lpaste|
    int k;
    void f()
    {
04:     k = 23;
    }
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
22:     f();
        ec_stack_start.ec_frames.block.i = k;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
25:     block(&ec_stack_start.ec_frames.block);
        return;
    ec_label_start_1:
        ;
        return;
    }
  |], [
      (3,  4, Nothing)
    , (7, 22, Just 0)
    , (8, 25, Just 0)
  ])
-- critical function call {{{2
  ,([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    void c(int i) {
03:   block(i+1);
    }
    __attribute__((tc_run_thread)) void start() {
06:   c(23);
    }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_frame_block_t;
    typedef struct {
                void * ec_cont;
                union {
                    ec_frame_block_t block;
                } ec_frames;
                int i;
            } ec_frame_c_t;
    typedef struct {
                union {
                    ec_frame_c_t c;
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
        ec_stack_start.ec_frames.c.i = 23;
        ec_stack_start.ec_frames.c.ec_cont = &&ec_label_start_1;
26:     goto ec_label_c_0;
    ec_label_start_1:
        ;
        return;
    ec_label_c_0:
        ;
        ec_stack_start.ec_frames.c.ec_frames.block.i = ec_stack_start.ec_frames.c.i + 1;
        ec_stack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
34:     block(&ec_stack_start.ec_frames.c.ec_frames.block);
        return;
    ec_label_c_1:
        ;
        goto * (ec_stack_start.ec_frames.c.ec_cont);
    }
  |], [
      (6, 26, Just 0)
    , (3, 34, Just 0)
  ])
-- re-entrance {{{2
  ,([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    void c(int i) {
03:   block(i+1);
    }
    __attribute__((tc_run_thread)) void s1() {
06:   c(23);
    }
    __attribute__((tc_run_thread)) void s2() {
09:   c(42);
    }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_frame_block_t;
    typedef struct {
                void * ec_cont;
                union {
                    ec_frame_block_t block;
                } ec_frames;
                int i;
            } ec_frame_c_t;
    typedef struct {
                union {
                    ec_frame_c_t c;
                } ec_frames;
            } ec_frame_s1_t;
    typedef struct {
                union {
                    ec_frame_c_t c;
                } ec_frames;
            } ec_frame_s2_t;
    ec_frame_s1_t ec_stack_s1;
    ec_frame_s2_t ec_stack_s2;
    void block(ec_frame_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
        ec_stack_s1.ec_frames.c.i = 23;
        ec_stack_s1.ec_frames.c.ec_cont = &&ec_label_s1_1;
32:     goto ec_label_c_0;
    ec_label_s1_1:
        ;
        return;
    ec_label_c_0:
        ;
        ec_stack_s1.ec_frames.c.ec_frames.block.i = ec_stack_s1.ec_frames.c.i + 1;
        ec_stack_s1.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
40:     block(&ec_stack_s1.ec_frames.c.ec_frames.block);
        return;
    ec_label_c_1:
        ;
        goto * (ec_stack_s1.ec_frames.c.ec_cont);
    }
    void ec_thread_1(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
        ec_stack_s2.ec_frames.c.i = 42;
        ec_stack_s2.ec_frames.c.ec_cont = &&ec_label_s2_1;
54:     goto ec_label_c_0;
    ec_label_s2_1:
        ;
        return;
    ec_label_c_0:
        ;
        ec_stack_s2.ec_frames.c.ec_frames.block.i = ec_stack_s2.ec_frames.c.i + 1;
        ec_stack_s2.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
62:     block(&ec_stack_s2.ec_frames.c.ec_frames.block);
        return;
    ec_label_c_1:
        ;
        goto * (ec_stack_s2.ec_frames.c.ec_cont);
    }
  |], [
      (6, 32, Just 0)
    , (3, 40, Just 0)
    , (9, 54, Just 1)
    , (3, 62, Just 1)
  ])
-- multiple statements in a row {{{2
  ,([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    __attribute__((tc_run_thread)) void start() {
03:   block(23); block(42);
    }
  |], [lpaste|
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
        ec_stack_start.ec_frames.block.i = 42;
        ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_2;
25:     block(&ec_stack_start.ec_frames.block);
        return;
    ec_label_start_2:
        ;
        return;
    }
  |], [
      (3, 19, Just 0)
    , (3, 25, Just 0)
  ])
-- return {{{2
  ,([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    int c(int i) {
03:   if (i ==0) {
04:     return block(23);
      } else {
06:     return block(42);
      }
    }
    __attribute__((tc_run_thread)) void start() {
10:   c(0);
    }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_frame_block_t;
    typedef struct {
                void * ec_cont;
                int ec_result;
                union {
                    ec_frame_block_t block;
                } ec_frames;
                void ec_crit_0_0;
                void ec_crit_0_1;
                int i;
            } ec_frame_c_t;
    typedef struct {
                union {
                    ec_frame_c_t c;
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
        ec_stack_start.ec_frames.c.i = 0;
        ec_stack_start.ec_frames.c.ec_cont = &&ec_label_start_1;
29:     goto ec_label_c_0;
    ec_label_start_1:
        ;
        return;
    ec_label_c_0:
        ;
35:     if (ec_stack_start.ec_frames.c.i == 0)
        {
            ec_stack_start.ec_frames.c.ec_frames.block.i = 23;
            ec_stack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
            block(&ec_stack_start.ec_frames.c.ec_frames.block);
            return;
        ec_label_c_1:
            ;
            ec_stack_start.ec_frames.c.ec_crit_0_0 = ec_stack_start.ec_frames.c.ec_frames.block.ec_result;
            {
                ec_stack_start.ec_frames.c.ec_result = ec_stack_start.ec_frames.c.ec_crit_0_0;
46:             goto * (ec_stack_start.ec_frames.c.ec_cont);
            }
        }
        else
        {
            ec_stack_start.ec_frames.c.ec_frames.block.i = 42;
            ec_stack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_2;
            block(&ec_stack_start.ec_frames.c.ec_frames.block);
            return;
        ec_label_c_2:
            ;
            ec_stack_start.ec_frames.c.ec_crit_0_1 = ec_stack_start.ec_frames.c.ec_frames.block.ec_result;
            {
                ec_stack_start.ec_frames.c.ec_result = ec_stack_start.ec_frames.c.ec_crit_0_1;
60:             goto * (ec_stack_start.ec_frames.c.ec_cont);
            }
        }
    }
  |], [
      (10, 29, Just 0)
    , ( 3, 35, Just 0)
    , ( 4, 46, Just 0)
    , ( 6, 60, Just 0)
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
        (resultCode, resultLocMap) = print_with_log ast'
        resultCode' = BS.unpack resultCode
        resultLocMap' = reduce resultLocMap
      in do
        expectedCode @=? resultCode'
        let locs = intercalate "\n" $ map show $ everything (++) (mkQ [] traceLocationExpr `extQ` traceLocationStat) ast'
        assertEqual (locs ++ "\n" ++ show resultLocMap) expectedLocMap resultLocMap'

  where
    traceLocationExpr :: CExpr' -> [String]
    traceLocationExpr expr 
      | (enTraceLocation . annotation) expr = [show expr]
      | otherwise = []

    traceLocationStat :: CStat' -> [String]
    traceLocationStat stmt
      | (enTraceLocation . annotation) stmt = [show stmt]
      | otherwise = []
