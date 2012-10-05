{-# LANGUAGE QuasiQuotes #-}
module Ocram.Print.Test
-- export {{{1
(
  tests
) where

-- import {{{1
--import Data.Generics (everything, mkQ, extQ)
--import Data.List (intercalate)
--import Ocram.Debug (ENodeInfo(..))
--import Language.C.Syntax.AST (annotation)
import Ocram.Analysis (analysis, Analysis(..))
import Ocram.Print (print_with_log)
import Ocram.Test.Lib (enumTestGroup, lpaste, enrich, reduce, TBreakpoint)
import Ocram.Text (show_errors)
import Ocram.Backend (tcode_2_ecode)
import Ocram.Intermediate (ast_2_ir)
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertFailure, assertEqual)

import qualified Data.ByteString.Char8 as BS

tests :: Test -- {{{1
tests = testGroup "Print" [test_print_with_log]

test_print_with_log :: Test -- {{{1
test_print_with_log = enumTestGroup "print_with_log" $ map runTest [
-- , 01 - setup {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
02: __attribute__((tc_run_thread)) void start() { 
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
17:     ec_stack_start.ec_frames.block.i = 23;
18:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
19:     block(&ec_stack_start.ec_frames.block);
20:     return;
    ec_label_start_1:
        ;
23:     return;
    }
    |], [
        (3, 17, Just 0, False)
      , (3, 18, Just 0, False)
      , (3, 19, Just 0, True)
      , (3, 20, Just 0, False)
      , (2, 23, Just 0, False)
    ]
  )
  , -- 02 - function static variable {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
02: __attribute__((tc_run_thread)) void start() {
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
18:     ec_stack_start.ec_frames.block.i = i;
19:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
20:     block(&ec_stack_start.ec_frames.block);
21:     return;
    ec_label_start_1:
        ;
24:     return;
    }
  |], [
      (3, 17, Just 0, False)
    , (4, 18, Just 0, False)
    , (4, 19, Just 0, False)
    , (4, 20, Just 0, True)
    , (4, 21, Just 0, False)
    , (2, 24, Just 0, False)
  ])
  , -- 03 - global variable {{{2
  ([lpaste|
    int k;
    __attribute__((tc_blocking)) void block(int i);
03: __attribute__((tc_run_thread)) void start() {
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
19:     ec_stack_start.ec_frames.block.i = k;
20:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
21:     block(&ec_stack_start.ec_frames.block);
22:     return;
    ec_label_start_1:
        ;
25:     return;
    }
  |], [
      (4, 18, Just 0, False)
    , (5, 19, Just 0, False)
    , (5, 20, Just 0, False)
    , (5, 21, Just 0, True)
    , (5, 22, Just 0, False)
    , (3, 25, Just 0, False)
  ])
  , -- 04 - non-critical function call {{{2
  ([lpaste|
    int k;
    void f() {
03:   k = 23; 
    }
    __attribute__((tc_blocking)) void block(int i);
06: __attribute__((tc_run_thread)) void start() {
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
23:     ec_stack_start.ec_frames.block.i = k;
24:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
25:     block(&ec_stack_start.ec_frames.block);
26:     return;
    ec_label_start_1:
        ;
29:     return;
    }
  |], [
      (3,  4, Nothing, False)
    , (7, 22, Just 0, False)
    , (8, 23, Just 0, False)
    , (8, 24, Just 0, False)
    , (8, 25, Just 0, True)
    , (8, 26, Just 0, False)
    , (6, 29, Just 0, False)
  ])
  , -- 05 - critical function call {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
02: void c(int i) {
03:   block(i+1);
    }
05: __attribute__((tc_run_thread)) void start() {
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
24:     ec_stack_start.ec_frames.c.i = 23;
25:     ec_stack_start.ec_frames.c.ec_cont = &&ec_label_start_1;
26:     goto ec_label_c_0;
    ec_label_start_1:
        ;
28:     return;
    ec_label_c_0:
        ;
32:     ec_stack_start.ec_frames.c.ec_frames.block.i = ec_stack_start.ec_frames.c.i + 1;
33:     ec_stack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
34:     block(&ec_stack_start.ec_frames.c.ec_frames.block);
35:     return;
    ec_label_c_1:
        ;
38:     goto * (ec_stack_start.ec_frames.c.ec_cont);
    }
  |], [
      (6, 24, Just 0, False)
    , (6, 25, Just 0, False)
    , (6, 26, Just 0, False)
    , (5, 29, Just 0, False)
    , (3, 32, Just 0, False)
    , (3, 33, Just 0, False)
    , (3, 34, Just 0, True)
    , (3, 35, Just 0, False)
    , (2, 38, Just 0, False)
  ])
  , -- 06 - re-entrance {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
02: void c(int i) {
03:   block(i+1);
    }
05: __attribute__((tc_run_thread)) void s1() {
06:   c(23);
    }
08: __attribute__((tc_run_thread)) void s2() {
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
30:     ec_stack_s1.ec_frames.c.i = 23;
31:     ec_stack_s1.ec_frames.c.ec_cont = &&ec_label_s1_1;
32:     goto ec_label_c_0;
    ec_label_s1_1:
        ;
35:     return;
    ec_label_c_0:
        ;
38:     ec_stack_s1.ec_frames.c.ec_frames.block.i = ec_stack_s1.ec_frames.c.i + 1;
39:     ec_stack_s1.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
40:     block(&ec_stack_s1.ec_frames.c.ec_frames.block);
41:     return;
    ec_label_c_1:
        ;
44:     goto * (ec_stack_s1.ec_frames.c.ec_cont);
    }
    void ec_thread_1(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
52:     ec_stack_s2.ec_frames.c.i = 42;
53:     ec_stack_s2.ec_frames.c.ec_cont = &&ec_label_s2_1;
54:     goto ec_label_c_0;
    ec_label_s2_1:
        ;
57:     return;
    ec_label_c_0:
        ;
60:     ec_stack_s2.ec_frames.c.ec_frames.block.i = ec_stack_s2.ec_frames.c.i + 1;
61:     ec_stack_s2.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
62:     block(&ec_stack_s2.ec_frames.c.ec_frames.block);
63:     return;
    ec_label_c_1:
        ;
66:     goto * (ec_stack_s2.ec_frames.c.ec_cont);
    }
  |], [
      (6, 30, Just 0, False)
    , (6, 31, Just 0, False)
    , (6, 32, Just 0, False)
    , (5, 35, Just 0, False)
    , (3, 38, Just 0, False)
    , (3, 39, Just 0, False)
    , (3, 40, Just 0, True)
    , (3, 41, Just 0, False)
    , (2, 44, Just 0, False)
    , (9, 52, Just 1, False)
    , (9, 53, Just 1, False)
    , (9, 54, Just 1, False)
    , (8, 57, Just 1, False)
    , (3, 60, Just 1, False)
    , (3, 61, Just 1, False)
    , (3, 62, Just 1, True)
    , (3, 63, Just 1, False)
    , (2, 66, Just 1, False)
  ])
  , -- 07 - multiple statements in a row {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
02: __attribute__((tc_run_thread)) void start() {
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
17:     ec_stack_start.ec_frames.block.i = 23;
18:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
19:     block(&ec_stack_start.ec_frames.block);
20:     return;
    ec_label_start_1:
        ;
23:     ec_stack_start.ec_frames.block.i = 42;
24:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_2;
25:     block(&ec_stack_start.ec_frames.block);
26:     return;
    ec_label_start_2:
        ;
29:     return;
    }
  |], [
      (3, 17, Just 0, False)
    , (3, 18, Just 0, False)
    , (3, 19, Just 0, True)
    , (3, 20, Just 0, False)
    , (3, 23, Just 0, False)
    , (3, 24, Just 0, False)
    , (3, 25, Just 0, True)
    , (3, 26, Just 0, False)
    , (2, 29, Just 0, False)
  ])
  , -- 08 - return {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    int c(int i) {
03:   if (i ==0) {
04:     return block(23);
      } else {
06:     return block(42);
      }
    }
09: __attribute__((tc_run_thread)) void start() {
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
27:     ec_stack_start.ec_frames.c.i = 0;
28:     ec_stack_start.ec_frames.c.ec_cont = &&ec_label_start_1;
29:     goto ec_label_c_0;
    ec_label_start_1:
        ;
32:     return;
    ec_label_c_0:
        ;
35:     if (ec_stack_start.ec_frames.c.i == 0)
        {
37:         ec_stack_start.ec_frames.c.ec_frames.block.i = 23;
38:         ec_stack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
39:         block(&ec_stack_start.ec_frames.c.ec_frames.block);
40:         return;
        ec_label_c_1:
            ;
43:         ec_stack_start.ec_frames.c.ec_crit_0_0 = ec_stack_start.ec_frames.c.ec_frames.block.ec_result;
            {
45:             ec_stack_start.ec_frames.c.ec_result = ec_stack_start.ec_frames.c.ec_crit_0_0;
46:             goto * (ec_stack_start.ec_frames.c.ec_cont);
            }
        }
        else
        {
51:         ec_stack_start.ec_frames.c.ec_frames.block.i = 42;
52:         ec_stack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_2;
53:         block(&ec_stack_start.ec_frames.c.ec_frames.block);
54:         return;
        ec_label_c_2:
            ;
57:         ec_stack_start.ec_frames.c.ec_crit_0_1 = ec_stack_start.ec_frames.c.ec_frames.block.ec_result;
            {
59:             ec_stack_start.ec_frames.c.ec_result = ec_stack_start.ec_frames.c.ec_crit_0_1;
60:             goto * (ec_stack_start.ec_frames.c.ec_cont);
            }
        }
    }
  |], [
      (10, 27, Just 0, False)
    , (10, 28, Just 0, False)
    , (10, 29, Just 0, False)
    , ( 9, 32, Just 0, False)
    , ( 3, 35, Just 0, False)
    , ( 4, 37, Just 0, False)
    , ( 4, 38, Just 0, False)
    , ( 4, 39, Just 0, True)
    , ( 4, 40, Just 0, False)
    , ( 4, 43, Just 0, False)
    , ( 4, 45, Just 0, False)
    , ( 4, 46, Just 0, False)
    , ( 6, 51, Just 0, False)
    , ( 6, 52, Just 0, False)
    , ( 6, 53, Just 0, True)
    , ( 6, 54, Just 0, False)
    , ( 6, 57, Just 0, False)
    , ( 6, 59, Just 0, False)
    , ( 6, 60, Just 0, False)
  ])
  , -- 09 - while loops {{{2
  ([lpaste|
      __attribute__((tc_blocking)) int block(int i);
02:   void c(int i) {
03:     while (block(i) != 0);
      }
05:   __attribute__((tc_run_thread)) void start() {
06:     c(23);
      }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int ec_result; int i;
            } ec_frame_block_t;
    typedef struct {
                void * ec_cont;
                union {
                    ec_frame_block_t block;
                } ec_frames;
                int ec_crit_0_0;
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
25:     ec_stack_start.ec_frames.c.i = 23;
26:     ec_stack_start.ec_frames.c.ec_cont = &&ec_label_start_1;
27:     goto ec_label_c_0;
    ec_label_start_1:
        ;
30:     return;
    ec_label_c_0:
        ;
        {
        ec_ctrlbl_0_0:
            ;
36:         ec_stack_start.ec_frames.c.ec_frames.block.i = ec_stack_start.ec_frames.c.i;
37:         ec_stack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_label_c_1;
38:         block(&ec_stack_start.ec_frames.c.ec_frames.block);
39:         return;
        ec_label_c_1:
            ;
42:         ec_stack_start.ec_frames.c.ec_crit_0_0 = ec_stack_start.ec_frames.c.ec_frames.block.ec_result;
43:         if (!(ec_stack_start.ec_frames.c.ec_crit_0_0 != 0))
            {
45:             goto ec_ctrlbl_0_1;
            }
            ;
48:         goto ec_ctrlbl_0_0;
        ec_ctrlbl_0_1:
            ;
        }
52:     goto * (ec_stack_start.ec_frames.c.ec_cont);
    }
  |], [
      ( 6, 25, Just 0, False)
    , ( 6, 26, Just 0, False)
    , ( 6, 27, Just 0, False)
    , ( 5, 30, Just 0, False)
    , ( 3, 36, Just 0, False)
    , ( 3, 37, Just 0, False)
    , ( 3, 38, Just 0, True)
    , ( 3, 39, Just 0, False)
    , ( 3, 42, Just 0, False)
    , ( 3, 43, Just 0, False)
    , ( 3, 45, Just 0, False)
    , ( 3, 48, Just 0, False)
    , ( 2, 52, Just 0, False)
  ])
  , -- 10 - declaration with initializer {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    int f();
03: __attribute__((tc_run_thread)) void start() {
04:   int i = 0;
05:   int k = f();
06:   block(i+k); 
    }
  |], [lpaste|
    int f();
    typedef struct {
                void * ec_cont; int i;
            } ec_frame_block_t;
    typedef struct {
                union {
                    ec_frame_block_t block;
                } ec_frames;
                int i;
                int k;
            } ec_frame_start_t;
    ec_frame_start_t ec_stack_start;
    void block(ec_frame_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
20:     ec_stack_start.i = 0;
21:     ec_stack_start.k = f();
22:     ec_stack_start.ec_frames.block.i = ec_stack_start.i + ec_stack_start.k;
23:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
24:     block(&ec_stack_start.ec_frames.block);
25:     return;
    ec_label_start_1:
        ;
28:     return;
    }
  |], [
      ( 4, 20, Just 0, False)
    , ( 5, 21, Just 0, False)
    , ( 6, 22, Just 0, False)
    , ( 6, 23, Just 0, False)
    , ( 6, 24, Just 0, True)
    , ( 6, 25, Just 0, False)
    , ( 3, 28, Just 0, False)
  ])
  , -- 11 - declaration with critical initializer {{{2
  ([lpaste|
    __attribute__((tc_blocking)) int block();
02: __attribute__((tc_run_thread)) void start() {
03:   int i = block();
    }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int ec_result;
            } ec_frame_block_t;
    typedef struct {
                union {
                    ec_frame_block_t block;
                } ec_frames;
                int i;
            } ec_frame_start_t;
    ec_frame_start_t ec_stack_start;
    void block(ec_frame_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
18:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
19:     block(&ec_stack_start.ec_frames.block);
20:     return;
    ec_label_start_1:
        ;
23:     ec_stack_start.i = ec_stack_start.ec_frames.block.ec_result;
24:     return;
    }
  |], [
      ( 3, 18, Just 0, False)
    , ( 3, 19, Just 0, True)
    , ( 3, 20, Just 0, False)
    , ( 3, 23, Just 0, False)
    , ( 2, 24, Just 0, False)
  ])
  , -- 12 - declaration in non-critical function {{{2
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
    int f() {
03:   int i = 0;
04:   return i;
    }
06: __attribute__((tc_run_thread)) void start() {
07:   block(f());
    }
  |], [lpaste|
    int f()
    {
03:     int i = 0;
04:     return i;
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
22:     ec_stack_start.ec_frames.block.i = f();
23:     ec_stack_start.ec_frames.block.ec_cont = &&ec_label_start_1;
24:     block(&ec_stack_start.ec_frames.block);
25:     return;
    ec_label_start_1:
        ;
28:     return;
    }
  |], [
      ( 3,  3, Nothing, False)
    , ( 4,  4, Nothing, False)
    , ( 7, 22, Just 0, False)
    , ( 7, 23, Just 0, False)
    , ( 7, 24, Just 0, True)
    , ( 7, 25, Just 0, False)
    , ( 6, 28, Just 0, False)
  ])
  -- end {{{2
  ]

runTest :: (String, String, [TBreakpoint]) -> Assertion -- {{{1
runTest (inputCode, expectedCode, expectedBps) =
  let tAst = enrich inputCode in
  case analysis tAst of
    Left es -> assertFailure $ show_errors "analysis" es
    Right ana ->
      let
        cfs          = ast_2_ir  (anaBlocking ana) (anaCritical ana)
        
        (eAst, _)    = tcode_2_ecode ana cfs
        (resultCode, resultBps) = print_with_log eAst
        resultCode' = BS.unpack resultCode
        resultBps'  = reduce resultBps
      in do
--        let dbg = debug ast' -- needed for debugging the test cases
        assertEqual "code"           expectedCode resultCode'
        assertEqual "breakpoints: "  expectedBps  resultBps'

--   where
--     debug ast = intercalate "\n\n" $ map show $ everything (++) (mkQ [] traceExpr `extQ` traceStat) ast

--     traceExpr :: CExpr' -> [String]
--     traceExpr expr 
--       | (enBreakpoint . annotation) expr = [show expr]
--       | otherwise = []

--     traceStat :: CStat' -> [String]
--     traceStat stmt
--       | (enBreakpoint . annotation) stmt = [show stmt]
--       | otherwise = []
