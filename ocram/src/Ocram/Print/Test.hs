{-# LANGUAGE QuasiQuotes #-}
module Ocram.Print.Test
-- export {{{1
(
  tests
) where

-- import {{{1
import Data.List (nub)
import Ocram.Analysis (analysis, Analysis(..))
import Ocram.Print (render_with_log)
import Ocram.Test.Lib (enumTestGroup, lpaste, enrich, reduce, TBreakpoint)
import Ocram.Text (show_errors)
import Ocram.Backend (tcode_2_ecode)
import Ocram.Intermediate (ast_2_ir)
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertFailure, assertEqual)

tests :: Test -- {{{1
tests = testGroup "Print" [test_render_with_log]

test_render_with_log :: Test -- {{{1
test_render_with_log = enumTestGroup "render_with_log" $ map runTest [
  -- , 01 - setup {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() { 
03:   block(23);
04: }
  |],[lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                union {
                    ec_tframe_block_t block;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
19:     ec_tstack_start.ec_frames.block.i = 23;
20:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
21:     block(&ec_tstack_start.ec_frames.block);
22:     return;
    ec_contlbl_L2_start:
        ;
25:     return;
    }
  |], [
      (3, 19, Just 0, False)
    , (3, 20, Just 0, False)
    , (3, 21, Just 0, True)
    , (3, 22, Just 0, False)
    , (4, 25, Just 0, False)
  ])
  , -- 02 - function static variable {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() {
      static int i = 0;
04:   block(i);
05: }
  |],[lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                union {
                    ec_tframe_block_t block;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    void block(ec_tframe_block_t *);
    static int ec_static_start_i = 0;
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
20:     ec_tstack_start.ec_frames.block.i = ec_static_start_i;
21:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
22:     block(&ec_tstack_start.ec_frames.block);
23:     return;
    ec_contlbl_L2_start:
        ;
26:     return;
    }
  |], [
      (4, 20, Just 0, False)
    , (4, 21, Just 0, False)
    , (4, 22, Just 0, True)
    , (4, 23, Just 0, False)
    , (5, 26, Just 0, False)
  ])
  , -- 03 - global variable {{{2
  ([lpaste|
    int k;
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() {
04:   k = 23;
05:   block(k);
06: }
  |],[lpaste|
    int k;
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                union {
                    ec_tframe_block_t block;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
20:     k = 23;
21:     ec_tstack_start.ec_frames.block.i = k;
22:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
23:     block(&ec_tstack_start.ec_frames.block);
24:     return;
    ec_contlbl_L2_start:
        ;
27:     return;
    }
  |], [
      (4, 20, Just 0, False)
    , (5, 21, Just 0, False)
    , (5, 22, Just 0, False)
    , (5, 23, Just 0, True)
    , (5, 24, Just 0, False)
    , (6, 27, Just 0, False)
  ])
  , -- 04 - non-critical function call {{{2
  ([lpaste|
    int k;
    void f() {
03:   k = 23; 
    }
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() {
07:   f();
08:   block(k);
09: }
  |], [lpaste|
    int k;
    void f()
    {
04:     k = 23;
    }
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                union {
                    ec_tframe_block_t block;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
24:     f();
25:     ec_tstack_start.ec_frames.block.i = k;
26:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
27:     block(&ec_tstack_start.ec_frames.block);
28:     return;
    ec_contlbl_L2_start:
        ;
31:     return;
    }
  |], [
      (3,  4, Nothing, False)
    , (7, 24, Just 0, False)
    , (8, 25, Just 0, False)
    , (8, 26, Just 0, False)
    , (8, 27, Just 0, True)
    , (8, 28, Just 0, False)
    , (9, 31, Just 0, False)
  ])
  , -- 05 - critical function call {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
    void c(int i) {
03:   block(i+1);
04: }
    __attribute__((tc_thread)) void start() {
06:   c(23);
07: }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                void * ec_cont;
                union {
                    ec_tframe_block_t block;
                } ec_frames;
                int i;
            } ec_tframe_c_t;
    typedef struct {
                union {
                    ec_tframe_c_t c;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
26:     ec_tstack_start.ec_frames.c.i = 23;
27:     ec_tstack_start.ec_frames.c.ec_cont = &&ec_contlbl_L2_start;
28:     goto ec_contlbl_L1_c;
    ec_contlbl_L2_start:
        ;
31:     return;
    ec_contlbl_L1_c:
        ;
34:     ec_tstack_start.ec_frames.c.ec_frames.block.i = ec_tstack_start.ec_frames.c.i + 1;
35:     ec_tstack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_contlbl_L2_c;
36:     block(&ec_tstack_start.ec_frames.c.ec_frames.block);
37:     return;
    ec_contlbl_L2_c:
        ;
40:     goto * (ec_tstack_start.ec_frames.c.ec_cont);
    }
  |], [
      (6, 26, Just 0, False)
    , (6, 27, Just 0, False)
    , (6, 28, Just 0, False)
    , (7, 31, Just 0, False)
    , (3, 34, Just 0, False)
    , (3, 35, Just 0, False)
    , (3, 36, Just 0, True)
    , (3, 37, Just 0, False)
    , (4, 40, Just 0, False)
  ])
  , -- 06 - re-entrance {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
    void c(int i) {
03:   block(i+1);
04: }
    __attribute__((tc_thread)) void s1() {
06:   c(23);
07: }
    __attribute__((tc_thread)) void s2() {
09:   c(42);
10: }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                void * ec_cont;
                union {
                    ec_tframe_block_t block;
                } ec_frames;
                int i;
            } ec_tframe_c_t;
    typedef struct {
                union {
                    ec_tframe_c_t c;
                } ec_frames;
            } ec_tframe_s1_t;
    typedef struct {
                union {
                    ec_tframe_c_t c;
                } ec_frames;
            } ec_tframe_s2_t;
    ec_tframe_s1_t ec_tstack_s1;
    ec_tframe_s2_t ec_tstack_s2;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_s1:
        ;
32:     ec_tstack_s1.ec_frames.c.i = 23;
33:     ec_tstack_s1.ec_frames.c.ec_cont = &&ec_contlbl_L2_s1;
34:     goto ec_contlbl_L1_c;
    ec_contlbl_L2_s1:
        ;
37:     return;
    ec_contlbl_L1_c:
        ;
40:     ec_tstack_s1.ec_frames.c.ec_frames.block.i = ec_tstack_s1.ec_frames.c.i + 1;
41:     ec_tstack_s1.ec_frames.c.ec_frames.block.ec_cont = &&ec_contlbl_L2_c;
42:     block(&ec_tstack_s1.ec_frames.c.ec_frames.block);
43:     return;
    ec_contlbl_L2_c:
        ;
46:     goto * (ec_tstack_s1.ec_frames.c.ec_cont);
    }
    void ec_thread_1(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_s2:
        ;
56:     ec_tstack_s2.ec_frames.c.i = 42;
57:     ec_tstack_s2.ec_frames.c.ec_cont = &&ec_contlbl_L2_s2;
58:     goto ec_contlbl_L1_c;
    ec_contlbl_L2_s2:
        ;
61:     return;
    ec_contlbl_L1_c:
        ;
64:     ec_tstack_s2.ec_frames.c.ec_frames.block.i = ec_tstack_s2.ec_frames.c.i + 1;
65:     ec_tstack_s2.ec_frames.c.ec_frames.block.ec_cont = &&ec_contlbl_L2_c;
66:     block(&ec_tstack_s2.ec_frames.c.ec_frames.block);
67:     return;
    ec_contlbl_L2_c:
        ;
70:     goto * (ec_tstack_s2.ec_frames.c.ec_cont);
    }
  |], [
      ( 6, 32, Just 0, False)
    , ( 6, 33, Just 0, False)
    , ( 6, 34, Just 0, False)
    , ( 7, 37, Just 0, False)
    , ( 3, 40, Just 0, False)
    , ( 3, 41, Just 0, False)
    , ( 3, 42, Just 0, True)
    , ( 3, 43, Just 0, False)
    , ( 4, 46, Just 0, False)
    , ( 9, 56, Just 1, False)
    , ( 9, 57, Just 1, False)
    , ( 9, 58, Just 1, False)
    , (10, 61, Just 1, False)
    , ( 3, 64, Just 1, False)
    , ( 3, 65, Just 1, False)
    , ( 3, 66, Just 1, True)
    , ( 3, 67, Just 1, False)
    , ( 4, 70, Just 1, False)
  ])
  , -- 07 - multiple statements in a row {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
    __attribute__((tc_thread)) void start() {
03:   block(23); block(42);
04: }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                union {
                    ec_tframe_block_t block;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
19:     ec_tstack_start.ec_frames.block.i = 23;
20:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
21:     block(&ec_tstack_start.ec_frames.block);
22:     return;
    ec_contlbl_L2_start:
        ;
25:     ec_tstack_start.ec_frames.block.i = 42;
26:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L3_start;
27:     block(&ec_tstack_start.ec_frames.block);
28:     return;
    ec_contlbl_L3_start:
        ;
31:     return;
    }
  |], [
      (3, 19, Just 0, False)
    , (3, 20, Just 0, False)
    , (3, 21, Just 0, True)
    , (3, 22, Just 0, False)
    , (3, 25, Just 0, False)
    , (3, 26, Just 0, False)
    , (3, 27, Just 0, True)
    , (3, 28, Just 0, False)
    , (4, 31, Just 0, False)
  ])
  , -- 08 - return {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
    int c(int i) {
03:   if (i ==0) {
04:     return block(23);
      } else {
06:     return block(42);
      }
    }
    __attribute__((tc_thread)) void start() {
10:   c(0);
11: }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                void * ec_cont;
                int ec_result;
                union {
                    ec_tframe_block_t block;
                } ec_frames;
                int i;
            } ec_tframe_c_t;
    typedef struct {
                union {
                    ec_tframe_c_t c;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    typedef struct {
                void ec_crit_1; void ec_crit_0;
            } ec_eframe_c_t;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        union {
            ec_eframe_c_t c;
        } ec_estack;
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
33:     ec_tstack_start.ec_frames.c.i = 0;
34:     ec_tstack_start.ec_frames.c.ec_cont = &&ec_contlbl_L2_start;
35:     goto ec_contlbl_L1_c;
    ec_contlbl_L2_start:
        ;
38:     return;
    ec_contlbl_L1_c:
        ;
41:     if (ec_tstack_start.ec_frames.c.i == 0)
        {
43:         ec_tstack_start.ec_frames.c.ec_frames.block.i = 23;
44:         ec_tstack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_contlbl_L3_c;
45:         block(&ec_tstack_start.ec_frames.c.ec_frames.block);
46:         return;
        }
        else
        {
50:         ec_tstack_start.ec_frames.c.ec_frames.block.i = 42;
51:         ec_tstack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_contlbl_L6_c;
52:         block(&ec_tstack_start.ec_frames.c.ec_frames.block);
53:         return;
        }
    ec_contlbl_L6_c:
        ;
57:     ec_estack.c.ec_crit_1 = ec_tstack_start.ec_frames.c.ec_frames.block.ec_result;
58:     ec_tstack_start.ec_frames.c.ec_result = ec_estack.c.ec_crit_1;
59:     goto * (ec_tstack_start.ec_frames.c.ec_cont);
    ec_contlbl_L3_c:
        ;
62:     ec_estack.c.ec_crit_0 = ec_tstack_start.ec_frames.c.ec_frames.block.ec_result;
63:     ec_tstack_start.ec_frames.c.ec_result = ec_estack.c.ec_crit_0;
64:     goto * (ec_tstack_start.ec_frames.c.ec_cont);
    }
  |], [
      (10, 33, Just 0, False)
    , (10, 34, Just 0, False)
    , (10, 35, Just 0, False)
    , (11, 38, Just 0, False)
    , ( 3, 41, Just 0, False)
    , ( 4, 43, Just 0, False)
    , ( 4, 44, Just 0, False)
    , ( 4, 45, Just 0, True)
    , ( 4, 46, Just 0, False)
    , ( 6, 50, Just 0, False)
    , ( 6, 51, Just 0, False)
    , ( 6, 52, Just 0, True)
    , ( 6, 53, Just 0, False)
    , ( 6, 57, Just 0, False)
    , ( 6, 58, Just 0, False)
    , ( 6, 59, Just 0, False)
    , ( 4, 62, Just 0, False)
    , ( 4, 63, Just 0, False)
    , ( 4, 64, Just 0, False)
  ])
  , -- 09 - while loops {{{2
  ([lpaste|
      __attribute__((tc_api)) int block(int i);
      void c(int i) {
03:     while (block(i) != 0);
04:   }
      __attribute__((tc_thread)) void start() {
06:     c(23);
07:   }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int ec_result; int i;
            } ec_tframe_block_t;
    typedef struct {
                void * ec_cont;
                union {
                    ec_tframe_block_t block;
                } ec_frames;
                int i;
            } ec_tframe_c_t;
    typedef struct {
                union {
                    ec_tframe_c_t c;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    typedef struct {
                int ec_crit_0;
            } ec_eframe_c_t;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        union {
            ec_eframe_c_t c;
        } ec_estack;
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
32:     ec_tstack_start.ec_frames.c.i = 23;
33:     ec_tstack_start.ec_frames.c.ec_cont = &&ec_contlbl_L2_start;
34:     goto ec_desugar_0_c;
    ec_contlbl_L2_start:
        ;
37:     return;
    ec_desugar_0_c:
        ;
40:     ec_tstack_start.ec_frames.c.ec_frames.block.i = ec_tstack_start.ec_frames.c.i;
41:     ec_tstack_start.ec_frames.c.ec_frames.block.ec_cont = &&ec_contlbl_L2_c;
42:     block(&ec_tstack_start.ec_frames.c.ec_frames.block);
43:     return;
    ec_contlbl_L2_c:
        ;
46:     ec_estack.c.ec_crit_0 = ec_tstack_start.ec_frames.c.ec_frames.block.ec_result;
47:     if (!(ec_estack.c.ec_crit_0 != 0))
        {
49:         goto * (ec_tstack_start.ec_frames.c.ec_cont);
        }
        else
        {
53:         goto ec_desugar_0_c;
        }
    }
  |], [
      ( 6, 32, Just 0, False)
    , ( 6, 33, Just 0, False)
    , ( 6, 34, Just 0, False)
    , ( 7, 37, Just 0, False)
    , ( 3, 40, Just 0, False)
    , ( 3, 41, Just 0, False)
    , ( 3, 42, Just 0, True)
    , ( 3, 43, Just 0, False)
    , ( 3, 46, Just 0, False)
    , ( 3, 47, Just 0, False)
    , ( 4, 49, Just 0, False)
    , ( 3, 53, Just 0, False)
  ])
  , -- 10 - declaration with initializer {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
    int f();
    __attribute__((tc_thread)) void start() {
04:   int i = 0;
05:   int k = f();
06:   block(i+k); 
07: }
  |], [lpaste|
    int f();
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                union {
                    ec_tframe_block_t block;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    typedef struct {
                int i; int k;
            } ec_eframe_start_t;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        union {
            ec_eframe_start_t start;
        } ec_estack;
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
26:     ec_estack.start.i = 0;
27:     ec_estack.start.k = f();
28:     ec_tstack_start.ec_frames.block.i = ec_estack.start.i + ec_estack.start.k;
29:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
30:     block(&ec_tstack_start.ec_frames.block);
31:     return;
    ec_contlbl_L2_start:
        ;
34:     return;
    }
  |], [
      ( 4, 26, Just 0, False)
    , ( 5, 27, Just 0, False)
    , ( 6, 28, Just 0, False)
    , ( 6, 29, Just 0, False)
    , ( 6, 30, Just 0, True)
    , ( 6, 31, Just 0, False)
    , ( 7, 34, Just 0, False)
  ])
  , -- 11 - declaration with critical initializer {{{2
  ([lpaste|
    __attribute__((tc_api)) int block();
    __attribute__((tc_thread)) void start() {
03:   int i = block();
04: }
  |], [lpaste|
    typedef struct {
                void * ec_cont; int ec_result;
            } ec_tframe_block_t;
    typedef struct {
                union {
                    ec_tframe_block_t block;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    typedef struct {
                int i;
            } ec_eframe_start_t;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        union {
            ec_eframe_start_t start;
        } ec_estack;
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
25:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
26:     block(&ec_tstack_start.ec_frames.block);
27:     return;
    ec_contlbl_L2_start:
        ;
30:     ec_estack.start.i = ec_tstack_start.ec_frames.block.ec_result;
31:     return;
    }
  |], [
      ( 3, 25, Just 0, False)
    , ( 3, 26, Just 0, True)
    , ( 3, 27, Just 0, False)
    , ( 3, 30, Just 0, False)
    , ( 4, 31, Just 0, False)
  ])
  , -- 12 - declaration in non-critical function {{{2
  ([lpaste|
    __attribute__((tc_api)) void block(int i);
    int f() {
03:   int i = 0;
04:   return i;
    }
    __attribute__((tc_thread)) void start() {
07:   block(f());
08: }
  |], [lpaste|
    int f()
    {
03:     int i = 0;
04:     return i;
    }
    typedef struct {
                void * ec_cont; int i;
            } ec_tframe_block_t;
    typedef struct {
                union {
                    ec_tframe_block_t block;
                } ec_frames;
            } ec_tframe_start_t;
    ec_tframe_start_t ec_tstack_start;
    void block(ec_tframe_block_t *);
    void ec_thread_0(void * ec_cont)
    {
        if (ec_cont)
        {
            goto * ec_cont;
        }
    ec_contlbl_L1_start:
        ;
24:     ec_tstack_start.ec_frames.block.i = f();
25:     ec_tstack_start.ec_frames.block.ec_cont = &&ec_contlbl_L2_start;
26:     block(&ec_tstack_start.ec_frames.block);
27:     return;
    ec_contlbl_L2_start:
        ;
30:     return;
    }
  |], [
      ( 3,  3, Nothing, False)
    , ( 4,  4, Nothing, False)
    , ( 7, 24, Just 0, False)
    , ( 7, 25, Just 0, False)
    , ( 7, 26, Just 0, True)
    , ( 7, 27, Just 0, False)
    , ( 8, 30, Just 0, False)
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
        cfs                     = ast_2_ir  (anaBlocking ana) (anaCritical ana)
        
        (eAst, _, _)            = tcode_2_ecode ana cfs
        (resultCode, resultBps) = render_with_log eAst
        resultBps'              = reduce $ nub resultBps
      in do
        assertEqual "code"          expectedCode resultCode
        assertEqual "breakpoints: " expectedBps  resultBps'
