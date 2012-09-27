{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Ocram.Intermediate.Test (tests) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Monad (msum)
import Compiler.Hoopl (showGraph)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Language.C.Data.Node (getLastTokenPos, posOfNode)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Position (posRow)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST
import Ocram.Analysis (Analysis(..), analysis)
import Ocram.Intermediate
import Ocram.Intermediate.BooleanShortCircuiting
import Ocram.Intermediate.BuildBasicBlocks
import Ocram.Intermediate.CollectDeclarations
import Ocram.Intermediate.DesugarControlStructures
import Ocram.Intermediate.NormalizeCriticalCalls
import Ocram.Intermediate.Optimize
import Ocram.Symbols (Symbol)
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, lpaste, paste)
import Ocram.Text (show_errors)
import Ocram.Util (fromJust_s, abort)
import Ocram.Query (return_type_fd, return_type_cd)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)
import Text.Printf (printf)

import qualified Data.Map as M
import qualified Data.Set as S

tests :: Test -- {{{1
tests = testGroup "Intermediate" [unit_tests, integration_tests]

-- types {{{1
type Input = String -- {{{2
type ScopedVariable = ( -- {{{2
    String   -- declaration
  , Int      -- first row of scope
  , Int      -- last row of scope
  )
  
type OutputCollectDeclarations = ( -- {{{2
    [(
        String             -- critical function name
      , [ScopedVariable]   -- automatic variables
      , [ScopedVariable]   -- static variables
    )]
  , String                 -- code
  )

type OutputDesugarControlStructures = -- {{{2
    String -- code

type OutputBooleanShortCircuiting = ( -- {{{2
    [(
      String    -- critical function name
    , [String]  -- declarations
    )]
  , String      -- code
  )

type OutputNormalize = ( -- {{{2
    [(
      String   -- critical function name
    , [String] -- declarations
    )]
  , String     -- code
  )

type OutputBasicBlocks = -- {{{2
    [(
      String -- critical function name
    , String -- entry label
    , String -- intermediate representation
    )]

type OutputOptimize = -- {{{2
  OutputBasicBlocks

type OutputCriticalVariables = -- {{{2
  [(
      String      -- critical function name
    , [TaggedVar] -- variables
  )] 

data TaggedVar -- {{{3
  = C String -- critical
  | U String -- uncritical

-- unit tests {{{1
unit_tests :: Test -- {{{2
unit_tests = testGroup "unit" [
    group "collect" test_collect_declarations unitTestsCollect
  ]
  where
    group name fun cases = enumTestGroup name $ map (uncurry fun) cases

unitTestsCollect :: [(Input, OutputCollectDeclarations)] -- {{{2
unitTestsCollect = [
  -- , 01 - nothing to do {{{3
  ([lpaste|
    __attribute__((tc_blocking)) void block();
02: void c(int i) {
      i++;
      block();
    }
    __attribute__((tc_run_thread)) void start() {
      c(23);
    }  
  |], ([
    ("start", [], [])
  , ("c", [("int i", 2, 5)], [])
  ], [paste|
    void c(int i) {
      i++;
      block();
    }
    void start () {
      c(23);
    }
  |]))
  , -- 02 - local variable with initializer {{{3
  ([lpaste|
    __attribute__((tc_blocking)) void block();
02: int c(int i) {
      int j = 42;
      block();
      return i + j;
    }
    __attribute__((tc_run_thread)) void start() {
      c(23);
    }  
  |], ([
    ("start", [], [])
  , ("c", [
        ("int i", 2, 6)
      , ("int j", 2, 6)
    ], [])
  ], [paste|
    int c(int i) {
      j = 42;
      block();
      return i + j;
    }
    void start () {
      c(23);
    }
  |]))
  , -- 03 - local variable shadowing {{{3
  ([lpaste|
    __attribute__((tc_blocking)) void block();
02: __attribute__((tc_run_thread)) void start() {
      int i = 23;
04:   {
        char i = 42;
        block();
      }
    }  
  |], ([
    ("start", [
        ("char ec_unique_i_0", 4, 7)
      , ("int i", 2, 8)
    ], [])
  ], [paste|
    void start () {
      i = 23;
      {
        ec_unique_i_0 = 42;
        block();
      }
    }
  |]))
  , -- 04 - local variable shadowing - with access {{{3
  ([lpaste|
    __attribute__((tc_blocking)) void block();
02: __attribute__((tc_run_thread)) void start() {
      int i = 23;
04:   {
        char i = 42;
        block();
        i = 19;
      }
    }  
  |], ([
    ("start", [
        ("char ec_unique_i_0", 4, 8)
      , ("int i", 2, 9)
    ], [])
  ], [paste|
    void start () {
      i = 23;
      {
        ec_unique_i_0 = 42;
        block();
        ec_unique_i_0 = 19;
      }
    }
  |]))
  , -- 05 - for loop with declaration {{{3
  ([lpaste|
    __attribute__((tc_blocking)) void block();
02: __attribute__((tc_run_thread)) void start() {
      int i = 23;
04:   for (int i=0; i<23; i++) {
        block();
      }
    }
  |], ([
    ("start", [
        ("int ec_unique_i_0", 4, 6)
      , ("int i", 2, 7)
    ], [])
  ], [paste|
    void start () {
      i = 23;
      {
        ec_unique_i_0 = 0;
        for (; ec_unique_i_0<23; ec_unique_i_0++) {
          block();
        }
      }
    }
  |]))
  , -- 06 - multiple declarations {{{3
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
02: __attribute__((tc_run_thread)) void start() {
      int i = 23, j = 42;
04:   {
        char i = 0, j = 1;
        block(i + j);
      }
08: }  
  |], ([
    ("start", [
        ("char ec_unique_i_0", 4, 7)
      , ("char ec_unique_j_0", 4, 7)
      , ("int i", 2, 8)
      , ("int j", 2, 8)
    ], [])
  ], [paste|
    void start () {
      i = 23;
      j = 42;
      {
        ec_unique_i_0 = 0;
        ec_unique_j_0 = 1;
        block(ec_unique_i_0 + ec_unique_j_0);
      }
    }
  |]))
  , -- 07 - static variable with initializer {{{3
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
02: __attribute__((tc_run_thread)) void start() {
      int j = 42;
04:   {
        static int i = 23;
        block (i + j);
07:   }
    }  
  |], ([
    ("start", [
        ("int j", 2, 8)
    ], [
        ("static int ec_static_start_i = 23", 4, 7)
    ])
  ], [paste|
    void start () {
      j = 42;
      {
        block(ec_static_start_i + j);
      }
    }
  |]))
  , -- 08 - static variable shadowing {{{3
  ([lpaste|
    __attribute__((tc_blocking)) void block(int i);
02: __attribute__((tc_run_thread)) void start() {
      static int i = 42;
04:   {
        static int i = 23;
        block (i);
        i = 13;
08:   }
      i = 14;
    }  
  |], ([
    ("start", [], [
        ("static int ec_static_start_ec_unique_i_0 = 23", 4, 8)
      , ("static int ec_static_start_i = 42", 2, 10)
    ])
  ], [paste|
    void start () {
      {
        block(ec_static_start_ec_unique_i_0);
        ec_static_start_ec_unique_i_0 = 13;
      }
      ec_static_start_i = 14;
    }
  |]))
{-
  , -- 09 - static variable shadowing - with access {{{3
  ([lpaste|
01: void foo() {
      static int i = 23;
03:   {
        static int i = 42;
        i = 19;
06:   }
07: }
  |], [paste|
    void foo() {
      {
        ec_static_foo_ec_unique_i_0 = 19;
      }
    }
  |], [], [
      ("static int i = 42", "ec_static_foo_ec_unique_i_0", 3, 6)
    , ("static int i = 23", "ec_static_foo_i", 1, 7)
  ])
  , -- 10 - multiple declarations mixed {{{3
  ([lpaste|
01: int foo() {
      static int i=0, j=1;
03:   {
        int i=23, j=42;
        return i + j;
06:   }
07: }
  |], [paste|
    int foo() {
      {
        ec_unique_i_0 = 23;
        ec_unique_j_0 = 42;
        return ec_unique_i_0 + ec_unique_j_0;
      }
    }
  |], [
      ("int i", "ec_unique_i_0", 3, 6)
    , ("int j", "ec_unique_j_0", 3, 6)
  ], [
      ("static int i = 0", "ec_static_foo_i", 1, 7)
    , ("static int j = 1", "ec_static_foo_j", 1, 7)
  ])
  , -- 11 - reuse without shadowing {{{3
  ([lpaste|
    int foo() {
02:   {
        int i = 0;
04:   }
05:   {
        int i = 1;
07:   }
    }
  |], [paste|
    int foo() {
      {
        i = 0;
      }
      {
        ec_unique_i_0 = 1;
      }
    }
  |], [
      ("int i", "ec_unique_i_0", 5, 7)
    , ("int i", "i", 2, 4)
  ], [])
  , -- 12 - substitution in initializer {{{3
  ([lpaste|
    void foo() {
02:   if (0) {
        int i = 0;
04:   } else {
        int i = 1;
        int size = bar(i);
07:   }
    }
  |], [paste|
    void foo()
    {
        if (0)
        {
            i = 0;
        }
        else
        {
            ec_unique_i_0 = 1;
            size = bar(ec_unique_i_0);
        }
    }
  |], [
      ("int size", "size", 4, 7)
    , ("int i", "ec_unique_i_0", 4, 7)
    , ("int i", "i", 2, 4)
  ], [])
-}
  ]

-- integration tests {{{1
integrationTestCases :: [TestCase] -- {{{2
integrationTestCases = [
    TestCase { -- , 01 - setup {{{3
    input           = -- {{{4
      [lpaste|
        __attribute__((tc_blocking)) void block();
        __attribute__((tc_run_thread)) void start() {
          block();
        }
      |]
  , outCollect      = ( -- {{{4
      [("start", [], [])]
    , [paste|
        void start() {
          block();
        }
      |]
    )
  , outDesugar      = Nothing -- {{{4
  , outShortCircuit = Nothing -- {{{4
  , outNormalize    = Nothing -- {{{4
  , outBasicBlocks  = [ -- {{{4
      ("start", "L1", [paste|
          L1:
          block(); GOTO L2

          L2:
          RETURN
      |])
    ]
  , outOptimize     = Nothing -- {{{4
  , outCritical     = Nothing -- {{{4
  }
  , TestCase { -- 02 - while loop {{{3
    input           = -- {{{4
      [lpaste|
        __attribute__((tc_blocking)) void block();
        void a();
        void b();
        __attribute__((tc_run_thread)) void start() {
          a();
          while (1) {
            block();
          }
          b();
        }
      |]
  , outCollect      = ( -- {{{4
      [("start", [], [])]
    , [paste|
        void start() {
          a();
          while (1) {
            block();
          }
          b();
        }
      |]
    )
  , outDesugar      = Just -- {{{4
      [paste|
        void start() {
          a();
          ec_ctrlbl_0: ;
          if (! 1) goto ec_ctrlbl_1;
          block();
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
          b();
        }
      |]
  , outShortCircuit = Nothing -- {{{4
  , outNormalize    = Nothing -- {{{4
  , outBasicBlocks  = [ -- {{{4
      ("start", "L1", [paste|
          L1:
          a();
          GOTO L2/ec_ctrlbl_0

          L2/ec_ctrlbl_0:
          IF !1 THEN L5/ec_ctrlbl_1 ELSE L3

          L3:
          block(); GOTO L4

          L4:
          GOTO L2/ec_ctrlbl_0

          L5/ec_ctrlbl_1:
          b();
          RETURN
      |])
    ]
  , outOptimize     = Just [ -- {{{4
      ("start", "L1", [paste|
          L1:
          a();
          GOTO L2/ec_ctrlbl_0

          L2/ec_ctrlbl_0:
          IF !1 THEN L5/ec_ctrlbl_1 ELSE L3

          L3:
          block(); GOTO L2/ec_ctrlbl_0

          L5/ec_ctrlbl_1:
          b();
          RETURN
      |])
    ]
  , outCritical     = Nothing -- {{{4
  }
  , TestCase { -- 03 - do loop {{{3
    input          = -- {{{4
      [lpaste|
        __attribute__((tc_blocking)) void block();
        void a();
        void b();
        __attribute__((tc_run_thread)) void start() {
          a();
          do {
            block();
          } while(1);
          b();
        }
      |]
  , outCollect     = ( -- {{{4
      [("start", [], [])]
    , [paste|
        void start() {
          a();
          do {
            block();
          } while(1);
          b();
        }
      |]
    )
  , outDesugar     = Just -- {{{4
      [paste|
        void start() {
          a();
          ec_ctrlbl_0: ;
          block();
          if (1) goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
          b();
        }
      |]
  , outShortCircuit = Nothing -- {{{4
  , outNormalize    = Nothing -- {{{4
  , outBasicBlocks = [ -- {{{4
      ("start", "L1", [paste|
          L1:
          a();
          GOTO L2/ec_ctrlbl_0

          L2/ec_ctrlbl_0:
          block(); GOTO L3

          L3:
          IF 1 THEN L2/ec_ctrlbl_0 ELSE L4/ec_ctrlbl_1

          L4/ec_ctrlbl_1:
          b();
          RETURN
      |])
    ]
  , outOptimize = Nothing -- {{{4
  , outCritical = Nothing -- {{{4
  }
  , TestCase { -- 04 - for loop - with continue {{{3
    input       = -- {{{4
      [lpaste|
        __attribute__((tc_blocking)) void block();
        void a();
        void b();
        __attribute__((tc_run_thread)) void start() {
          a();
06:       for (int i=0; i<23; i++) {
            if (i==2) continue;
            block(i);
          }
          b();
        }
      |]
    , outCollect = ( -- {{{4
        [("start", [("int i", 6, 9)], []) ]
      , [paste|
          void start() {
            a();
            {
              i=0;
              for (; i<23; i++) {
                if (i==2) continue;
                block(i);
              }
            }
            b();
          }
        |]
      )
    , outDesugar = Just  -- {{{4
        [paste|
          void start() {
            a();
            i = 0;

            ec_ctrlbl_0: ;
            if (!(i < 23)) goto ec_ctrlbl_2;
            if (i == 2) goto ec_ctrlbl_3; else goto ec_ctrlbl_4;

            ec_ctrlbl_3: ;
            goto ec_ctrlbl_1;

            ec_ctrlbl_4: ;
            block(i);

            ec_ctrlbl_1: ;
            i++;
            goto ec_ctrlbl_0;

            ec_ctrlbl_2: ;
            b();
          }
        |]
    , outShortCircuit = Nothing -- {{{4
    , outNormalize    = Nothing -- {{{4
    , outBasicBlocks = [ -- {{{4
        ("start", "L1", [paste|
          L1:
          a();
          i = 0;
          GOTO L2/ec_ctrlbl_0

          L2/ec_ctrlbl_0:
          IF !(i < 23) THEN L7/ec_ctrlbl_2 ELSE L3

          L3:
          IF i == 2 THEN L4/ec_ctrlbl_3 ELSE L5/ec_ctrlbl_4

          L4/ec_ctrlbl_3:
          GOTO L6/ec_ctrlbl_1

          L5/ec_ctrlbl_4:
          block(i); GOTO L6/ec_ctrlbl_1

          L6/ec_ctrlbl_1:
          i++;
          GOTO L2/ec_ctrlbl_0

          L7/ec_ctrlbl_2:
          b();
          RETURN
        |])
      ]
    , outOptimize = Just [ -- {{{4
        ("start", "L1", [paste|
          L1:
          a();
          i = 0;
          GOTO L2/ec_ctrlbl_0

          L2/ec_ctrlbl_0:
          IF !(i < 23) THEN L7/ec_ctrlbl_2 ELSE L3

          L3:
          IF i == 2 THEN L6/ec_ctrlbl_1 ELSE L5/ec_ctrlbl_4

          L5/ec_ctrlbl_4:
          block(i); GOTO L6/ec_ctrlbl_1

          L6/ec_ctrlbl_1:
          i++;
          GOTO L2/ec_ctrlbl_0

          L7/ec_ctrlbl_2:
          b();
          RETURN
        |])
      ]
    , outCritical = Just [ -- {{{4
        ("start", [C "i"])
      ]
  }
  , TestCase { -- 05 - for loop with explicit break {{{3
    input       = -- {{{4
      [lpaste|
        __attribute__((tc_blocking)) void block(int i);
        __attribute__((tc_run_thread)) void start() {
03:       for (int i=0; ; i++) {
            block(i);
            if (i == 23) break;
          }
        }
      |]
    , outCollect = ( -- {{{4
        [("start", [("int i", 3, 6)], []) ]
      , [paste|
          void start() {
            {
              i = 0;
              for (; ; i++) {
                block(i);
                if (i == 23) {break;}
              }
            }
          }
        |]
      )
    , outDesugar = Just  -- {{{4
        [paste|
          void start() {
            i = 0;
            ec_ctrlbl_0: ;
            block(i);
            if (i==23) goto ec_ctrlbl_3; else goto ec_ctrlbl_4;
            ec_ctrlbl_3: ;
            goto ec_ctrlbl_2;
            ec_ctrlbl_4: ;  
            ec_ctrlbl_1: ;  
            i++;
            goto ec_ctrlbl_0;
            ec_ctrlbl_2: ;
          }
        |]
    , outShortCircuit = Nothing -- {{{4
    , outNormalize    = Nothing -- {{{4
    , outBasicBlocks  = [       -- {{{4
        ("start", "L1", [paste|
          L1:
          i = 0;
          GOTO L2/ec_ctrlbl_0

          L2/ec_ctrlbl_0:
          block(i); GOTO L3

          L3:
          IF i == 23 THEN L4/ec_ctrlbl_3 ELSE L5/ec_ctrlbl_4

          L4/ec_ctrlbl_3:
          GOTO L7/ec_ctrlbl_2

          L5/ec_ctrlbl_4:
          GOTO L6/ec_ctrlbl_1

          L6/ec_ctrlbl_1:
          i++;
          GOTO L2/ec_ctrlbl_0

          L7/ec_ctrlbl_2:
          RETURN
        |])
      ]
    , outOptimize = Just [ -- {{{4
        ("start", "L1", [paste|
          L1:
          i = 0;
          GOTO L2/ec_ctrlbl_0

          L2/ec_ctrlbl_0:
          block(i); GOTO L3

          L3:
          IF i == 23 THEN L7/ec_ctrlbl_2 ELSE L6/ec_ctrlbl_1

          L6/ec_ctrlbl_1:
          i++;
          GOTO L2/ec_ctrlbl_0

          L7/ec_ctrlbl_2:
          RETURN
        |])
      ]
    , outCritical = Just [ -- {{{4
        ("start", [C "i"])
      ]
  }
  -- end {{{3
{-  
  , TestCase { -- {{{3
    input       = -- {{{4
      [lpaste|
        __attribute__((tc_blocking)) void block();
        __attribute__((tc_run_thread)) void start() {
        }
      |]
    , outCollect = ( -- {{{4
        [("start", [], []) ]
      , [paste|
        |]
      )
    , outDesugar = Just  -- {{{4
        [paste|
        |]
    , outShortCircuit = ( -- {{{4
        [("start", [])]
      , Nothing
      )
    , outNormalize   = ( -- {{{4
        [("start", [])]
      , Nothing
      )
    , outBasicBlocks = [ -- {{{4
        ("start", "L1", [paste|
        |])
      ]
    , outOptimize = -- {{{4
        Nothing
    , outCritical = [ -- {{{4
        ("start", [], [])
      ]
  }
-}
  ]

data TestCase = TestCase { -- {{{2
    input              :: Input
  , outCollect         :: OutputCollectDeclarations
  , outDesugar         :: Maybe OutputDesugarControlStructures
  , outShortCircuit    :: Maybe OutputBooleanShortCircuiting
  , outNormalize       :: Maybe OutputNormalize
  , outBasicBlocks     :: OutputBasicBlocks
  , outOptimize        :: Maybe OutputOptimize
  , outCritical        :: Maybe OutputCriticalVariables 
  }

-- wrappers {{{2
testCollectDeclarations :: TestCase -> Assertion -- {{{3
testCollectDeclarations = test_collect_declarations <$> input <*> outCollect

testDesugarControlStructures :: TestCase -> Assertion -- {{{3
testDesugarControlStructures = test_desugar_control_structures <$> input <*> output
  where
    output = fromMaybe <$> snd . outCollect <*> outDesugar

testBooleanShortCircuiting :: TestCase -> Assertion -- {{{3
testBooleanShortCircuiting = test_boolean_short_circuiting <$> input <*> output
  where
    output = (,) <$> emptyCfList <*> code
    code = fromMaybe <$> snd . outCollect <*> msum . sequence [fmap snd . outShortCircuit, outDesugar]

testNormalizeCriticalCalls :: TestCase -> Assertion -- {{{3
testNormalizeCriticalCalls = test_normalize_critical_calls <$> input <*> output
  where
    output = (,) <$> emptyCfList <*> code
    code   = fromMaybe <$> snd . outCollect <*> msum . sequence [fmap snd . outNormalize, fmap snd . outShortCircuit, outDesugar]

testBuildBasicBlocks :: TestCase -> Assertion -- {{{3
testBuildBasicBlocks = test_build_basic_blocks <$> input <*> outBasicBlocks

testOptimizeIr :: TestCase -> Assertion -- {{{3
testOptimizeIr = test_optimize_ir <$> input <*> output
  where
    output = fromMaybe <$> outBasicBlocks <*> outOptimize

testCriticalVariables :: TestCase -> Assertion -- {{{3
testCriticalVariables = test_critical_variables <$> input <*> output
  where
    output = fromMaybe <$> emptyCfList <*> outCritical

emptyCfList :: TestCase -> [(String, [a])] -- {{{3
emptyCfList = M.toList . M.map (\_ -> []) . anaCritical . analyze . input

integration_tests :: Test -- {{{2
integration_tests = testGroup "integration" $ _byCase
  where
    _byFunction = map runTest integrationTestFunctions
      where runTest (s, f) = enumTestGroup s $ map f integrationTestCases
    
    _byCase = zipWith runCase [(1::Int)..] integrationTestCases
      where
        runCase number tc = testGroup (printf "%.2d" number) $ map (runTest tc) integrationTestFunctions
        runTest tc (name, fun) = testCase name (fun tc)

integrationTestFunctions :: [(String, TestCase -> Assertion)] -- {{{2
integrationTestFunctions = [
    ("collect"     , testCollectDeclarations)
  , ("desugar"     , testDesugarControlStructures)
  , ("shortCircuit", testBooleanShortCircuiting)
  , ("normalize"   , testNormalizeCriticalCalls)
  , ("basicBlocks" , testBuildBasicBlocks)
  , ("optimize"    , testOptimizeIr)
  , ("critical"    , testCriticalVariables)
  ]

-- test functions -- {{{1
test_collect_declarations :: Input -> OutputCollectDeclarations -> Assertion -- {{{2
test_collect_declarations inputCode (expectedVars', expectedCode) = do
  let
    ana = analyze inputCode
    items = pipeline ana (map (\(CBlockStmt s) -> s) . collectDeclarations)
    outputCode = printOutputCode ana items
  assertEqual "output code" (blurrSyntax expectedCode) outputCode

  let
    expectedVars = M.fromList . map (\(x, y, z) -> (x, (y, z))) $ expectedVars'
    result = pipeline ana collect_declarations
  assertEqual "set of critical functions" (M.keysSet expectedVars) (M.keysSet result)
  sequence_ $ M.elems $ M.intersectionWithKey cmpVars expectedVars result

  where
    cmpVars fname (av, sv) (_, av', sv') = do
      assertEqual "number of automatic variables" (length av) (length av')
      mapM_ (cmpVar fname "automatic") $ zip av av'
      assertEqual "number of static variables" (length sv) (length sv')
      mapM_ (cmpVar fname "static")    $ zip sv sv'

    cmpVar fname kind ((decl, start, end), var) =
      let prefix = printf "function: '%s', %s variable: '%s', " fname kind decl in
      do
        assertEqual (prefix ++ "T-code decl") decl $ (show . pretty . var_decl) var
        assertEqual (prefix ++ "start of scope")  start ((posRow . posOfNode . $fromJust_s . var_scope) var)
        assertEqual (prefix ++ "end of scope") end ((posRow . fst . getLastTokenPos . $fromJust_s . var_scope) var)

test_desugar_control_structures :: Input -> OutputDesugarControlStructures -> Assertion -- {{{2
test_desugar_control_structures inputCode expectedCode = 
  let
    ana = analyze inputCode
    items = pipeline ana (
        desugarControlStructures
      . collectDeclarations
      )
    outputCode = printOutputCode ana items 
  in
    assertEqual "output code" (blurrSyntax expectedCode) outputCode
        
test_boolean_short_circuiting :: Input -> OutputBooleanShortCircuiting -> Assertion -- {{{2
test_boolean_short_circuiting inputCode (expectedDecls', expectedCode) = do
  let
    expectedDecls = M.fromList expectedDecls'
    ana = analyze inputCode
    items = pipeline ana (
        booleanShortCircuiting ana
      . desugarControlStructures
      . collectDeclarations
      )
    outputCode = printOutputCode ana items
  assertEqual "output code" (blurrSyntax expectedCode) outputCode

  let
    result = pipeline ana (
        boolean_short_circuiting (criticalFunctions ana)
      . desugarControlStructures
      . collectDeclarations
      )
  assertEqual "set of critical functions" (M.keysSet expectedDecls) (M.keysSet result)
  sequence_ $ M.elems $ M.intersectionWithKey cmpVars expectedDecls result

  where
    cmpVars fname decls (_, vars) = do
      assertEqual (printf "function: '%s', number of variables" fname) (length decls) (length vars)
      mapM_ (cmpVar fname) $ zip decls vars

    cmpVar fname (decl, var) = 
      let msg = printf "function: '%s', variable" fname in
      assertEqual msg decl ((show . pretty . var_decl) var)

test_normalize_critical_calls :: Input -> OutputNormalize -> Assertion -- {{{2
test_normalize_critical_calls inputCode (expectedDecls', expectedCode) = do
  let
    expectedDecls = M.fromList expectedDecls'
    ana = analyze inputCode
    stmts = pipeline ana (
        normalizeCriticalCalls ana
      . booleanShortCircuiting ana
      . desugarControlStructures
      . collectDeclarations
      )
    outputCode = printOutputCode ana stmts
  assertEqual "output code" (blurrSyntax expectedCode) outputCode

  let
    result = pipeline ana (
        normalize_critical_calls (returnTypes ana)
      . booleanShortCircuiting ana
      . desugarControlStructures
      . collectDeclarations
      )

  assertEqual "set of critical functions" (M.keysSet expectedDecls) (M.keysSet result)
  sequence_ $ M.elems $ M.intersectionWithKey cmpVars expectedDecls result

  where
    cmpVars fname decls (_, vars) = do
      assertEqual (printf "function: '%s', number of variables" fname) (length decls) (length vars)
      mapM_ (cmpVar fname) $ zip decls vars

    cmpVar fname (decl, var) = 
      let msg = printf "function: '%s', variable" fname in
      assertEqual msg decl ((show . pretty . var_decl) var)

test_build_basic_blocks :: Input -> OutputBasicBlocks -> Assertion -- {{{2
test_build_basic_blocks inputCode expectedIrs' = do
  let
    expectedIrs = M.fromList $ map (\(x, y, z) -> (x, (y, z))) expectedIrs'
    ana = analyze inputCode
    result = pipeline ana (
        buildBasicBlocks ana
      . normalizeCriticalCalls ana
      . booleanShortCircuiting ana
      . desugarControlStructures
      . collectDeclarations
      )
  assertEqual "set of critical functions (bodies)" (M.keysSet expectedIrs) (M.keysSet result)
  sequence_ $ M.elems $ M.intersectionWithKey cmp expectedIrs result

  where
    cmp :: String -> (String, String) -> (Label, Body) -> Assertion
    cmp fname (eentry, ebody) (oentry, obody) =
      let
        prefix = printf "function: '%s', " fname
        ebody' = (unlines . drop 1 . map (drop 10) . lines) ebody
        obody' = showGraph show obody
      in do
        assertEqual (prefix ++ " entry") eentry (show oentry)
        assertEqual (prefix ++ " body") ebody' obody'
      
test_optimize_ir :: Input -> OutputOptimize -> Assertion -- {{{2
test_optimize_ir inputCode expectedIrs' = do
  let
    expectedIrs = M.fromList $ map (\(x, y, z) -> (x, (y, z))) expectedIrs'
    ana = analyze inputCode
    result = pipeline ana (
        optimizeIr
      . buildBasicBlocks ana
      . normalizeCriticalCalls ana
      . booleanShortCircuiting ana
      . desugarControlStructures
      . collectDeclarations
      )
  assertEqual "set of critical functions (bodies)" (M.keysSet expectedIrs) (M.keysSet result)
  sequence_ $ M.elems $ M.intersectionWithKey cmp expectedIrs result

  where
    cmp :: String -> (String, String) -> (Label, Body) -> Assertion
    cmp fname (eentry, ebody) (oentry, obody) =
      let
        prefix = printf "function: '%s', " fname
        ebody' = (unlines . drop 1 . map (drop 10) . lines) ebody
        obody' = showGraph show obody
      in do
        assertEqual (prefix ++ " entry") eentry (show oentry)
        assertEqual (prefix ++ " body") ebody' obody'

test_critical_variables :: Input -> OutputCriticalVariables -> Assertion -- {{{2
test_critical_variables inputCode expectedVars' = do
  let
    ana = analyze inputCode
    funs = ast_2_ir (anaBlocking ana) (anaCritical ana)
    expectedVars = M.fromList expectedVars'
  assertEqual "set of critical functions" (M.keysSet expectedVars) (M.keysSet funs)
  sequence_ $ M.elems $ M.intersectionWithKey cmpVars expectedVars funs

  where
    cmpVars fname evs (Function ocs ous _ _ _ _) =
      let 
        (ecs, eus) = partitionEithers $ map sep evs
        msg        = printf "function: '%s', number of %s variables" fname
      in do
        assertEqual (msg "critical") (length ecs) (length ocs)
        mapM_ (cmpVar fname "critical")   $ zip ecs ocs
        assertEqual (msg "uncritical") (length eus) (length ous)
        mapM_ (cmpVar fname "uncritical") $ zip eus ous
      where
        sep (C v) = Left v
        sep (U v) = Right v

    cmpVar fname kind (ev, ov) =
      let msg = printf "function: '%s', %s variable" fname kind in
      assertEqual msg ev (var_unique ov)
        
-- utils {{{2
analyze :: String -> Analysis -- {{{3
analyze code = case analysis (enrich code) of
  Left es -> $abort $ show_errors "test" es
  Right x -> x

blurrSyntax :: String -> String -- {{{3
blurrSyntax code = reduce (enrich code :: CTranslUnit)

printOutputCode :: Analysis -> M.Map Symbol [CStat] -> String -- {{{3
printOutputCode ana items =
  let funs = M.elems $ M.intersectionWith replaceBody (anaCritical ana) items in
  reduce (CTranslUnit (map CFDefExt funs) undefNode)
  where
    replaceBody (CFunDef x1 x2 x3 (CCompound x4 _ x5) x6) ss =
      CFunDef (filter (not . isAttr) x1) x2 x3 (CCompound x4 (map CBlockStmt ss) x5) x6
      where
        isAttr (CTypeQual (CAttrQual _)) = True
        isAttr _                         = False
    replaceBody _ _ = $abort "function without body"

pipeline :: Analysis -> (CFunDef -> a) -> M.Map Symbol a -- {{{3
pipeline ana pipe = M.map pipe (anaCritical ana)

collectDeclarations :: CFunDef -> [CBlockItem] -- {{{4
collectDeclarations = (\(x, _, _) -> x) . collect_declarations

desugarControlStructures :: [CBlockItem] -> [CStat] -- {{{4
desugarControlStructures = desugar_control_structures

booleanShortCircuiting :: Analysis -> [CStat] -> [CStat] -- {{{4
booleanShortCircuiting ana = fst . boolean_short_circuiting (criticalFunctions ana)

normalizeCriticalCalls :: Analysis -> [CStat] -> [CStat] -- {{{4
normalizeCriticalCalls ana = fst . normalize_critical_calls (returnTypes ana)

buildBasicBlocks :: Analysis -> [CStat] -> (Label, Body) -- {{{4
buildBasicBlocks ana = build_basic_blocks (blockingAndCriticalFunctions ana)

optimizeIr :: (Label, Body) -> (Label, Body) -- {{{4
optimizeIr = optimize_ir

-- preparation {{{3
returnTypes :: Analysis -> M.Map Symbol (CTypeSpec, [CDerivedDeclr]) -- {{{4
returnTypes = M.union <$> M.map return_type_fd . anaCritical <*> M.map return_type_cd . anaBlocking

criticalFunctions :: Analysis -> S.Set Symbol -- {{{4
criticalFunctions = M.keysSet . anaCritical

blockingAndCriticalFunctions :: Analysis -> S.Set Symbol -- {{{4
blockingAndCriticalFunctions = S.union <$> M.keysSet . anaCritical <*> M.keysSet . anaBlocking

-- old stuff -- {{{1
{-
test_collect_declarations :: Test -- {{{2
test_collect_declarations = enumTestGroup "collect_declarations" $ map runTest [
  -- , 01 - nothing to do {{{3
  ([lpaste|
01: int foo(int i) {
      return i;
03: }  
  |], [paste|
    int foo(int i) {
      return i;
    }
  |], [
      ("int i", "i", 1, 3)
  ], []) 
  , -- 02 - local variable with initializer {{{3
  ([lpaste|
01: int foo(int i) {
      int j = 23;
      return i + j;
04: }
  |], [paste|
    int foo(int i) {
      j = 23;
      return i + j;
    }
  |], [
      ("int i", "i", 1, 4)
    , ("int j", "j", 1, 4)
  ], [])
  , -- 03 - local variable shadowing {{{3
  ([lpaste|
01: void foo() {
      int i = 23;
03:   {
        char i = 42;
05:   }
06: }
  |], [paste|
    void foo() {
      i = 23;
      {
        ec_unique_i_0 = 42;
      }
    }
  |], [
      ("char i", "ec_unique_i_0", 3, 5)
    , ("int i", "i", 1, 6)
  ], [])
  , -- 04 - local variable shadowing - with access {{{3
  ([lpaste|
01: void foo() {
      int i = 23;
03:   {
        char i = 42;
        i = 19;
06:   }
07: }
  |], [paste|
    void foo() {
      i = 23;
      {
        ec_unique_i_0 = 42;
        ec_unique_i_0 = 19;
      }
    }
  |], [
      ("char i", "ec_unique_i_0", 3, 6)
    , ("int i", "i", 1, 7)
  ], [])
  , -- 05 - for loop with declaration {{{3
  ([lpaste|
01: void foo(int i) {
02:   for (int i = 0; i < 23; i++) {
03:   }
04: } 
  |], [paste|
    void foo(int i) {
      {
        ec_unique_i_0 = 0;
        for (; ec_unique_i_0 < 23; ec_unique_i_0++) { }
      }
    }
  |], [
      ("int i", "i", 1, 4)
    , ("int i", "ec_unique_i_0", 2, 3)
  ], [])
  , -- 06 - multiple declarations {{{3
  ([lpaste|
01: int foo() {
      int i=0, j=1;
03:   {
        int i=23, j=42;
        return i + j;
06:   }
07: }
  |], [paste|
    int foo() {
      i = 0;
      j = 1;
      {
        ec_unique_i_0 = 23;
        ec_unique_j_0 = 42;
        return ec_unique_i_0 + ec_unique_j_0;
      }
    }
  |], [
      ("int i", "ec_unique_i_0", 3, 6)
    , ("int j", "ec_unique_j_0", 3, 6)
    , ("int i", "i", 1, 7)
    , ("int j", "j", 1, 7)
  ], [])
  , -- 07 - static variable with initializer {{{3
  ([lpaste|
01: int foo(int i) {
      static int j = 23;
      return i + j;
04: }
  |], [paste|
    int foo(int i) {
      return i + ec_static_foo_j;
    }
  |], [
      ("int i", "i", 1, 4)
  ], [
      ("static int j = 23", "ec_static_foo_j", 1, 4)
  ])
  , -- 08 - static variable shadowing {{{3
  ([lpaste|
01: void foo() {
      static int i = 23;
03:   {
        static int i = 42;
05:   }
06: }
  |], [paste|
    void foo() {
      {
      }
    }
  |], [], [
      ("static int i = 42", "ec_static_foo_ec_unique_i_0", 3, 5)
    , ("static int i = 23", "ec_static_foo_i", 1, 6)
  ])
  , -- 09 - static variable shadowing - with access {{{3
  ([lpaste|
01: void foo() {
      static int i = 23;
03:   {
        static int i = 42;
        i = 19;
06:   }
07: }
  |], [paste|
    void foo() {
      {
        ec_static_foo_ec_unique_i_0 = 19;
      }
    }
  |], [], [
      ("static int i = 42", "ec_static_foo_ec_unique_i_0", 3, 6)
    , ("static int i = 23", "ec_static_foo_i", 1, 7)
  ])
  , -- 10 - multiple declarations mixed {{{3
  ([lpaste|
01: int foo() {
      static int i=0, j=1;
03:   {
        int i=23, j=42;
        return i + j;
06:   }
07: }
  |], [paste|
    int foo() {
      {
        ec_unique_i_0 = 23;
        ec_unique_j_0 = 42;
        return ec_unique_i_0 + ec_unique_j_0;
      }
    }
  |], [
      ("int i", "ec_unique_i_0", 3, 6)
    , ("int j", "ec_unique_j_0", 3, 6)
  ], [
      ("static int i = 0", "ec_static_foo_i", 1, 7)
    , ("static int j = 1", "ec_static_foo_j", 1, 7)
  ])
  , -- 11 - reuse without shadowing {{{3
  ([lpaste|
    int foo() {
02:   {
        int i = 0;
04:   }
05:   {
        int i = 1;
07:   }
    }
  |], [paste|
    int foo() {
      {
        i = 0;
      }
      {
        ec_unique_i_0 = 1;
      }
    }
  |], [
      ("int i", "ec_unique_i_0", 5, 7)
    , ("int i", "i", 2, 4)
  ], [])
  , -- 12 - substitution in initializer {{{3
  ([lpaste|
    void foo() {
02:   if (0) {
        int i = 0;
04:   } else {
        int i = 1;
        int size = bar(i);
07:   }
    }
  |], [paste|
    void foo()
    {
        if (0)
        {
            i = 0;
        }
        else
        {
            ec_unique_i_0 = 1;
            size = bar(ec_unique_i_0);
        }
    }
  |], [
      ("int size", "size", 4, 7)
    , ("int i", "ec_unique_i_0", 4, 7)
    , ("int i", "i", 2, 4)
  ], [])
  -- end {{{3
  ]
  where
    runTest :: (String, String, [(String, String, Int, Int)], [(String, String, Int, Int)]) -> Assertion -- {{{3
    runTest (inputCode, expectedCode, expectedAutoVars, expectedStaticVars) =
      let
        (CTranslUnit eds x7) = enrich inputCode :: CTranslUnit
        (CFDefExt fd@(CFunDef x1 x2 x3 (CCompound x4 _ x5) x6)) = last eds
        (outputBody, outputAutoVars, outputStaticVars) = collect_declarations fd
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputBody x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in do
        assertEqual "output code" expectedCode' outputCode

        assertEqual "number of auto variables" (length expectedAutoVars) (length outputAutoVars)
        mapM_ (uncurry (cmpVar "auto")) (zip expectedAutoVars outputAutoVars)

        assertEqual "number of static variables" (length expectedStaticVars) (length outputStaticVars)
        mapM_ (uncurry (cmpVar "static")) (zip expectedStaticVars outputStaticVars)

    cmpVar kind (tdecl, ename, start, end) var =
      let prefix = printf "%s,  variable '%s', " kind ename in
      do
        assertEqual (prefix ++ "T-code decl") tdecl $ (show . pretty . var_decl) var
        assertEqual (prefix ++ "E-code name") ename (var_unique var)
        assertEqual (prefix ++ "start of scope")  start ((posRow . posOfNode . $fromJust_s . var_scope) var)
        assertEqual (prefix ++ "end of scope") end ((posRow . fst . getLastTokenPos . $fromJust_s . var_scope) var)

test_desugar_control_structures:: Test -- {{{2
test_desugar_control_structures = enumTestGroup "desugar_control_structures" $ map runTest [
  -- , 01 - while loop {{{3
  ([paste|
      void foo() {
        a();
        while(1)
          g();
        
        b();
      }
  |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_0: ;
          if (! 1) goto ec_ctrlbl_1;
          g();
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
  |])
  , -- 02 - do loop {{{3
  ([paste|
      void foo() {
        a();
        do {
          g();
        } while(1);
        b();
      }
  |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_0: ;
          g();
          if (1) goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
  |])
  , -- 03 - for loop {{{3
  ([paste|
      void foo() {
        a();
        {
          i=0;
          for (; i<23; i++) {
            g(i);
          }
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          i = 0;
          {
            ec_ctrlbl_0: ;
            if (! (i<23)) goto ec_ctrlbl_1;
            g(i);
            i++;
            goto ec_ctrlbl_0;
            ec_ctrlbl_1: ;
          }
        }
        b();
      }
    |])
  , -- 04 - for loop - with declaration {{{3
  ([paste|
      void foo() {
        a();
        {
          i = 0;
          for (; i<23; i++) {
            g(i);
          }
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          i = 0;
          {
            ec_ctrlbl_0: ;
            if (! (i<23)) goto ec_ctrlbl_1;
            g(i);
            i++;
            goto ec_ctrlbl_0;
            ec_ctrlbl_1: ;
          }
        }
        b();
      }
    |])
  , -- 05 - for loop - no init expression {{{3
  ([paste|
      void foo() {
        a();
        for (; i<23; i++) {
          g(i);
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
    |])
  , -- 06 - for loop - no break condition {{{3
  ([paste|
      void foo() {
        a();
        {
          i = 0;
          for (; ; i++) {
            g(i);
          }
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          i = 0;
          {
            ec_ctrlbl_0: ;
            g(i);
            i++;
            goto ec_ctrlbl_0;
            ec_ctrlbl_1: ;
          }
        }
        b();
      }
    |])
  , -- 07 - for loop - no incr expression{{{3
  ([paste|
      void foo() {
        a();
        for (i = 0; i<23; ) {
          g(i);
        }
        b();
      }
    |], [paste|
      void foo() {
        a();
        {
          i = 0;
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
    |])
  , -- 08 - continue and break {{{3
  ([paste|
      void foo() {
        a();
        do {
          continue;
          g();
          break;
        } while(1);
        b();
      }
  |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_0: ;
          goto ec_ctrlbl_0;
          g();
          goto ec_ctrlbl_1;
          if (1) goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
  |])
  , -- 09 - nested {{{3
  ([paste|
      void foo() {
        a();
        while(1) {
          b();
          continue;
          c();
          do {
            d();
            continue;
            e();
            break;
            f();
          } while(23);
          g();
          break;
          h();
        }
        i();
      }
    |], [paste|
      void foo() {
        a();
        {
          ec_ctrlbl_0: ;
          if (!1) goto ec_ctrlbl_1;
          b();
          goto ec_ctrlbl_0;
          c();
          {
            ec_ctrlbl_2: ;
            d();
            goto ec_ctrlbl_2;
            e();
            goto ec_ctrlbl_3;
            f();
            if (23) goto ec_ctrlbl_2;
            ec_ctrlbl_3: ;
          }
          g();
          goto ec_ctrlbl_1;
          h();
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ; 
        }
        i(); 
      }
    |])
  , -- 10 - if statements {{{3
  ([paste|
    void foo() {
      if (1) {
        b();
      }
    } 
  |], [paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
        }
        ec_ctrlbl_1: ;
      }
    }
  |])
  , -- 11 - if statements with else block {{{3
  ([paste|
    void foo() {
      if (1) {
        b();
        return;
      } else {
        c();
        return;
      }
    } 
  |], [paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
          return;
          goto ec_ctrlbl_2;
        }
        {
          ec_ctrlbl_1: ;
          c();
          return;
        }
        ec_ctrlbl_2: ;
      }
    }
  |])
  , -- 12 - if statements with else if {{{3
  ([paste|
    void foo() {
      if (1) {
        b();
        return;
      } else if (2) {
        c();
        return;
      }
    } 
  |], [paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
          return;
          goto ec_ctrlbl_2;
        }
        {
          ec_ctrlbl_1: ;
          {
            if (2) {
              goto ec_ctrlbl_3;
            } else {
              goto ec_ctrlbl_4;
            }
            {
              ec_ctrlbl_3: ;
              c();
              return;
            }
            ec_ctrlbl_4: ;
          }
        }
        ec_ctrlbl_2: ;
      }
    }
  |])
  , -- 13 - chained else if {{{3
  ([paste|
    void foo() {
      if (0) {
      } else if(1) {
          a();
      } else if (2) {
          b();
      } else if (3) {
          c();
      } else {
      }
    }
  |], [paste|
    void foo() {
    {
      if (0) goto ec_ctrlbl_0; else goto ec_ctrlbl_1;
      {
        ec_ctrlbl_0: ;
        goto ec_ctrlbl_2;
      }
      {
        ec_ctrlbl_1: ;
        {
          if (1) goto ec_ctrlbl_3; else goto ec_ctrlbl_4;
          {
            ec_ctrlbl_3: ;
            a();
            goto ec_ctrlbl_5;
          }
          {
            ec_ctrlbl_4: ;
            {
              if (2) goto ec_ctrlbl_6; else goto ec_ctrlbl_7;
              {
                ec_ctrlbl_6: ;
                b();
                goto ec_ctrlbl_8;
              }
              {
                ec_ctrlbl_7: ;
                {
                  if (3) goto ec_ctrlbl_9; else goto ec_ctrlbl_10;
                  {
                    ec_ctrlbl_9: ;
                    c();
                    goto ec_ctrlbl_11;
                  }
                  {
                    ec_ctrlbl_10: ;
                  }
                  ec_ctrlbl_11: ;
                }
              }
              ec_ctrlbl_8: ;
              }
            }
            ec_ctrlbl_5: ;
            }
          }
        ec_ctrlbl_2: ;
      }
    }
  |])
  , -- 14 - switch statement {{{3
  ([paste|
    void foo(int i) {
      switch (i) {
        case 1: a(); b(); break;
        case 2: c(); d();
        case 3: e(); f(); return;
      }
    }
  |], [paste|
    void foo(int i) {
      {
        if (i==1) goto ec_ctrlbl_1;
        if (i==2) goto ec_ctrlbl_2;
        if (i==3) goto ec_ctrlbl_3;
        goto ec_ctrlbl_0;
        
        {
          ec_ctrlbl_1: ;
          a(); b();
          goto ec_ctrlbl_0;
        }
        {
          ec_ctrlbl_2: ;
          c(); d();
        }
        {
          ec_ctrlbl_3: ;
          e(); f(); return;
        }
        ec_ctrlbl_0: ;
      }
    }
  |])
  , -- 15 - switch statement with default {{{3
  ([paste|
    void foo(int i) {
      switch (i) {
        case 1: a(); b(); break;
        case 2: c(); d();
        default: e(); f();
      }
    }
  |], [paste|
    void foo(int i) {
      {
        if (i==1) goto ec_ctrlbl_1;
        if (i==2) goto ec_ctrlbl_2;
        goto ec_ctrlbl_3;
        
        {
          ec_ctrlbl_1: ;
          a(); b();
          goto ec_ctrlbl_0;
        }
        {
          ec_ctrlbl_2: ;
          c(); d();
        }
        {
          ec_ctrlbl_3: ;
          e(); f();
        }
        ec_ctrlbl_0: ;
      }
    }
  |])
  , -- 16 - regression test - do and if {{{3  
  ([paste|
    void foo() {
      if (0) {
      } else {
        do { if (!(0)) { debug_file = "app-tc.c"; debug_line = 66; debug_mark = 0xffff; } } while(0);
      }
    }
  |], [paste|
    void foo() {
      {
          if (0) goto ec_ctrlbl_0; else goto ec_ctrlbl_1;
          {
          ec_ctrlbl_0: ;
              goto ec_ctrlbl_2;
          }
          {
          ec_ctrlbl_1: ;
              {
              ec_ctrlbl_3: ;
                  {
                      if (!0) goto ec_ctrlbl_5; else goto ec_ctrlbl_6;
                      {
                      ec_ctrlbl_5: ;
                          debug_file = "app-tc.c";
                          debug_line = 66;
                          debug_mark = 0xffff;
                      }
                  ec_ctrlbl_6: ;
                  }
                  if (0)
                  {
                      goto ec_ctrlbl_3;
                  }
              ec_ctrlbl_4: ;
              }
          }
      ec_ctrlbl_2: ;
      }
    }
  |])
  -- end {{{3
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{3
    runTest (inputCode, expectedCode) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7) = enrich inputCode
        outputItems = desugar_control_structures inputItems
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in
        expectedCode' @=? outputCode
        
test_boolean_short_circuiting :: Test -- {{{2
test_boolean_short_circuiting = enumTestGroup "boolean_short_circuiting" $ map runTest [
  -- , 01 - no critical function {{{3
  ([],
  [paste|
    void foo() {
      if(g() || h()) ;
    }
  |], [paste|
    void foo() {
      if(g() || h()) ;
    }
  |], [])
  , -- 02 - critical function on left hand side, or expression {{{3
  (["g"],
  [paste|
    void foo() {
      if(g() || h()) ;
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        if (ec_bool_0) ;
      }
    }
  |], ["int ec_bool_0"])
  , -- 03 - critical function on right hand side, and expression {{{3
  (["h"],
  [paste|
    void foo() {
      if(g() && h()) ;
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (ec_bool_0) {
          ec_bool_0 = !!h();
        }
        if (ec_bool_0) ;
      }
    }
  |], ["int ec_bool_0"])
  , -- 04 - expression statement {{{3
  (["g"],
  [paste|
    void foo() {
      g() || h();
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        ec_bool_0;
      }
    }
  |], ["int ec_bool_0"])
  , -- 05 - switch statement {{{3
  (["g"],
  [paste|
    void foo() {
      switch(g() || h()) ;
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        switch(ec_bool_0) ;
      }
    }
  |], ["int ec_bool_0"])
  , -- 06 - return statement {{{3
  (["g"],
  [paste|
    int foo() {
      return (g() || h()) ;
    }
  |], [paste|
    int foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        return ec_bool_0;
      }
    }
  |], ["int ec_bool_0"])
  , -- 07 - function call {{{3
  (["g"],
  [paste|
    void foo() {
      h(k() || g());
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!k();
        if (! ec_bool_0) {
          ec_bool_0 = !!g();
        }
        h(ec_bool_0);
      }
    }
  |], ["int ec_bool_0"])
  , -- 08 - within algebraic expression {{{3
  (["g"],
  [paste|
    void foo() {
      if ((g() || 1) + 3);
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g();
        if (! ec_bool_0) {
          ec_bool_0 = !!1;
        }
        if (ec_bool_0 + 3);
      }
    }
  |], ["int ec_bool_0"])
  , -- 09 - containing algebraic expression {{{3
  (["g"],
  [paste|
    void foo() {
      if ((1+g()) || h());
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!(1 + g());
        if (! ec_bool_0) {
          ec_bool_0 = !!h();
        }
        if (ec_bool_0);
      }
    }
  |], ["int ec_bool_0"])
  , -- 10 - nested {{{3
  (["g1", "g2"],
  [paste|
    void foo() {
      if ((g1() || h1()) && (h2() || g2()));
    }
  |], [paste|
    void foo() {
      {
        ec_bool_0 = !!g1();
        if (! ec_bool_0) {
          ec_bool_0 = !!h1();
        }
        ec_bool_2 = !!ec_bool_0;
        if (ec_bool_2) {
          ec_bool_1 = !!h2();
          if (!ec_bool_1) {
            ec_bool_1 = !!g2();
          }
          ec_bool_2 = !!ec_bool_1;
        }
        if (ec_bool_2);
      }
    }
  |], [
      "int ec_bool_2"
    , "int ec_bool_1"
    , "int ec_bool_0"
  ])
  , -- 11 - generic case {{{3
  (["g", "h"],
  [paste|
      void foo() {
        if ((g() || (i = x(), 1)) && h());
      }
  |], [paste|
      void foo() {
        {
          ec_bool_0 = !!g();
          if (! ec_bool_0) {
            ec_bool_0 = !! (i = x(), 1);
          }
          ec_bool_1 = !!ec_bool_0;
          if (ec_bool_1) {
            ec_bool_1 = !!h();
          }
          if (ec_bool_1);
        }
      }
  |], [
      "int ec_bool_1"
    , "int ec_bool_0"
  ])
  ]
  where
    runTest :: ([String], String, String, [String]) -> Assertion -- {{{3
    runTest (cf, inputCode, expectedCode, expectedDecls) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7)
           = enrich inputCode

        outputAst                           
            = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] (x7 :: NodeInfo)

        (outputItems, outputVariables) = boolean_short_circuiting (S.fromList cf) inputItems
        expectedCode'                       = reduce $ (enrich expectedCode :: CTranslUnit) :: String
        outputCode                          = reduce $ outputAst
        outputDecls                         = map (show . pretty . var_decl) outputVariables
      in do
        expectedCode' @=? outputCode
        expectedDecls @=? outputDecls

test_sequencialize_body :: Test -- {{{2
test_sequencialize_body = enumTestGroup "sequencialize_body" $ map runTest [
  -- , 01 - while loop {{{3
  ([paste|
    void foo() {
      a();
      {
        ec_ctrlbl_0: ;
        if (! 1) goto ec_ctrlbl_1;
        g();
        goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      if (! 1) goto ec_ctrlbl_1;
      g();
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 02 - do loop {{{3
  ([paste|
    void foo() {
      a();
      {
        ec_ctrlbl_0: ;
        g();
        if (1) goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      g();
      if (1) goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 03 - for loop {{{3
  ([paste|
    void foo() {
      a();
      {
        i = 0;
        {
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      i = 0;
      ec_ctrlbl_0: ;
      if (! (i<23)) goto ec_ctrlbl_1;
      g(i);
      i++;
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 04 - for loop - with declaration {{{3
  ([paste|
    void foo() {
      a();
      {
        i = 0;
        {
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      i = 0;
      ec_ctrlbl_0: ;
      if (! (i<23)) goto ec_ctrlbl_1;
      g(i);
      i++;
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 05 - for loop - no init expression {{{3
  ([paste|
      void foo() {
        a();
        {
          ec_ctrlbl_0: ;
          if (! (i<23)) goto ec_ctrlbl_1;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
        b();
      }
  |], [paste|
      void foo() {
        a();
        ec_ctrlbl_0: ;
        if (! (i<23)) goto ec_ctrlbl_1;
        g(i);
        i++;
        goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
        b();
      }
  |])
  , -- 06 - for loop - no break condition {{{3
  ([paste|
    void foo() {
      a();
      {
        i = 0;
        {
          ec_ctrlbl_0: ;
          g(i);
          i++;
          goto ec_ctrlbl_0;
          ec_ctrlbl_1: ;
        }
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      i = 0;
      ec_ctrlbl_0: ;
      g(i);
      i++;
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 07 - for loop - no incr expression{{{3
  ([paste|
    void foo() {
      a();
      {
        i = 0;
        ec_ctrlbl_0: ;
        if (! (i<23)) goto ec_ctrlbl_1;
        g(i);
        goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      i = 0;
      ec_ctrlbl_0: ;
      if (! (i<23)) goto ec_ctrlbl_1;
      g(i);
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 08 - continue and break {{{3
  ([paste|
    void foo() {
      a();
      {
        ec_ctrlbl_0: ;
        goto ec_ctrlbl_0;
        g();
        goto ec_ctrlbl_1;
        if (1) goto ec_ctrlbl_0;
        ec_ctrlbl_1: ;
      }
      b();
    }
  |], [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      goto ec_ctrlbl_0;
      g();
      goto ec_ctrlbl_1;
      if (1) goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |])
  , -- 09 - nested {{{3
  ([paste|
    void foo() {
      a();
      {
        ec_ctrlbl_0: ;
        if (!1) goto ec_ctrlbl_1;
        b();
        goto ec_ctrlbl_0;
        c();
        {
          ec_ctrlbl_2: ;
          d();
          goto ec_ctrlbl_2;
          e();
          goto ec_ctrlbl_3;
          f();
          if (23) goto ec_ctrlbl_2;
          ec_ctrlbl_3: ;
        }
        g();
        goto ec_ctrlbl_1;
        h();
        goto ec_ctrlbl_0;
        ec_ctrlbl_1: ; 
      }
      i(); 
    }
  |], [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      if (!1) goto ec_ctrlbl_1;
      b();
      goto ec_ctrlbl_0;
      c();
      ec_ctrlbl_2: ;
      d();
      goto ec_ctrlbl_2;
      e();
      goto ec_ctrlbl_3;
      f();
      if (23) goto ec_ctrlbl_2;
      ec_ctrlbl_3: ;
      g();
      goto ec_ctrlbl_1;
      h();
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ; 
      i(); 
    }
  |])
  , -- 10 - if statements {{{3
  ([paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
        }
        ec_ctrlbl_1: ;
      }
    }
  |], [paste|
    void foo() {
      if (1) goto ec_ctrlbl_0;
      else goto ec_ctrlbl_1;
      ec_ctrlbl_0: ;
      b();
      ec_ctrlbl_1: ;
    }
  |])
  , -- 11 - if statements with else block {{{3
  ([paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
          return;
          goto ec_ctrlbl_2;
        }
        {
          ec_ctrlbl_1: ;
          c();
          return;
        }
        ec_ctrlbl_2: ;
      }
    }
  |], [paste|
    void foo() {
      if (1) goto ec_ctrlbl_0;
      else goto ec_ctrlbl_1;
      ec_ctrlbl_0: ;
      b();
      return;
      goto ec_ctrlbl_2;
      ec_ctrlbl_1: ;
      c();
      return;
      ec_ctrlbl_2: ;
    }
  |])
  , -- 12 - if statement with else if {{{3
  ([paste|
    void foo() {
      {
        if (1) {
          goto ec_ctrlbl_0;
        } else {
          goto ec_ctrlbl_1;
        }
        {
          ec_ctrlbl_0: ;
          b();
          return;
          goto ec_ctrlbl_2;
        }
        {
          ec_ctrlbl_1: ;
          {
            if (2) {
              goto ec_ctrlbl_3;
            } else {
              goto ec_ctrlbl_4;
            }
            {
              ec_ctrlbl_3: ;
              c();
              return;
            }
            ec_ctrlbl_4: ;
          }
        }
        ec_ctrlbl_2: ;
      }
    }
  |], [paste|
    void foo() {
      if (1) goto ec_ctrlbl_0; else goto ec_ctrlbl_1;
      ec_ctrlbl_0: ;
      b();
      return;
      goto ec_ctrlbl_2;
      ec_ctrlbl_1: ;
      if (2) goto ec_ctrlbl_3; else goto ec_ctrlbl_2;
      ec_ctrlbl_3: ;
      c();
      return;
      ec_ctrlbl_2: ;
    }
  |])
  , -- 13 - chained else if {{{3
  ([paste|
    void foo() {
    {
      if (0) goto ec_ctrlbl_0; else goto ec_ctrlbl_1;
      {
        ec_ctrlbl_0: ;
        goto ec_ctrlbl_2;
      }
      {
        ec_ctrlbl_1: ;
        {
          if (1) goto ec_ctrlbl_3; else goto ec_ctrlbl_4;
          {
            ec_ctrlbl_3: ;
            a();
            goto ec_ctrlbl_5;
          }
          {
            ec_ctrlbl_4: ;
            {
              if (2) goto ec_ctrlbl_6; else goto ec_ctrlbl_7;
              {
                ec_ctrlbl_6: ;
                b();
                goto ec_ctrlbl_8;
              }
              {
                ec_ctrlbl_7: ;
                {
                  if (3) goto ec_ctrlbl_9; else goto ec_ctrlbl_10;
                  {
                    ec_ctrlbl_9: ;
                    c();
                    goto ec_ctrlbl_11;
                  }
                  {
                    ec_ctrlbl_10: ;
                  }
                  ec_ctrlbl_11: ;
                }
              }
              ec_ctrlbl_8: ;
              }
            }
            ec_ctrlbl_5: ;
            }
          }
        ec_ctrlbl_2: ;
      }
    }
  |], [paste|
    void foo() {
      if (0) goto ec_ctrlbl_0; else goto ec_ctrlbl_1;

      ec_ctrlbl_0: ;
      goto ec_ctrlbl_2;

      ec_ctrlbl_1: ;
      if (1) goto ec_ctrlbl_3; else goto ec_ctrlbl_4;

      ec_ctrlbl_3: ;
      a();
      goto ec_ctrlbl_2;

      ec_ctrlbl_4: ;
      if (2) goto ec_ctrlbl_6; else goto ec_ctrlbl_7;

      ec_ctrlbl_6: ;
      b();
      goto ec_ctrlbl_2;

      ec_ctrlbl_7: ;
      if (3) goto ec_ctrlbl_9; else goto ec_ctrlbl_2;

      ec_ctrlbl_9: ;
      c();
      goto ec_ctrlbl_2;

      ec_ctrlbl_2: ;
    }
  |])
  , -- 14 - switch statement {{{3
  ([paste|
    void foo(int i) {
      {
        if (i==1) goto ec_ctrlbl_1;
        if (i==2) goto ec_ctrlbl_2;
        if (i==3) goto ec_ctrlbl_3;
        
        {
          ec_ctrlbl_1: ;
          a(); b();
          goto ec_ctrlbl_0;
        }
        {
          ec_ctrlbl_2: ;
          c(); d();
        }
        {
          ec_ctrlbl_3: ;
          e(); f(); return;
        }
        ec_ctrlbl_0: ;
      }
    }
  |], [paste|
    void foo(int i) {
      if (i==1) goto ec_ctrlbl_1;
      if (i==2) goto ec_ctrlbl_2;
      if (i==3) goto ec_ctrlbl_3;
      ec_ctrlbl_1: ;
      a(); b();
      goto ec_ctrlbl_0;
      ec_ctrlbl_2: ;
      c(); d();
      ec_ctrlbl_3: ;
      e(); f(); return;
      ec_ctrlbl_0: ;
    }
  |])
  , -- 15 - switch statement with default {{{3
  ([paste|
    void foo(int i) {
      {
        if (i==1) goto ec_ctrlbl_1;
        if (i==2) goto ec_ctrlbl_2;
        goto ec_ctrlbl_3;
        
        {
          ec_ctrlbl_1: ;
          a(); b();
          goto ec_ctrlbl_0;
        }
        {
          ec_ctrlbl_2: ;
          c(); d();
        }
        {
          ec_ctrlbl_3: ;
          e(); f();
        }
        ec_ctrlbl_0: ;
      }
    }
  |], [paste|
    void foo(int i) {
      if (i==1) goto ec_ctrlbl_1;
      if (i==2) goto ec_ctrlbl_2;
      goto ec_ctrlbl_3;
      ec_ctrlbl_1: ;
      a(); b();
      goto ec_ctrlbl_0;
      ec_ctrlbl_2: ;
      c(); d();
      ec_ctrlbl_3: ;
      e(); f();
      ec_ctrlbl_0: ;
    }
  |])
  , -- 16 - empty statements -- {{{3
  ([paste|
    void foo() {
      i++;
      ;
      {
        i*=2;
        ;
      }
      i/=2;
      {
        ;
      }
      i--;
    }
  |], [paste|
    void foo () {
      i++;
      i*=2;
      i/=2;
      i--;
    }
  |])
  , -- 17 - regression test {{{3
  ([paste|
    void foo() {
      {
          if (23) goto ec_ctrlbl_0; else goto ec_ctrlbl_1;
          {
          ec_ctrlbl_0: ;
              goto ec_ctrlbl_2;
          }
          {
          ec_ctrlbl_1: ;
              {
              ec_ctrlbl_3: ;
                  {
                      if (!0) goto ec_ctrlbl_5; else goto ec_ctrlbl_6;
                      {
                      ec_ctrlbl_5: ;
                          debug_file = "app-tc.c";
                          debug_line = 66;
                          debug_mark = 0xffff;
                      }
                  ec_ctrlbl_6: ;
                  }
                  if (0)
                  {
                      goto ec_ctrlbl_3;
                  }
              ec_ctrlbl_4: ;
              }
          }
      ec_ctrlbl_2: ;
      }
    }
  |], [paste|
    void foo() {
        if (23) goto ec_ctrlbl_0; else goto ec_ctrlbl_3;

      ec_ctrlbl_0: ;
        goto ec_ctrlbl_2;

      ec_ctrlbl_3: ;
        if (!0) goto ec_ctrlbl_5; else goto ec_ctrlbl_6;

      ec_ctrlbl_5: ;
        debug_file = "app-tc.c";
        debug_line = 66;
        debug_mark = 0xffff;

      ec_ctrlbl_6: ;
        if (0) goto ec_ctrlbl_3;
      ec_ctrlbl_2: ;
    }
  |])
  -- end {{{3
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{3
    runTest (inputCode, expectedCode) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7) = enrich inputCode
        outputItems = map CBlockStmt $ sequencialize_body inputItems
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in
        expectedCode' @=? outputCode

test_normalize_critical_calls :: Test -- {{{2
test_normalize_critical_calls = enumTestGroup "normalize_critical_calls" $ map runTest [
  -- , 01 - critical call in return statement {{{3
  ([paste|
    int foo() {
      return bar() + 23;
    } 

    int bar() { return 0; }
  |], [paste|
    int foo() {
      ec_crit_0 = bar();
      return ec_crit_0 + 23;
    }
  |], [
      "int ec_crit_0"
  ])
  , -- 02 - critical call in condition of if statement {{{3
  ([paste|
    int foo() {
      if (bar() == 'a') return 0; else return 1;
    } 

    char bar() { return 0; }
  |], [paste|
    int foo() {
      ec_crit_0 = bar();
      if (ec_crit_0 == 'a') return 0; else return 1;
    }
  |], [
      "char ec_crit_0"
  ])
  , -- 03 - critical call in nested expressions {{{3
  ([paste|
    void foo() {
      i = bar() + 23;
    } 

    double bar() { return 0; }
  |], [paste|
    void foo() {
      ec_crit_0 = bar();
      i = ec_crit_0 + 23;
    }
  |], [
      "double ec_crit_0"
  ])
  , -- 04 - empty function {{{3
  ("void foo(){ }", "void foo() { }", [])
  , -- 05 - first normal form {{{3
  ([paste|
    void foo() {
      g();
    }
    void g() { }
  |], [paste|
    void foo() {
      g();
    }
  |], [])  
  , -- 06 - second normal form {{{3
  ([paste|
    void foo() {
      i = g();
    }
    void g() { }
  |], [paste|
    void foo() {
      i = g();
    }
  |], [])  
  -- end {{{3
  ]
  where
    runTest :: (String, String, [String]) -> Assertion -- {{{3
    runTest (inputCode, expectedCode, expectedDecls) =
      let
        (CTranslUnit eds y) = enrich inputCode :: CTranslUnit
        (fd:fds) = map unwrapFd eds
        sf       = M.fromList $ zip (map symbol fds) (map return_type_fd fds)
        (CFunDef x1 x2 x3 (CCompound x4 bitems x5) x6) = fd
        inputItems = map unwrapB bitems
        (outputItems, outputVariables) = normalize_critical_calls sf inputItems
        outputFd = CFunDef x1 x2 x3 (CCompound x4 (map CBlockStmt outputItems) x5) x6
        outputAst = CTranslUnit [CFDefExt outputFd] y
        outputCode = reduce outputAst
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputDecls = map (show . pretty . var_decl) outputVariables
      in do
        expectedCode' @=? outputCode
        expectedDecls @=? outputDecls

    unwrapFd (CFDefExt fd) = fd
    unwrapFd _             = error "unwrapFd"

    unwrapB (CBlockStmt s) = s
    unwrapB _              = error "unwrapB"

test_build_basic_blocks :: Test -- {{{2
test_build_basic_blocks = enumTestGroup "build_basic_blocks" $ map runTest [
  -- , 01 - just return {{{3
  ([],
  [paste|
    void foo() {
      return;      
    }
  |], [paste|
    L1:
    RETURN
  |], "L1")
  , -- 02 - while loop {{{3
  ([],
  [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      if (! 1) goto ec_ctrlbl_1;
      g();
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |], [paste|
    L1:
    a();
    GOTO L2/ec_ctrlbl_0

    L2/ec_ctrlbl_0:
    IF !1 THEN L4/ec_ctrlbl_1 ELSE L3

    L3:
    g();
    GOTO L2/ec_ctrlbl_0

    L4/ec_ctrlbl_1:
    b();
    RETURN
  |], "L1")
  , -- 03 - do loop {{{3
  ([], 
  [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      g();
      if (1) goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |], [paste|
    L1:
    a();
    GOTO L2/ec_ctrlbl_0

    L2/ec_ctrlbl_0:
    g();
    IF 1 THEN L2/ec_ctrlbl_0 ELSE L3/ec_ctrlbl_1

    L3/ec_ctrlbl_1:
    b();
    RETURN
  |], "L1")
  , -- 04 - continue and break {{{3
  ([],
  [paste|
    void foo() {
      a();
      ec_ctrlbl_0: ;
      goto ec_ctrlbl_0;
      g();
      goto ec_ctrlbl_1;
      if (1) goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      b();
    }
  |], [paste|
    L1:
    a();
    GOTO L2/ec_ctrlbl_0

    L2/ec_ctrlbl_0:
    GOTO L2/ec_ctrlbl_0

    L3:
    g();
    GOTO L5/ec_ctrlbl_1

    L4:
    IF 1 THEN L2/ec_ctrlbl_0 ELSE L5/ec_ctrlbl_1
 
    L5/ec_ctrlbl_1:
    b();
    RETURN
  |], "L1")
  , -- 05 - switch statement {{{3
  ([],
  [paste|
    void foo(int i) {
      if (i==1) goto ec_ctrlbl_1;
      if (i==2) goto ec_ctrlbl_2;
      if (i==3) goto ec_ctrlbl_3;
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
      a(); b();
      goto ec_ctrlbl_0;
      ec_ctrlbl_2: ;
      c(); d();
      ec_ctrlbl_3: ;
      e(); f(); return;
      ec_ctrlbl_0: ;
    }
  |], [paste|
    L1:
    IF i == 1 THEN L5/ec_ctrlbl_1 ELSE L2

    L2:
    IF i == 2 THEN L6/ec_ctrlbl_2 ELSE L3
 
    L3:
    IF i == 3 THEN L7/ec_ctrlbl_3 ELSE L4

    L4:
    GOTO L8/ec_ctrlbl_0

    L5/ec_ctrlbl_1:
    a();
    b();
    GOTO L8/ec_ctrlbl_0

    L6/ec_ctrlbl_2:
    c();
    d();
    GOTO L7/ec_ctrlbl_3

    L7/ec_ctrlbl_3:
    e();
    f();
    RETURN

    L8/ec_ctrlbl_0:
    RETURN
  |], "L1")
  , -- 06 - critical call {{{3
  (["g"],
  [paste|
    void foo() {
      ec_ctrlbl_0: ;
      if (! 1) goto ec_ctrlbl_1;
      g();
      goto ec_ctrlbl_0;
      ec_ctrlbl_1: ;
    }
  |], [paste| 
    L1/ec_ctrlbl_0:
    IF !1 THEN L4/ec_ctrlbl_1 ELSE L2
    
    L2:
    g(); GOTO L3

    L3:
    GOTO L1/ec_ctrlbl_0

    L4/ec_ctrlbl_1:
    RETURN
  |], "L1/ec_ctrlbl_0")
  , -- 07 - trailing critical call {{{3
  (["g"],
  [paste|
    void foo() {
      g();
    }
  |], [paste|
    L1:
    g(); GOTO L2
    
    L2:
    RETURN
  |], "L1")
  , -- 08 - trailing if {{{3
  (["g"],
  [paste|
    void foo() {
      ec_ctrlbl_0: ;
      g();
      if (1) goto ec_ctrlbl_0;
    }
  |], [paste|
    L1/ec_ctrlbl_0:
    g(); GOTO L2

    L2:
    IF 1 THEN L1/ec_ctrlbl_0 ELSE L3

    L3:
    RETURN
  |], "L1/ec_ctrlbl_0")
  , -- 09 - trailing statement {{{3
  (["g"],
  [paste|
    void foo() {
      g();
      h();
    }
  |], [paste|
    L1:
    g(); GOTO L2

    L2:
    h();
    RETURN
  |], "L1")
  , -- 10 - first normal form {{{3
  (["g"],
  [paste|
    void foo() {
      g();
    }
  |], [paste|
    L1:
    g(); GOTO L2

    L2:
    RETURN
  |], "L1")
  , -- 11 - second normal form {{{3
  (["g"],
  [paste|
    void foo() {
      i = g();
    }
  |], [paste|
    L1:
    i = g(); GOTO L2

    L2:
    RETURN
  |], "L1")
  , -- 12 - dead code {{{3
  (["g"],
  [paste|
    void foo() {
      if (1) goto ec_ctrlbl_0;
      else goto ec_ctrlbl_1;
      ec_ctrlbl_0: ;
      b();
      return;
      goto ec_ctrlbl_2;
      ec_ctrlbl_1: ;
      c();
      return;
      ec_ctrlbl_2: ;
    }
  |], [paste|
    L1:
    IF 1 THEN L2/ec_ctrlbl_0 ELSE L4/ec_ctrlbl_1

    L2/ec_ctrlbl_0:
    b();
    RETURN

    L3:
    GOTO L5/ec_ctrlbl_2

    L4/ec_ctrlbl_1:
    c();
    RETURN

    L5/ec_ctrlbl_2:
    RETURN
  |], "L1")

  -- end {{{3
  ]
  where
    runTest :: ([String], String, String, String) -> Assertion -- {{{3
    runTest (cf, inputCode, expectedIr, expectedEntry) = 
      let
        (CTranslUnit [CFDefExt fd] _) = enrich inputCode :: CTranslationUnit NodeInfo
        (CFunDef _ _ _ (CCompound _ bitems _) _) = fd
        inputItems = map (\(CBlockStmt s) -> s) bitems
        (outputEntry, outputIr) = build_basic_blocks (S.fromList cf) inputItems
        expectedIr' = (unlines . drop 1 . map (drop 4) . lines) expectedIr
      in do
        expectedIr' @=? showGraph show outputIr
        expectedEntry @=? show outputEntry

test_critical_variables :: Test  -- {{{2
test_critical_variables = enumTestGroup "critical_variables" $ map runTest [
  -- , 01 - single critical variable {{{3
  ([paste|
    int foo() {
      int i = 0;
      g();
      return i;
    }

    void g() { }
  |], ["i"], [])
  , -- 02 - single non-critical variable {{{3
  ([paste|
    int foo() {
      for (int i=0; i<23; i++) ;
      g();
      return 23;
    }

    void g() { }
  |], [], ["i"])
  , -- 03 - single critical variable in loop {{{3
  ([paste|
    void foo() {
      for (int i=0; i<23; i++) {
        g();
      }
    }

    void g() { }
  |], ["i"], [])
  , -- 04 - both critical and non-critical variables {{{3
  ([paste|
    void foo() {
      int j;
      for (int i=0; i<23; i++) {
        g();
      }
      j = 23;
    }

    void g() { }
  |], ["i"], ["j"])
  , -- 05 - kill liveness {{{3
  ([paste|
    void foo() {
      int j = 23;
      g();
      j = 42;
      j++;
    }
    void g() { }
  |], [], ["j"]) 
  , -- 06 - don't reuse {{{3
  ([paste|
    void foo() {
      int j = 23;
      g();
    }
    void g() { }
  |], [], ["j"])
  , -- 07 - take pointer {{{3
  ([paste|
    void foo() {
      int i = 23;
      int* j = &i;
      g();
    }
    void g() { }
  |], ["i"], ["j"])
  , -- 08 - pointer to array element {{{3
  ([paste|
    void foo() {
      int a[23];
      int* j = a + 1;
      g();
    }
    void g() { }
   |], ["a"], ["j"])
  , -- 09 - function parameters are always critical {{{3
  ([paste|
    void foo(int i) {
      g();
      i = 23;
    }
    void g() { }
  |], ["i"], [])
  , -- 10 - second normal form {{{3
  ([paste|
    void foo() {
      int j = g(23);
    }
    int g(int i) { return 23; }
  |], [], ["j"])
  , -- 11 - critical call in if-condition {{{3
  ([paste|
    void foo() {
      int i;
      if (g()) {
        i = 23;
      } 
    }
    int g() { return 23; }
  |], [], ["i", "ec_crit_0"])
  -- end {{{3
  ]
  where
    runTest :: (String, [String], [String]) -> Assertion -- {{{3
    runTest (inputCode, expectedCriticalVars, expecedUncriticalVars) =
      let
        (CTranslUnit eds _) = enrich inputCode :: CTranslUnit
        (cds, fds) = partitionEithers $ map unwrap eds
        cf = M.fromList $ map (\fd -> (symbol fd, fd)) fds
        bf = M.fromList $ map (\cd -> (symbol cd, cd)) cds
        funs = ast_2_ir bf cf
        (Function outputCriticalVars outputUncriticalVars _ _ _ _) = $fromJust_s $ M.lookup ((symbol . head) fds) funs
        outputCriticalVars' = map var_unique outputCriticalVars
        outputUncriticalVars' = map var_unique outputUncriticalVars
      in do
        assertEqual "critical variables" expectedCriticalVars outputCriticalVars'
        assertEqual "uncritical variables" expecedUncriticalVars outputUncriticalVars'

    unwrap (CFDefExt fd) = Right fd
    unwrap (CDeclExt cd) = Left cd 
    unwrap _             = error "unwrapFd"
-}
