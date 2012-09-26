{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Ocram.Intermediate.Test (tests) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Compiler.Hoopl (showGraph)
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
import Ocram.Intermediate.SequencializeBody
import Ocram.Intermediate.Optimize
import Ocram.Symbols (Symbol)
import Ocram.Test.Lib (enumTestGroup, enrich, reduce, lpaste, paste)
import Ocram.Text (show_errors)
import Ocram.Util (fromJust_s, abort)
import Ocram.Query (return_type_fd, return_type_cd)
import Test.Framework (Test, testGroup)
import Test.HUnit (assertEqual, Assertion)
import Text.Printf (printf)

import qualified Data.Map as M
import qualified Data.Set as S

tests :: Test -- {{{1
tests = testGroup "Intermediate" [
    test_collect_declarations
  , test_desugar_control_structures
  , test_boolean_short_circuiting
  , test_sequencialize_body
  , test_normalize_critical_calls
  , test_build_basic_blocks
  , test_optimize_ir
  , test_critical_variables
  ]

-- types {{{1
type ScopedVariable = ( -- {{{2
    String   -- declaration
  , String   -- unique name
  , Int      -- first row of scope
  , Int      -- last row of scope
  )
  
type OutputCollectDeclarations = ( -- {{{2
    String                 -- code
  , [(
        String             -- critical function name
      , [ScopedVariable]   -- automatic variables
      , [ScopedVariable]   -- static variables
    )]
  )

type OutputDesugarControlStructures = -- {{{2
    String -- code

type OutputBooleanShortCircuiting = ( -- {{{2
    String   -- code
  , [(
      String    -- critical function name
    , [String]  -- declarations
    )]
  )

type OutputSequentialize = -- {{{2
    String -- code

type OutputNormalize = ( -- {{{2
    String   -- code
  , [(
      String   -- critical function name
    , [String] -- declarations
    )]
  )

type OutputBasicBlocks = -- {{{2
    [(
      String -- critical function name
    , String -- entry label
    , String -- intermediate representation
    )]

type OutputCriticalVariables = -- {{{2
  [(
      String   -- critical function name
    , [String] -- critical variables
    , [String] -- uncritical variables
  )] 

type OutputOptimize = OutputBasicBlocks

data TestCase = TestCase { -- {{{2
    input              :: String
  , outCollect         :: OutputCollectDeclarations
  , outDesugar         :: OutputDesugarControlStructures
  , outShortCircuit    :: OutputBooleanShortCircuiting
  , outSequence        :: OutputSequentialize
  , outNormalize       :: OutputNormalize
  , outBasicBlocks     :: OutputBasicBlocks
  , outOptimize        :: OutputOptimize
  , outCritical        :: OutputCriticalVariables 
  }

testCases :: [TestCase] -- {{{1
testCases = [
  -- , 01 - setup {{{2
  TestCase {input = -- {{{3
    [lpaste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        block();
      }
    |]
  , outCollect = ( -- {{{3
    [paste|
      void start() {
        block();
      }
    |], [("start", [], [])]
    )
  , outDesugar = -- {{{3
    [paste|
      void start() {
        block();
      }
    |]
  , outShortCircuit = ( -- {{{3
    [paste|
      void start() {
        block();
      }
    |], [("start", [])]
    )
  , outSequence = -- {{{3
    [paste|
      void start() {
        block();
      }
    |]
  , outNormalize = ( -- {{{3
    [paste|
      void start() {
        block();
      }
    |], [("start", [])]
    )
  , outBasicBlocks = [ -- {{{3
      ("start", "L1", [paste|
          L1:
          block(); GOTO L2

          L2:
          RETURN
      |])
    ]
  , outOptimize = [ -- {{{3
    ("start", "L1", [paste|
        L1:
        block(); GOTO L2

        L2:
        RETURN    
    |])
    ]
  , outCritical = [ -- {{{3
      ("start", [], [])
    ]
  }
  ]

test_collect_declarations :: Test -- {{{1
test_collect_declarations = enumTestGroup "collect_declarations" $ map runTest testCases
  where
    runTest testCase = do
      let
        (expectedCode, expectedVars') = outCollect testCase
        ana = analyze (input testCase)
        items = pipeline ana collectDeclarations
        outputCode = printOutputCode ana items
      assertEqual "output code" (blurrSyntax expectedCode) outputCode

      let
        expectedVars = M.fromList . map (\(x, y, z) -> (x, (y, z))) $ expectedVars'
        result = pipeline ana collect_declarations
      assertEqual "set of critical functions" (M.keysSet expectedVars) (M.keysSet result)
      sequence_ $ M.elems $ M.intersectionWithKey cmpVars expectedVars result
      
    cmpVars fname (av, sv) (_, av', sv') = do
      mapM_ (cmpVar fname "automatic") $ zip av av'
      mapM_ (cmpVar fname "static")    $ zip sv sv'

    cmpVar fname kind ((tdecl, ename, start, end), var) =
      let prefix = printf "function: '%s', %s variable: '%s'" fname kind ename in
      do
        assertEqual (prefix ++ "T-code decl") tdecl $ (show . pretty . var_decl) var
        assertEqual (prefix ++ "E-code name") ename (var_unique var)
        assertEqual (prefix ++ "start of scope")  start ((posRow . posOfNode . $fromJust_s . var_scope) var)
        assertEqual (prefix ++ "end of scope") end ((posRow . fst . getLastTokenPos . $fromJust_s . var_scope) var)

test_desugar_control_structures :: Test -- {{{1
test_desugar_control_structures = enumTestGroup "desugar_control_structures" $ map runTest testCases
  where
    runTest testCase =
      let
        expectedCode = outDesugar testCase
        ana = analyze (input testCase)
        items = pipeline ana (
            desugarControlStructures
          . collectDeclarations
          )
        outputCode = printOutputCode ana items 
      in
        assertEqual "output code" (blurrSyntax expectedCode) outputCode
        
test_boolean_short_circuiting :: Test -- {{{1
test_boolean_short_circuiting = enumTestGroup "boolean_short_circuiting" $ map runTest testCases
  where
    runTest testCase = do
      let
        (expectedCode, expectedDecls') = outShortCircuit testCase
        ana = analyze (input testCase)
        items = pipeline ana (
            booleanShortCircuiting ana
          . desugarControlStructures
          . collectDeclarations
          )
        outputCode = printOutputCode ana items
      assertEqual "output code" (blurrSyntax expectedCode) outputCode

      let
        expectedDecls = M.fromList expectedDecls' 
        result = pipeline ana (
            boolean_short_circuiting (criticalFunctions ana)
          . desugarControlStructures
          . collectDeclarations
          )
      assertEqual "set of critical functions" (M.keysSet expectedDecls) (M.keysSet result)
      sequence_ $ M.elems $ M.intersectionWithKey cmpVars expectedDecls result

    cmpVars fname decls (_, vars) = mapM_ (cmpVar fname) $ zip decls vars
    cmpVar fname (decl, var) = 
      let msg = printf "function: '%s', variable" fname in
      assertEqual msg decl ((show . pretty . var_decl) var)

test_sequencialize_body :: Test -- {{{1
test_sequencialize_body = enumTestGroup "sequencialize_body" $ map runTest testCases
  where
    runTest testCase =
      let
        expectedCode = outSequence testCase
        ana = analyze (input testCase)
        stmts = pipeline ana (
            sequencializeBody
          . booleanShortCircuiting ana
          . desugarControlStructures
          . collectDeclarations
          )
        outputCode = printOutputCode ana (M.map (map CBlockStmt) stmts)
      in
        assertEqual "output code" (blurrSyntax expectedCode) outputCode

test_normalize_critical_calls :: Test -- {{{1
test_normalize_critical_calls = enumTestGroup "normalize_critical_calls" $ map runTest testCases
  where
    runTest testCase = do
      let
        (expectedCode, expectedDecls') = outNormalize testCase
        ana = analyze (input testCase)
        stmts = pipeline ana (
            normalizeCriticalCalls ana
          . sequencializeBody
          . booleanShortCircuiting ana
          . desugarControlStructures
          . collectDeclarations
          )
        outputCode = printOutputCode ana (M.map (map CBlockStmt) stmts)
      assertEqual "output code" (blurrSyntax expectedCode) outputCode

      let
        expectedDecls = M.fromList expectedDecls'
        result = pipeline ana (
            normalize_critical_calls (returnTypes ana)
          . sequencializeBody
          . booleanShortCircuiting ana
          . desugarControlStructures
          . collectDeclarations
          )

      assertEqual "set of critical functions" (M.keysSet expectedDecls) (M.keysSet result)
      sequence_ $ M.elems $ M.intersectionWithKey cmpVars expectedDecls result

    cmpVars fname decls (_, vars) = mapM_ (cmpVar fname) $ zip decls vars
    cmpVar fname (decl, var) = 
      let msg = printf "function: '%s', variable" fname in
      assertEqual msg decl ((show . pretty . var_decl) var)

test_build_basic_blocks :: Test -- {{{1
test_build_basic_blocks = enumTestGroup "build_basic_blocks" $ map runTest testCases
  where
    runTest testCase = do
      let
        expectedIrs = M.fromList $ map (\(x, y, z) -> (x, (y, z))) $ outBasicBlocks testCase
        ana = analyze (input testCase)
        result = pipeline ana (
            buildBasicBlocks ana
          . normalizeCriticalCalls ana
          . sequencializeBody
          . booleanShortCircuiting ana
          . desugarControlStructures
          . collectDeclarations
          )
      assertEqual "set of critical functions (bodies)" (M.keysSet expectedIrs) (M.keysSet result)
      sequence_ $ M.elems $ M.intersectionWithKey cmp expectedIrs result

    cmp :: String -> (String, String) -> (Label, Body) -> Assertion
    cmp fname (eentry, ebody) (oentry, obody) =
      let
        prefix = printf "function: '%s', " fname
        ebody' = (unlines . drop 1 . map (drop 10) . lines) ebody
        obody' = showGraph show obody
      in do
        assertEqual (prefix ++ " entry") eentry (show oentry)
        assertEqual (prefix ++ " body") ebody' obody'
      
test_optimize_ir :: Test -- {{{1
test_optimize_ir = enumTestGroup "optimize_ir" $ map runTest testCases
  where
    runTest testCase = do
      let
        expectedIrs = M.fromList $ map (\(x, y, z) -> (x, (y, z))) $ outBasicBlocks testCase
        ana = analyze (input testCase)
        result = pipeline ana (
            optimizeIr
          . buildBasicBlocks ana
          . normalizeCriticalCalls ana
          . sequencializeBody
          . booleanShortCircuiting ana
          . desugarControlStructures
          . collectDeclarations
          )
      assertEqual "set of critical functions (bodies)" (M.keysSet expectedIrs) (M.keysSet result)
      sequence_ $ M.elems $ M.intersectionWithKey cmp expectedIrs result

    cmp :: String -> (String, String) -> (Label, Body) -> Assertion
    cmp fname (eentry, ebody) (oentry, obody) =
      let
        prefix = printf "function: '%s', " fname
        ebody' = (unlines . drop 1 . map (drop 10) . lines) ebody
        obody' = showGraph show obody
      in do
        assertEqual (prefix ++ " entry") eentry (show oentry)
        assertEqual (prefix ++ " body") ebody' obody'

test_critical_variables :: Test -- {{{1
test_critical_variables = enumTestGroup "critical_variables" $ map runTest testCases
  where
    runTest testCase = do
      let
        ana = analyze (input testCase)
        funs = ast_2_ir (anaBlocking ana) (anaCritical ana)
        expectedVars = M.fromList $ map (\(x, y, z) -> (x, (y, z))) (outCritical testCase)
      assertEqual "set of critical functions" (M.keysSet expectedVars) (M.keysSet funs)
      sequence_ $ M.elems $ M.intersectionWithKey cmpVars expectedVars funs

    cmpVars fname (ecs, eus) (Function ocs ous _ _ _ _) = do
      mapM_ (cmpVar fname "critical")   $ zip ecs ocs
      mapM_ (cmpVar fname "uncritical") $ zip eus ous

    cmpVar fname kind (ev, ov) =
      let msg = printf "function: '%s', %s variable" fname kind in
      assertEqual msg ev (var_unique ov)
        

-- utils {{{1
analyze :: String -> Analysis -- {{{2
analyze code = case analysis (enrich code) of
  Left es -> $abort $ show_errors "test" es
  Right x -> x

pipeline :: Analysis -> (CFunDef -> a) -> M.Map Symbol a -- {{{2
pipeline ana pipe = M.map pipe (anaCritical ana)

collectDeclarations :: CFunDef -> [CBlockItem] -- {{{3
collectDeclarations = (\(x, _, _) -> x) . collect_declarations

desugarControlStructures :: [CBlockItem] -> [CBlockItem] -- {{{3
desugarControlStructures = desugar_control_structures

booleanShortCircuiting :: Analysis -> [CBlockItem] -> [CBlockItem] -- {{{3
booleanShortCircuiting ana = fst . boolean_short_circuiting (criticalFunctions ana)

sequencializeBody :: [CBlockItem] -> [CStat]  -- {{{3
sequencializeBody = sequencialize_body

normalizeCriticalCalls :: Analysis -> [CStat] -> [CStat] -- {{{3
normalizeCriticalCalls ana = fst . normalize_critical_calls (returnTypes ana)

buildBasicBlocks :: Analysis -> [CStat] -> (Label, Body) -- {{{3
buildBasicBlocks ana = build_basic_blocks (blockingAndCriticalFunctions ana)

optimizeIr :: (Label, Body) -> (Label, Body) -- {{{3
optimizeIr = optimize_ir

-- preparation {{{2
returnTypes :: Analysis -> M.Map Symbol (CTypeSpec, [CDerivedDeclr]) -- {{{3
returnTypes = M.union <$> M.map return_type_fd . anaCritical <*> M.map return_type_cd . anaBlocking

criticalFunctions :: Analysis -> S.Set Symbol -- {{{3
criticalFunctions = M.keysSet . anaCritical

blockingAndCriticalFunctions :: Analysis -> S.Set Symbol -- {{{3
blockingAndCriticalFunctions = S.union <$> M.keysSet . anaCritical <*> M.keysSet . anaBlocking

blurrSyntax :: String -> String -- {{{2
blurrSyntax code = reduce (enrich code :: CTranslUnit)

printOutputCode :: Analysis -> M.Map Symbol [CBlockItem] -> String
printOutputCode ana items =
  let funs = M.elems $ M.intersectionWith replaceBody (anaCritical ana) items in
  reduce (CTranslUnit (map CFDefExt funs) undefNode)
  where
    replaceBody :: CFunDef -> [CBlockItem] -> CFunDef -- {{{2
    replaceBody (CFunDef x1 x2 x3 (CCompound x4 _ x5) x6) is =
      CFunDef (filter (not . isAttr) x1) x2 x3 (CCompound x4 is x5) x6
      where
        isAttr (CTypeQual (CAttrQual _)) = True
        isAttr _                         = False
    replaceBody _ _ = $abort "function without body"

{-
test_collect_declarations :: Test -- {{{1
test_collect_declarations = enumTestGroup "collect_declarations" $ map runTest [
  -- , 01 - nothing to do {{{2
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
  , -- 02 - local variable with initializer {{{2
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
  , -- 03 - local variable shadowing {{{2
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
  , -- 04 - local variable shadowing - with access {{{2
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
  , -- 05 - for loop with declaration {{{2
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
  , -- 06 - multiple declarations {{{2
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
  , -- 07 - static variable with initializer {{{2
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
  , -- 08 - static variable shadowing {{{2
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
  , -- 09 - static variable shadowing - with access {{{2
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
  , -- 10 - multiple declarations mixed {{{2
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
  , -- 11 - reuse without shadowing {{{2
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
  , -- 12 - substitution in initializer {{{2
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
  -- end {{{2
  ]
  where
    runTest :: (String, String, [(String, String, Int, Int)], [(String, String, Int, Int)]) -> Assertion -- {{{2
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
      let prefix = kind ++ " variable " ++ ename ++ ": " in
      do
        assertEqual (prefix ++ "T-code decl") tdecl $ (show . pretty . var_decl) var
        assertEqual (prefix ++ "E-code name") ename (var_unique var)
        assertEqual (prefix ++ "start of scope")  start ((posRow . posOfNode . $fromJust_s . var_scope) var)
        assertEqual (prefix ++ "end of scope") end ((posRow . fst . getLastTokenPos . $fromJust_s . var_scope) var)

test_desugar_control_structures:: Test -- {{{1
test_desugar_control_structures = enumTestGroup "desugar_control_structures" $ map runTest [
  -- , 01 - while loop {{{2
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
  , -- 02 - do loop {{{2
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
  , -- 03 - for loop {{{2
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
  , -- 04 - for loop - with declaration {{{2
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
  , -- 05 - for loop - no init expression {{{2
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
  , -- 06 - for loop - no break condition {{{2
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
  , -- 07 - for loop - no incr expression{{{2
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
  , -- 08 - continue and break {{{2
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
  , -- 09 - nested {{{2
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
  , -- 10 - if statements {{{2
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
  , -- 11 - if statements with else block {{{2
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
  , -- 12 - if statements with else if {{{2
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
  , -- 13 - chained else if {{{2
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
  , -- 14 - switch statement {{{2
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
  , -- 15 - switch statement with default {{{2
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
  , -- 16 - regression test - do and if {{{2  
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
  -- end {{{2
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{2
    runTest (inputCode, expectedCode) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7) = enrich inputCode
        outputItems = desugar_control_structures inputItems
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in
        expectedCode' @=? outputCode
        
test_boolean_short_circuiting :: Test -- {{{1
test_boolean_short_circuiting = enumTestGroup "boolean_short_circuiting" $ map runTest [
  -- , 01 - no critical function {{{2
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
  , -- 02 - critical function on left hand side, or expression {{{2
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
  , -- 03 - critical function on right hand side, and expression {{{2
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
  , -- 04 - expression statement {{{2
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
  , -- 05 - switch statement {{{2
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
  , -- 06 - return statement {{{2
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
  , -- 07 - function call {{{2
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
  , -- 08 - within algebraic expression {{{2
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
  , -- 09 - containing algebraic expression {{{2
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
  , -- 10 - nested {{{2
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
  , -- 11 - generic case {{{2
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
    runTest :: ([String], String, String, [String]) -> Assertion -- {{{2
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

test_sequencialize_body :: Test -- {{{1
test_sequencialize_body = enumTestGroup "sequencialize_body" $ map runTest [
  -- , 01 - while loop {{{2
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
  , -- 02 - do loop {{{2
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
  , -- 03 - for loop {{{2
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
  , -- 04 - for loop - with declaration {{{2
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
  , -- 05 - for loop - no init expression {{{2
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
  , -- 06 - for loop - no break condition {{{2
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
  , -- 07 - for loop - no incr expression{{{2
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
  , -- 08 - continue and break {{{2
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
  , -- 09 - nested {{{2
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
  , -- 10 - if statements {{{2
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
  , -- 11 - if statements with else block {{{2
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
  , -- 12 - if statement with else if {{{2
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
  , -- 13 - chained else if {{{2
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
  , -- 14 - switch statement {{{2
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
  , -- 15 - switch statement with default {{{2
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
  , -- 16 - empty statements -- {{{2
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
  , -- 17 - regression test {{{2
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
  -- end {{{2
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{2
    runTest (inputCode, expectedCode) =
      let
        (CTranslUnit [CFDefExt (CFunDef x1 x2 x3 (CCompound x4 inputItems x5) x6)] x7) = enrich inputCode
        outputItems = map CBlockStmt $ sequencialize_body inputItems
        outputAst = CTranslUnit [CFDefExt $ CFunDef x1 x2 x3 (CCompound x4 outputItems x5) x6] x7
        expectedCode' = reduce (enrich expectedCode :: CTranslUnit) :: String
        outputCode = reduce outputAst
      in
        expectedCode' @=? outputCode

test_normalize_critical_calls :: Test -- {{{1
test_normalize_critical_calls = enumTestGroup "normalize_critical_calls" $ map runTest [
  -- , 01 - critical call in return statement {{{2
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
  , -- 02 - critical call in condition of if statement {{{2
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
  , -- 03 - critical call in nested expressions {{{2
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
  , -- 04 - empty function {{{2
  ("void foo(){ }", "void foo() { }", [])
  , -- 05 - first normal form {{{2
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
  , -- 06 - second normal form {{{2
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
  -- end {{{2
  ]
  where
    runTest :: (String, String, [String]) -> Assertion -- {{{2
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

test_build_basic_blocks :: Test -- {{{1
test_build_basic_blocks = enumTestGroup "build_basic_blocks" $ map runTest [
  -- , 01 - just return {{{2
  ([],
  [paste|
    void foo() {
      return;      
    }
  |], [paste|
    L1:
    RETURN
  |], "L1")
  , -- 02 - while loop {{{2
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
  , -- 03 - do loop {{{2
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
  , -- 04 - continue and break {{{2
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
  , -- 05 - switch statement {{{2
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
  , -- 06 - critical call {{{2
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
  , -- 07 - trailing critical call {{{2
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
  , -- 08 - trailing if {{{2
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
  , -- 09 - trailing statement {{{2
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
  , -- 10 - first normal form {{{2
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
  , -- 11 - second normal form {{{2
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
  , -- 12 - dead code {{{2
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

  -- end {{{2
  ]
  where
    runTest :: ([String], String, String, String) -> Assertion -- {{{2
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

test_critical_variables :: Test  -- {{{1
test_critical_variables = enumTestGroup "critical_variables" $ map runTest [
  -- , 01 - single critical variable {{{2
  ([paste|
    int foo() {
      int i = 0;
      g();
      return i;
    }

    void g() { }
  |], ["i"], [])
  , -- 02 - single non-critical variable {{{2
  ([paste|
    int foo() {
      for (int i=0; i<23; i++) ;
      g();
      return 23;
    }

    void g() { }
  |], [], ["i"])
  , -- 03 - single critical variable in loop {{{2
  ([paste|
    void foo() {
      for (int i=0; i<23; i++) {
        g();
      }
    }

    void g() { }
  |], ["i"], [])
  , -- 04 - both critical and non-critical variables {{{2
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
  , -- 05 - kill liveness {{{2
  ([paste|
    void foo() {
      int j = 23;
      g();
      j = 42;
      j++;
    }
    void g() { }
  |], [], ["j"]) 
  , -- 06 - don't reuse {{{2
  ([paste|
    void foo() {
      int j = 23;
      g();
    }
    void g() { }
  |], [], ["j"])
  , -- 07 - take pointer {{{2
  ([paste|
    void foo() {
      int i = 23;
      int* j = &i;
      g();
    }
    void g() { }
  |], ["i"], ["j"])
  , -- 08 - pointer to array element {{{2
  ([paste|
    void foo() {
      int a[23];
      int* j = a + 1;
      g();
    }
    void g() { }
   |], ["a"], ["j"])
  , -- 09 - function parameters are always critical {{{2
  ([paste|
    void foo(int i) {
      g();
      i = 23;
    }
    void g() { }
  |], ["i"], [])
  , -- 10 - second normal form {{{2
  ([paste|
    void foo() {
      int j = g(23);
    }
    int g(int i) { return 23; }
  |], [], ["j"])
  , -- 11 - critical call in if-condition {{{2
  ([paste|
    void foo() {
      int i;
      if (g()) {
        i = 23;
      } 
    }
    int g() { return 23; }
  |], [], ["i", "ec_crit_0"])
  -- end {{{2
  ]
  where
    runTest :: (String, [String], [String]) -> Assertion -- {{{2
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
