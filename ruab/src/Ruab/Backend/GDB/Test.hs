{-# LANGUAGE QuasiQuotes #-}
module Ruab.Backend.GDB.Test (tests) where

-- imports {{{1
import Ruab.Backend.GDB.Representation
import Ruab.Backend.GDB.Responses
import Ruab.Test.Lib (enumTestGroup, paste)
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), Assertion)

tests :: Test -- {{{1
tests = testGroup "GDB" [
        test_render_command
      , test_parse_output
      , test_response_break_insert
      , test_response_stopped
      , test_response_stack_list_frames
      , test_response_exec_return
      , test_response_evaluate_expression
      , test_response_error
      ]

test_render_command:: Test -- {{{2
test_render_command = enumTestGroup "render_command" $ map runTest [
    (
      MICommand Nothing "break-info" [] []
    , "-break-info\n"
    ), (
      MICommand (Just 23) "exec-arguments" [Option (qp "-v") (Just (qp "word"))] []
    , "23-exec-arguments \"-v\" \"word\"\n"
    ), (
      MICommand (Just 42) "break-commands" [Option (qp "1") Nothing, Option (qp "print v") Nothing] []
      , "42-break-commands \"1\" \"print v\"\n"
    )
  ]
  where
    runTest :: (Command, String) -> Assertion
    runTest (cmd, expected) = expected @=? render_command cmd

test_parse_output :: Test -- {{{2
test_parse_output = enumTestGroup "parse_output" $ map runTest [
    -- welcome text {{{3
    ([paste|
=thread-group-added,id="i1"
~"GNU gdb (GDB) 7.2-ubuntu\n"
~"Copyright (C) 2010 Free Software Foundation, Inc.\n"
~"License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.  Type \"show copying\"\nand \"show warranty\" for details.\n"
~"This GDB was configured as \"x86_64-linux-gnu\".\nFor bug reporting instructions, please see:\n"
~"<http://www.gnu.org/software/gdb/bugs/>...\n"
~"Reading symbols from /home/alex/scm/ocram/applications/simulation_os/collect-and-forward/tc.elf..."
~"done.\n"
(gdb) 
|], Output ([
        OOBAsyncRecord $ ARNotifyAsyncOutput $ NotifyAsyncOutput Nothing $ AsyncOutput ACThreadGroupAdded [Result "id" (VConst "i1")]
    ] ++ map (OOBStreamRecord . SRConsoleStreamOutput . ConsoleStreamOutput) [
        "GNU gdb (GDB) 7.2-ubuntu\n"
      , "Copyright (C) 2010 Free Software Foundation, Inc.\n"
      , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.  Type \"show copying\"\nand \"show warranty\" for details.\n"
      , "This GDB was configured as \"x86_64-linux-gnu\".\nFor bug reporting instructions, please see:\n"
      , "<http://www.gnu.org/software/gdb/bugs/>...\n"
      , "Reading symbols from /home/alex/scm/ocram/applications/simulation_os/collect-and-forward/tc.elf..."
      , "done.\n"
    ])
    Nothing)
  , -- command result -break-info {{{3
    ([paste|
^done,BreakpointTable={nr_rows="0",nr_cols="6",hdr=[{width="7",alignment="-1",col_name="number",colhdr="Num"},{width="14",alignment="-1",col_name="type",colhdr="Type"},{width="4",alignment="-1",col_name="disp",colhdr="Disp"},{width="3",alignment="-1",col_name="enabled",colhdr="Enb"},{width="10",alignment="-1",col_name="addr",colhdr="Address"},{width="40",alignment="2",col_name="what",colhdr="What"}],body=[]}
(gdb) 
|], Output [] $ Just $ ResultRecord Nothing RCDone [
        Result "BreakpointTable" $ VTuple $ Tuple [
            Result "nr_rows" $ VConst "0"
          , Result "nr_cols" $ VConst "6"
          , Result "hdr" $ VList $ ValueList [
                VTuple $ Tuple [
                    Result "width" $ VConst "7"
                  , Result "alignment" $ VConst "-1"
                  , Result "col_name" $ VConst "number"
                  , Result "colhdr" $ VConst "Num"
                  ]
              , VTuple $ Tuple [
                    Result "width" $ VConst "14"
                  , Result "alignment" $ VConst "-1"
                  , Result "col_name" $ VConst "type"
                  , Result "colhdr" $ VConst "Type"
                  ]
              , VTuple $ Tuple [
                    Result "width" $ VConst "4"
                  , Result "alignment" $ VConst "-1"
                  , Result "col_name" $ VConst "disp"
                  , Result "colhdr" $ VConst "Disp"
                  ]
              , VTuple $ Tuple [
                    Result "width" $ VConst "3"
                  , Result "alignment" $ VConst "-1"
                  , Result "col_name" $ VConst "enabled"
                  , Result "colhdr" $ VConst "Enb"
                  ]
              , VTuple $ Tuple [
                    Result "width" $ VConst "10"
                  , Result "alignment" $ VConst "-1"
                  , Result "col_name" $ VConst "addr"
                  , Result "colhdr" $ VConst "Address"
                  ]
              , VTuple $ Tuple [
                    Result "width" $ VConst "40"
                  , Result "alignment" $ VConst "2"
                  , Result "col_name" $ VConst "what"
                  , Result "colhdr" $ VConst "What"
                  ]
              ]
          , Result "body" $ VList $ EmptyList
          ]
      ]
    )
  , -- command result break-insert {{{3
  ([paste|
^done,bkpt={number="1",type="breakpoint",disp="keep",enabled="y",addr="0x000000000040154e",func="cond_wait",file="tc.c",fullname="/home/alex/scm/ocram/applications/simulation_os/os/tc.c",line="23",times="0",original-location="tc.c:23"}
(gdb) 
|], Output [] $ Just $ ResultRecord Nothing RCDone [
        Result "bkpt" $ VTuple $ Tuple [
            Result "number" $ VConst "1"
          , Result "type" $ VConst "breakpoint"
          , Result "disp" $ VConst "keep"
          , Result "enabled" $ VConst "y"
          , Result "addr" $ VConst "0x000000000040154e"
          , Result "func" $ VConst "cond_wait"
          , Result "file" $ VConst "tc.c"
          , Result "fullname" $ VConst "/home/alex/scm/ocram/applications/simulation_os/os/tc.c"
          , Result "line" $ VConst "23"
          , Result "times" $ VConst "0"
          , Result "original-location" $ VConst "tc.c:23"
          ]
      ]
    )
  , -- command result gdb-version {{{3
  ([paste|
~"GNU gdb (GDB) 7.2-ubuntu\n"
0^done
(gdb) 
|], Output [OOBStreamRecord $ SRConsoleStreamOutput $ ConsoleStreamOutput "GNU gdb (GDB) 7.2-ubuntu\n"] (Just $ ResultRecord (Just 0) RCDone [])
  )
  , -- command result exec-run {{{3
  ([paste|
=thread-group-started,id="i1",pid="18510"
=thread-created,id="1",group-id="i1"
2^running
*running,thread-id="all"
(gdb) 
|], Output [
        OOBAsyncRecord $ ARNotifyAsyncOutput $ NotifyAsyncOutput Nothing $ AsyncOutput ACThreadGroupStarted [Result "id" (VConst "i1"), Result "pid" (VConst "18510")]
      , OOBAsyncRecord $ ARNotifyAsyncOutput $ NotifyAsyncOutput Nothing $ AsyncOutput ACThreadCreated [Result "id" (VConst "1"), Result "group-id" (VConst "i1")]
      , OOBAsyncRecord $ ARExecAsyncOutput $ ExecAsyncOutput Nothing $ AsyncOutput ACRunning [Result "thread-id" (VConst "all")]
    ] $ Just $ ResultRecord (Just 2) RCRunning []
  )
  , -- breakpoint hit {{{3
  ([paste|
=library-loaded,id="/lib64/ld-linux-x86-64.so.2",target-name="/lib64/ld-linux-x86-64.so.2",host-name="/lib64/ld-linux-x86-64.so.2",symbols-loaded="0",thread-group="i1"
=library-loaded,id="/lib/libc.so.6",target-name="/lib/libc.so.6",host-name="/lib/libc.so.6",symbols-loaded="0",thread-group="i1"
*stopped,reason="breakpoint-hit",disp="keep",bkptno="1",frame={addr="0x0000000000400ba9",func="ec_thread_0",args=[{name="ec_cont",value="0x0"}],file="ec.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/ec.c",line="303"},thread-id="1",stopped-threads="all",core="0"
(gdb) 
|], Output
      [
        OOBAsyncRecord $ ARNotifyAsyncOutput $ NotifyAsyncOutput Nothing $ AsyncOutput ACLibraryLoaded 
          [
            Result "id" (VConst "/lib64/ld-linux-x86-64.so.2")
          , Result "target-name" (VConst "/lib64/ld-linux-x86-64.so.2")
          , Result "host-name" (VConst "/lib64/ld-linux-x86-64.so.2")
          , Result "symbols-loaded" (VConst "0")
          , Result "thread-group" (VConst "i1")
          ]
      , OOBAsyncRecord $ ARNotifyAsyncOutput $ NotifyAsyncOutput Nothing $ AsyncOutput ACLibraryLoaded 
          [
            Result "id" (VConst "/lib/libc.so.6")
          , Result "target-name" (VConst "/lib/libc.so.6")
          , Result "host-name" (VConst "/lib/libc.so.6")
          , Result "symbols-loaded" (VConst "0")
          , Result "thread-group" (VConst "i1")
          ]
      , OOBAsyncRecord $ ARExecAsyncOutput $ ExecAsyncOutput Nothing $ AsyncOutput ACStop
          [
            Result "reason" (VConst "breakpoint-hit")
          , Result "disp" (VConst "keep")
          , Result "bkptno" (VConst "1")
          , Result "frame" (VTuple $ Tuple
            [
              Result "addr" (VConst "0x0000000000400ba9")
            , Result "func" (VConst "ec_thread_0")
            , Result "args" (VList $ ValueList [VTuple $ Tuple
              [
                Result "name" (VConst "ec_cont")
              , Result "value" (VConst "0x0")
              ]])
            , Result "file" (VConst "ec.c")
            , Result "fullname" (VConst "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/ec.c")
            , Result "line" (VConst "303")
            ])
          , Result "thread-id" (VConst "1")
          , Result "stopped-threads" (VConst "all")
          , Result "core" (VConst "0")
          ]
      ] Nothing)
  ]
  where
    runTest :: (String, Output) -> Assertion -- {{{3
    runTest (str, output) =
      show output @=? show (parse_output (tail str))

test_response_break_insert :: Test -- {{{2
test_response_break_insert = enumTestGroup "response_break_insert" $ map runTest [
  -- example {{{3
  ([paste|
^done,bkpt={number="1",type="breakpoint",disp="keep",enabled="y",addr="0x0000000000400ba9",func="ec_thread_0",file="ec.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/ec.c",line="303",times="0",original-location="ec.c:ec_thread_0"}
(gdb) 
|], Breakpoint 1 "breakpoint" BreakpointKeep True "0x0000000000400ba9" "ec_thread_0" "ec.c" "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/ec.c" 303 0 "ec.c:ec_thread_0")
  ]
  where 
    runTest :: (String, Breakpoint) -> Assertion -- {{{3
    runTest (str, bp) =
      let
        output = parse_output (tail str)
        bp' = do 
          response <- output_response output
          response_break_insert (respResults response)
      in 
        show (Just bp) @=? show bp'

test_response_stopped :: Test -- {{{2
test_response_stopped = enumTestGroup "response_stopped" $ map runTest [
  -- breakpoint hit {{{3
  ([paste|
*stopped,reason="breakpoint-hit",disp="keep",bkptno="7",frame={addr="0x0000000000400e24",func="ec_thread_1",args=[{name="ec_cont",value="0x400ed5"}],file="ec.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/ec.c",line="433"},thread-id="1",stopped-threads="all",core="1"
(gdb) 
|], Stopped (BreakpointHit BreakpointKeep 7) (Frame Nothing "0x0000000000400e24" "ec_thread_1" (Just [Arg "ec_cont" "0x400ed5"]) "ec.c" (Just "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/ec.c") 433) 1 "all" 1)
  , -- end stepping range {{{3
  ([paste|
*stopped,reason="end-stepping-range",frame={addr="0x00000000004017fa",func="main",args=[],file="pal.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c",line="196"},thread-id="1",stopped-threads="all",core="1"
(gdb) 
|], Stopped EndSteppingRange (Frame Nothing "0x00000000004017fa" "main" (Just []) "pal.c" (Just "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c") 196) 1 "all" 1)
  ]
    where
    runTest :: (String, Stopped) -> Assertion -- {{{3
    runTest (str, stp) =
      let
        output = parse_output (tail str)
        [notification] = output_notification output
        stp' = response_stopped (notiResults notification)
      in
        show (Just stp) @=? show stp'

test_response_stack_list_frames :: Test -- {{{2
test_response_stack_list_frames = enumTestGroup "response_stack_list_frames" $ map runTest [
    -- example {{{3 
    ([paste|
^done,stack=[frame={level="0",addr="0x00007ffff7a9dcc7",func="_IO_vfprintf_internal",file="vfprintf.c",line="1647"},frame={level="1",addr="0x00007ffff7ac2c79",func="__IO_vsprintf",file="iovsprintf.c",line="43"},frame={level="2",addr="0x0000000000402520",func="logger_syscall",file="logger.c",fullname="/home/alex/scm/ocram/applications/simulation_os/os/logger.c",line="57"},frame={level="3",addr="0x0000000000401c13",func="os_receive",file="core.c",fullname="/home/alex/scm/ocram/applications/simulation_os/os/core.c",line="145"},frame={level="4",addr="0x0000000000401489",func="tc_receive",file="pal.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c",line="116"},frame={level="5",addr="0x0000000000400e2e",func="ec_thread_1",file="ec.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/ec.c",line="433"},frame={level="6",addr="0x00000000004016b2",func="flash_write_cb",file="pal.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c",line="156"},frame={level="7",addr="0x00000000004019ff",func="cb_default",file="core.c",fullname="/home/alex/scm/ocram/applications/simulation_os/os/core.c",line="90"},frame={level="8",addr="0x0000000000402f05",func="dispatcher_run",file="dispatcher.c",fullname="/home/alex/scm/ocram/applications/simulation_os/os/dispatcher.c",line="93"},frame={level="9",addr="0x000000000040188e",func="os_run",file="core.c",fullname="/home/alex/scm/ocram/applications/simulation_os/os/core.c",line="37"},frame={level="10",addr="0x00000000004012f0",func="pal_run",file="pal.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c",line="70"},frame={level="11",addr="0x0000000000401818",func="main",file="pal.c",fullname="/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c",line="200"}]
(gdb) 
|], Stack [
        Frame  (Just 0) "0x00007ffff7a9dcc7" "_IO_vfprintf_internal" Nothing "vfprintf.c"   Nothing 1647
      , Frame  (Just 1) "0x00007ffff7ac2c79" "__IO_vsprintf"         Nothing "iovsprintf.c" Nothing 43
      , Frame  (Just 2) "0x0000000000402520" "logger_syscall"        Nothing "logger.c"     (Just "/home/alex/scm/ocram/applications/simulation_os/os/logger.c") 57
      , Frame  (Just 3) "0x0000000000401c13" "os_receive"            Nothing "core.c"       (Just "/home/alex/scm/ocram/applications/simulation_os/os/core.c") 145
      , Frame  (Just 4) "0x0000000000401489" "tc_receive"            Nothing "pal.c"        (Just "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c") 116
      , Frame  (Just 5) "0x0000000000400e2e" "ec_thread_1"           Nothing "ec.c"         (Just "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/ec.c") 433
      , Frame  (Just 6) "0x00000000004016b2" "flash_write_cb"        Nothing "pal.c"        (Just "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c") 156
      , Frame  (Just 7) "0x00000000004019ff" "cb_default"            Nothing "core.c"       (Just "/home/alex/scm/ocram/applications/simulation_os/os/core.c") 90
      , Frame  (Just 8) "0x0000000000402f05" "dispatcher_run"        Nothing "dispatcher.c" (Just "/home/alex/scm/ocram/applications/simulation_os/os/dispatcher.c") 93
      , Frame  (Just 9) "0x000000000040188e" "os_run"                Nothing "core.c"       (Just "/home/alex/scm/ocram/applications/simulation_os/os/core.c") 37
      , Frame (Just 10) "0x00000000004012f0" "pal_run"               Nothing "pal.c"        (Just "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c") 70
      , Frame (Just 11) "0x0000000000401818" "main"                  Nothing "pal.c"        (Just "/home/alex/scm/ocram/applications/simulation_os/collect-and-forward/pal.c") 200
      ])
  ]
  where
    runTest :: (String, Stack) -> Assertion -- {{{3
    runTest (str, stack) =
      let
        output = parse_output (tail str)
        stack' = do 
          response <- output_response output
          response_stack_list_frames (respResults response)
      in 
        show (Just stack) @=? show stack'

test_response_exec_return :: Test -- {{{2
test_response_exec_return = enumTestGroup "response_exec_return" $ map runTest [
  -- example {{{3
  ([paste|
^done,frame={level="0",addr="0x080483cc",func="f",args=[],file="foo.c",fullname="/home/alex/foo.c",line="9"}
(gdb) 
|], Frame (Just 0) "0x080483cc" "f" (Just []) "foo.c" (Just "/home/alex/foo.c") 9
  )
  ]
  where
    runTest :: (String, Frame) -> Assertion -- {{{3
    runTest (str, frame) = 
      let
        output = parse_output (tail str)
        frame' = do
          response <- output_response output
          response_exec_return (respResults response)
      in
        show (Just frame) @=? show frame'

test_response_evaluate_expression :: Test -- {{{2
test_response_evaluate_expression = enumTestGroup "response_evaluate_expression" $ map runTest [
    -- example {{{3
    ([paste|
^done,value="24"
(gdb) 
|], "24")
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{3
    runTest (str, expr) =
      let
        output = parse_output (tail str)
        (Just response) = output_response output
      in do
        RCDone @=? respClass response
        Just expr @=? (response_data_evaluate_expression . respResults) response

test_response_error :: Test -- {{{2
test_response_error = enumTestGroup "response_error" $ map runTest [
    -- example {{{3
    ([paste|
^error,msg="No symbol \"j\" in current context."
(gdb) 
|], "No symbol \"j\" in current context.")
  ]
  where
    runTest :: (String, String) -> Assertion -- {{{3
    runTest (str, err) =
      let
        output = parse_output (tail str)
        (Just response) = output_response output
      in do
        RCError @=? respClass response
        Just err @=? (response_error . respResults) response

-- utils {{{1
qp :: String -> Parameter
qp = QuotedString
