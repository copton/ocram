{-# LANGUAGE QuasiQuotes #-}
module Ruab.Backend.GDB.Test (tests) where

-- imports {{{1
import Ruab.Backend.GDB.Representation
import Ruab.Test.Lib (enumTestGroup, paste)
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), Assertion)

tests :: Test -- {{{1
tests = testGroup "GDB" [test_render_command, test_parse_output]

test_render_command:: Test -- {{{2
test_render_command = enumTestGroup "render_command" $ map runTest [
    (
      MICommand Nothing "break-info" [] []
    , "-break-info\n"
    ), (
      MICommand (Just 23) "exec-arguments" [Option "-v" (Just "word")] []
    , "23-exec-arguments \"-v\" \"word\"\n"
    ), (
      MICommand (Just 42) "break-commands" [Option "1" Nothing, Option "print v" Nothing] []
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
