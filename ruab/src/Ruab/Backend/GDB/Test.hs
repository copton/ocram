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
      MICommand (Just 23) "exec-arguments" [Option "v" (Just "word")] []
    , "23-exec-arguments -v word\n"
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
  ]
  where
    runTest :: (String, Output) -> Assertion
    runTest (str, output) =
      show output @=? show (parse_output (tail str))
