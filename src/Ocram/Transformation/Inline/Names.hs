module Ocram.Transformation.Inline.Names where

contVar = "ec_cont"
contFrameVar = "frame"
resVar = "ec_result"
label fName count = "ec_label_" ++ fName ++ "_" ++ show count
handlerFunction tid = "ec_thread_" ++ show tid

frameType fName = "ec_frame_" ++ fName ++ "_t"
frameUnion = "ec_frames"
frameVar fName = "ec_frame_" ++ fName

stackVar fName = "ec_stack_" ++ fName
