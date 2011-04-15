module Ocram.Transformation.Names where

contType = "ec_continuation_t"
contVar = "ec_cont"
resVar = "ec_result"
label fName count = "ec_label_" ++ fName ++ "_" ++ (show count)
handlerFunction = "ec_events"

frameType fName = "ec_frame_" ++ fName ++ "_t"
frameUnion = "ec_frames"
frameVar fName = "ec_frame_" ++ fName
