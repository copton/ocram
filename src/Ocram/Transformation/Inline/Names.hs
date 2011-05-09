module Ocram.Transformation.Inline.Names where

-- the name of the variable that stores the continuation information
contVar = "ec_cont"

-- the name of the variable inside a t-stack frame that stores the result of the called function
resVar = "ec_result"

-- the name of the labels inside of the thread function
label fName count = "ec_label_" ++ fName ++ "_" ++ show count

-- the name of the thread function that conatins all critical function code
handlerFunction tid = "ec_thread_" ++ show tid

-- the type alias for the T-stack structure of a critical function
frameType fName = "ec_frame_" ++ fName ++ "_t"

-- name of the union variable that holds all nested frames
frameUnion = "ec_frames"

-- name of the frame paraemter of blocking function declarations
frameParam = "frame"

-- name of the variables that save the t-stack of a thread
stackVar fName = "ec_stack_" ++ fName
