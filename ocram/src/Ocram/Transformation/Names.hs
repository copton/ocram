module Ocram.Transformation.Names where

-- the name of the variable that stores the continuation information
contVar :: String
contVar = "ec_cont"

-- the name of the variable inside a t-stack frame that stores the result of the called function
resVar :: String
resVar = "ec_result"

-- the name of the labels inside of the thread function
label :: String -> Int -> String
label fName count = "ec_label_" ++ fName ++ "_" ++ show count

-- the name of the labels from desugaration of control structures
ctrlbl :: Int -> String
ctrlbl count = "ec_ctrlbl_" ++ show count

-- the name of the thread function that conatins all critical function code
threadExecutionFunction :: Int -> String
threadExecutionFunction tid = "ec_thread_" ++ show tid

-- the type alias for the T-stack structure of a critical function
frameType :: String -> String
frameType fName = "ec_frame_" ++ fName ++ "_t"

-- name of the pointer to the calling thread
threadPointer :: String
threadPointer = "ec_thread"

-- name of the union variable that holds all nested frames
frameUnion :: String
frameUnion = "ec_frames"

-- name of the variables that save the t-stack of a thread
stackVar :: String -> String
stackVar fName = "ec_stack_" ++ fName

-- name of a temporary variable for cri
varCrit :: Int -> String
varCrit count = "ec_crit_" ++ show count

-- name of temporary variables for boolean short-circuiting
varBool :: Int -> String
varBool count = "ec_bool_" ++ show count

-- name of temporary variables that avoid shadowing
varShadow :: String -> Int -> String
varShadow name count = "ec_shadow_" ++ name ++ "_" ++ show count
