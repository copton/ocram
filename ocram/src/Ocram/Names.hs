module Ocram.Names where

-- TODO: split this file into the submodules

-- |The name of the attribute that marks blocking function declaratations
blockingAttr :: String
blockingAttr = "tc_blocking"

-- |The name of the attribute that marks thread start function definitions
startAttr :: String
startAttr = "tc_run_thread"

-- |The naming scheme for control flow labels
ctrLbl :: Int -> String
ctrLbl count = "ec_ctrlbl_" ++ show count

-- |The naming scheme for unique variable names
varUnique :: String -> Int -> String
varUnique name count =  "ec_unique_" ++ name ++ "_" ++ show count

-- |The name of a temporary variable for critical calls
varCrit :: Int -> String
varCrit count = "ec_crit_" ++ show count

-- |The name of a temporary variable for boolean short-circuiting
varBool :: Int -> String
varBool count = "ec_bool_" ++ show count

-- |The type alias for a T-stack frame
tframe :: String -> String
tframe name = "ec_tframe_" ++ name ++ "_t"

-- |The name of the variable inside a t-stack frame that stores the result of the called function
resVar :: String
resVar = "ec_result"

-- |The name of the variable inside a t-stack frame that stores the continuation information
contVar :: String
contVar = "ec_cont"

-- |The name of the union variable that holds all nested frames
frameUnion :: String
frameUnion = "ec_frames"

-- |The name of the variables that hold the t-stack of a thread
tstackVar :: String -> String
tstackVar fName = "ec_tstack_" ++ fName

-- |The type alias for a E-stack frame
eframe :: String -> String
eframe name = "ec_eframe_" ++ name ++ "_t"

-- |The name of the variable that holds the e-stack of a thread
estackVar :: String
estackVar = "ec_estack"

-- |The name of the thread execution function
tfunction :: Int -> String
tfunction tid = "ec_thread_" ++ show tid

-- |The name of a continuation label
contLbl :: String -> String
contLbl ilbl = "ec_contlbl_" ++ ilbl

-- |How to mangle labels with function names
mangleFun :: String -> String -> String
mangleFun x fun = x ++ "_" ++ fun

-- |The name of a function static variable
varStatic :: String -> String -> String
varStatic fun var = "ec_static_" ++ fun ++ "_" ++ var
