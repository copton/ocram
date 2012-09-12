module Ocram.Names where

-- |The name of the attribute that marks blocking function declaratations
blockingAttr :: String
blockingAttr = "tc_blocking"

-- |The name of the attribute that marks thread start function definitions
startAttr :: String
startAttr = "tc_run_thread"

-- |The naming scheme for control flow labels
ctrLbl :: Int -> String
ctrLbl count = "ec_ctrlbl_" ++ show count

-- |The naming scheme to unshadow automatic variables
varShadow :: String -> Int -> String
varShadow name count =  "ec_shadow_" ++ name ++ "_" ++ show count

-- |The name of a temporary variable for critical calls
varCrit :: Int -> String
varCrit count = "ec_crit_" ++ show count
