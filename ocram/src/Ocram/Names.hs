module Ocram.Names where

import Text.Printf (printf)

-- TODO: split this file into the submodules

ecPrefix :: String
ecPrefix = "ec_"

-- |The name of the attribute that marks blocking function declaratations
blockingAttr :: String
blockingAttr = "tc_block"

-- |The name of the attribute that marks thread start function definitions
startAttr :: String
startAttr = "tc_thread"

-- |The naming scheme for control flow labels
identDesugar :: Int -> String
identDesugar = printf "%sdesugar_%d" ecPrefix

-- |The naming of a new variable for desugaring of switch statements
switchVar :: Int -> String
switchVar = printf "%sswitch_%d" ecPrefix

-- |The naming scheme for unique variable names
varUnique :: String -> Int -> String
varUnique = printf "%sunique_%s_%d" ecPrefix 

-- |The name of a temporary variable for critical calls
varCrit :: Int -> String
varCrit = printf "%scrit_%d" ecPrefix

-- |The name of a new variable for boolean short-circuiting
varBool :: Int -> String
varBool = printf "%sbool_%d" ecPrefix

-- |The type alias for a T-stack frame
tframe :: String -> String
tframe = printf "%stframe_%s_t" ecPrefix

-- |The name of the variable inside a t-stack frame that stores the result of the called function
resVar :: String
resVar = printf "%sresult" ecPrefix

-- |The name of the variable inside a t-stack frame that stores the continuation information
contVar :: String
contVar = printf "%scont" ecPrefix

-- |The name of the union variable that holds all nested frames
frameUnion :: String
frameUnion = printf "%sframes" ecPrefix

-- |The name of the variables that hold the t-stack of a thread
tstackVar :: String -> String
tstackVar = printf "%ststack_%s" ecPrefix

-- |The type alias for a E-stack frame
eframe :: String -> String
eframe = printf "%seframe_%s_t" ecPrefix

-- |The name of the variable that holds the e-stack of a thread
estackVar :: String
estackVar = printf "%sestack" ecPrefix

-- |The name of the thread execution function
tfunction :: Int -> String
tfunction = printf "%sthread_%d" ecPrefix

-- |The name of a continuation label
contLbl :: String -> String
contLbl = printf "%scontlbl_%s" ecPrefix

-- |The name of a function static variable
varStatic :: String -> String -> String
varStatic = printf "%sstatic_%s_%s" ecPrefix

-- |How to mangle labels with function names
mangleFun :: String -> String -> String
mangleFun x fun = x ++ "_" ++ fun

