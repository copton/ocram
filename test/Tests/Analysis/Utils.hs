module Tests.Analysis.Utils (
    createContext
) where

import Context (Context)
import qualified CreateContext
import TestLib (parse)

createContext :: String -> Context
createContext code = CreateContext.createContext $ parse code
