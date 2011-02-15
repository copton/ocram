module CreateContext (
    createContext
) where

import Context
import Analysis

createContext ast = ctx
    where ctx = Context ast fm sr cg
          fm = getFunctions ctx
          sr = findStartRoutines ctx
          cg = determineCallGraph ctx
