module CreateContext (
    createContext
) where

import Context
import Ana1
import Ana2

createContext setup = ctx
    where ctx = Context setup res1 res2
          res1 = analize1 ctx
          res2 = analize2 ctx
