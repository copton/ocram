module Ana1 (
    analize1
)where

import Context
import Ana1Types

analize1 :: Context -> Result1
analize1 ctx = Result1 $ (ctxSetup ctx) + 1
