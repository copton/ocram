module Ana2 (
    analize2
)where

import Context
import Ana2Types
import Ana1Types

analize2 :: Context -> Result2
analize2 ctx = Result2 $ (get (ctxResult1 ctx)) + 1

get (Result1 i) = i
