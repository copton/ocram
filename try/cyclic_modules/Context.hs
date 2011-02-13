module Context (
    Context(..)
) where

import Ana1Types
import Ana2Types

data Context = Context {
    ctxSetup :: Int,
    ctxResult1 :: Result1,
    ctxResult2 :: Result2
}
