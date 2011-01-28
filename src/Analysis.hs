module Analysis (
    findStartRoutines
    , module Analysis.CallGraph
) where

import Analysis.StartRoutine (findStartRoutines)
import Analysis.CallGraph (determineCallGraph)
