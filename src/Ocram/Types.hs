module Ocram.Types (
	AST, Result
) where

import Control.Monad.Error
import Data.Either
import Language.C.Syntax.AST (CTranslUnit)

type AST = CTranslUnit

type Result a = Either String a
