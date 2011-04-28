module Ocram.Transformation (
	transform
) where

import Ocram.Types
import qualified Ocram.Transformation.Inline as Inline

transform :: Context -> Result OutputAst
transform ctx = do
	let options = getOptions ctx
	case optScheme options of
		"inline" -> Inline.transform ctx
		s -> fail $ "unknown compilation scheme \"" ++ s ++ "\"."
