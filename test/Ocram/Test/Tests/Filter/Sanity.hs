module Ocram.Test.Tests.Filter.Sanity (
	tests
) where

import Ocram.Types
import Ocram.Test.Lib
import Ocram.Filter.Sanity (check_sanity)
import Ocram.Test.Tests.Filter.Utils (extractErrorCodes)
import Control.Monad.Reader (ask)

reduce :: String -> ER [Int]
reduce code = do
	let ast = parse code
	opt <- ask
	return $ case execER opt (check_sanity ast) of
		Left e -> extractErrorCodes e
		Right _ -> []

tests = runTests "Sanity" reduce [
		("void foo { }", [1])
	]

