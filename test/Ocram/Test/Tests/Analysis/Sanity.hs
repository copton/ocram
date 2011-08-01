module Ocram.Test.Tests.Analysis.Sanity (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.Sanity (check_sanity)
import Control.Monad.Reader (ask)

type Input = ()
type Output = TErrorCodes

reduce :: Ast -> Input -> ER Output
reduce ast _ = do
	opt <- ask
	let check = check_sanity ast
	return $ case execER opt check of
		Left e -> extractErrorCodes e
		Right _ -> [] 

setup :: TCase -> (Input, Output)
setup tc = ((), tcSanity tc)

tests = runTests "Sanity" reduce setup
