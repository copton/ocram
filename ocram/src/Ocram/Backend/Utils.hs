module Ocram.Backend.Utils where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Ocram.Intermediate (FunctionVariable(..), fvar_static, Function(..))

allEVars :: Function -> [FunctionVariable] -- {{{1
allEVars = filter (not . or . sequence [fvar_critical, fvar_parameter, fvar_static]) . fun_vars

allTVars :: Function -> [FunctionVariable] -- {{{1
allTVars = filter ((&&) <$> not . fvar_static <*> or . sequence [fvar_critical, fvar_parameter]) . fun_vars

allSVars :: Function -> [FunctionVariable] -- {{{1
allSVars = filter fvar_static . fun_vars
