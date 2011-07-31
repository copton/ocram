module Ocram.Compiler (
	analysis, analysis', transformation
) where

import Ocram.Types
import Ocram.Analysis (blocking_functions, defined_functions, start_routines, call_graph, critical_functions, check_call_graph, check_sanity, check_constraints)
import qualified Ocram.Transformation.Inline as Inline

import Control.Monad.Error (runErrorT, ErrorT(ErrorT), throwError)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (runWriterT)


analysis :: Options -> Ast -> EIO (Analysis, Ast)
analysis opt ast = ErrorT $ return $ execER opt (analysis' ast)

analysis' :: Ast -> ER (Analysis, Ast)
analysis' ast = do
	check_sanity ast
	bf <- blocking_functions ast
	df <- defined_functions ast
	sr <- start_routines df ast
	cg <- call_graph df bf ast
	check_call_graph cg sr df ast
	cf <- critical_functions cg bf ast
	check_constraints cf sr ast
	let ana = Analysis bf df sr cg cf
	return (ana, ast)

select_transformation :: Options -> Either String (Ast -> WR Ast)
select_transformation opt =
	case optScheme opt of
		"inline" -> return Inline.transformation
		s -> throwError $ "unknown compilation scheme \"" ++ s ++ "\"."	

transformation :: Options -> Analysis -> Ast -> EIO (Ast, DebugSymbols)
transformation opt ana ast = do
	transformation' <- ErrorT $ return $ select_transformation opt	
	return $ execWR (opt,ana) (transformation' ast)	
