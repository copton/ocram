module Ocram.Main (main) where

import Ocram.Types
import Ocram.Parser (parse)
import Ocram.Options (options)
import Ocram.Analysis (blocking_functions, defined_functions, start_routines, call_graph, critical_functions)
import Ocram.Filter (check_call_graph, check_sanity, check_constraints)
import qualified Ocram.Transformation.Inline as Inline
import Ocram.Output (pretty_print, writeDebugSymbols)

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

import Control.Monad.Error (runErrorT, ErrorT(ErrorT), throwError)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (runWriterT)

analysis :: Options -> Ast -> EIO (Analysis, Ast)
analysis opt ast = ErrorT $ return $ runReader (runErrorT (runER (analysis' ast))) opt

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
	return $ runReader (runWriterT (runWR (transformation' ast))) (opt, ana)
	
main = do
	err <- runErrorT $ do
		opt <- options
		ast <- parse opt
		(ana, ast') <- analysis opt ast
		(ast'', ds) <- transformation opt ana ast'
		pretty_print opt ast''
		writeDebugSymbols opt ds
	report err

report :: Either String () -> IO ()
report (Left err) = hPutStrLn stderr err >> exitWith (ExitFailure 1)
report _ = return ()
