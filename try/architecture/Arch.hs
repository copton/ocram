{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

data Options = Options
data Ast = Ast
data BlockingFunctions = BlockingFunctions
data DefinedFunctions = DefinedFunctions
data StartRoutines = StartRoutines
data CallGraph = CallGraph
data CriticalFunctions = CriticalFunctions

-- IO
options :: ErrorT String IO Options
options = undefined

parse :: ErrorT String IO Ast
parse = undefined

pretty_print :: Ast -> ErrorT String IO ()
pretty_print = undefined

write_debug_symbols :: [DebugSymbol] -> ErrorT String IO ()
write_debug_symbols = undefined

report :: Either String () -> IO ()
report (Left err) = putStrLn $ "failed: " ++ err
report _ = putStrLn "complete"

-- Analysis
data Analysis = Analysis {
	getBlockingFunctions :: BlockingFunctions,
	getDefinedFunctions :: DefinedFunctions,
	getStartRoutines :: StartRoutines,
	getCallGraph :: CallGraph,
	getCriticalFunctions :: CriticalFunctions
}


check_sanity :: Ast -> ER ()
check_sanity = undefined

check_call_graph :: CallGraph -> StartRoutines -> DefinedFunctions -> Ast -> ER ()
check_call_graph = undefined

blocking_functions :: Ast -> ER BlockingFunctions
blocking_functions = undefined

defined_functions :: Ast -> ER DefinedFunctions
defined_functions = undefined

start_routines :: DefinedFunctions -> Ast -> ER StartRoutines
start_routines = undefined

call_graph :: DefinedFunctions -> BlockingFunctions -> Ast -> ER CallGraph
call_graph = undefined

critical_functions :: CallGraph -> BlockingFunctions -> Ast -> ER CriticalFunctions
critical_functions = undefined

check_constraints :: CriticalFunctions -> StartRoutines -> Ast -> ER ()
check_constraints = undefined

analysis :: Options -> Ast -> ErrorT String IO (Analysis, Ast)
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

-- Transformation

data DebugSymbol = DebugSymbol

data TransEnv = TransEnv {
	getAnalysis :: Analysis,
	getOptions :: Options
}

newtype Trans a = Trans {
		runTrans :: WriterT [DebugSymbol] (Reader TransEnv) a
 	} deriving (
		Monad, 
		MonadWriter [DebugSymbol], 
		MonadReader TransEnv
	)

transform :: Options -> Analysis -> Ast -> (Ast, [DebugSymbol])
transform opt ana ast = runReader (runWriterT (runTrans (transform' ast))) $ TransEnv ana opt

transform' :: Ast -> Trans Ast
transform' = undefined

main :: IO ()
main = do
	err <- runErrorT $ do
		opt <- options
		ast <- parse
		(ana, ast') <- analysis opt ast
		let (ast'', ds) = transform opt ana ast'
		pretty_print ast''
		write_debug_symbols ds
	report err
