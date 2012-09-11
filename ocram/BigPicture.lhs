\documentclass{article}
%include polycode.fmt
\begin{document}

\section{setup}

> import Language.C.Syntax.AST

\section{Analysis}

> data OcramError = OcramError

> check_sanity :: CTranslUnit -> Either [OcramError] ()

> data Footprint = Footprint
> data CallGraph = CallGraph

> data Analysis = Analysis {
> 	nonCritical :: [CExtDecl],
> 	blocking :: [CDecl],
> 	critical :: [CFunDef],
> 	footprint :: Footprint,
> 	callgraph :: CallGraph
> 	}
>
> analysis :: CTranslUnit -> Analysis

> check_constraints :: CallGraph -> CTranslUnit -> Either [OcramError] ()

\section{Representation}

No declarations any more from here on. We replace declarations with initializers with proper assignment expressions. We also rename variables if the shadow other local variables. We keep track of the original name and the original scope, so that we can create the @VarMap@ later.
We need proper support for initializer lists and other funky initializer syntax that is not allowed as rvalue.

> collect_declarations :: CFunDef -> ([CStat], [Variable])

Replace all higher control sturctures with @if@ and @goto@. This also includes @switch@ statements. The body of then and else blocks (if existent) consist of a single goto statement.

> desugar_control_structures :: [CStat] -> [CStat]

Remove all @CCompound@s

> flatten_scopes :: [CStat] -> [CStat]

Translate from AST to Intermediate representation. Replace all labels with Hoogle labels.

> ast2ir :: CFunDef -> [Variable] -> Function

\section{Generic Passes}

Replace critical function calls in boolean expressions with equivalent sequences of @if@ statements

> short_circuiting :: Set Symbol -> Function -> Function

Make sure that all code paths of @void@ functions end with a @return@ statement

> explicit_return :: Function -> Function

Determine the set of critical and non-critical variables

> critical_variables :: Function -> ([Variables, Variables])

\section{Schema-dependant Passes}

> create_thread_execution_function :: CallGraph -> ThreadId -> [Function] -> Function

> optimize :: Function -> Function

> tstack_frame :: Function -> CDecl

> estack_frame :: Function -> CDecl

> blocking_function_declaration :: CDecl -> CDecl

\section{Output}

> ir2ast :: ThreadId -> Function -> CFunDef'

> create_ecode :: [CExtDecl] -> [Function] -> CTranslUnit

> print :: CTranslUnit -> (ByteString, Locations, BlockingCalls)
    
\section{Debugging}

> t2p_rows :: T2P -> TRow -> PRow

> p2e_map :: Location -> P2E
> e2p_map :: Location -> E2P

> newtype T2P = T2P {
>   maxTrow :: TRow,
>   maxProw :: PRow,
>   mapping :: [(TRow, PRow)]
>   }
>
> t2p_map :: ByteString -> ByteString -> T2P

\end{document}
