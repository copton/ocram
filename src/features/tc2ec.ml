open Cil
open Trace
open Pretty

let apiOpt: string ref = ref ""

let doit(f:file) = begin
  Cfg.computeFileCFG f ;
	let ctx = Ctx.init f in 
  CallGraph.compute ctx;
	ApiFunctions.determine ctx !apiOpt;
	EntryFunctions.determine ctx;
	CriticalFunctions.determine ctx;
  Ctx.print ctx stdout

(*
  let cg = CallGraph.compute f in
  let ef = EntryFunctions.analyze f in
  let cf = CriticalFunctions.determine !api cg in

  List.iter (function name -> Printf.fprintf stdout "%s\n" name) ef;
  CallGraph.print graph stdout;
  dumpFile defaultCilPrinter stdout "stdout" f
*)
end

let feature : featureDescr = { 
	fd_name = "tc2ec";
    fd_enabled = ref true;
    fd_description = "generation of event-based code from thread-based code";
    fd_extraopt = [(
			"--api", 
			Arg.String (fun s -> apiOpt := s),
			"<list> comma-separated list of blocking functions which constitute the API of the platform abstraction layer."
		)];
    fd_doit = doit;
    fd_post_check = false;
} 
