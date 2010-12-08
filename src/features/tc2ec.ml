open Cil

let api: string list ref = ref []

let doit(f:file) = begin
  Cfg.computeFileCFG f ;
  let graph = CallGraph.compute f in
  let ef = EntryFunctions.analyze f in
  List.iter (function vi -> Printf.fprintf stdout "%s\n" vi.vname) ef
(*  CallGraph.print stdout graph; *)
end

let parseApiOpt(opt:string): unit = 
	api := Str.split (Str.regexp ",") opt

let feature : featureDescr = { 
	fd_name = "tc2ec";
    fd_enabled = ref true;
    fd_description = "generation of event-based code from thread-based code";
    fd_extraopt = [
		("--api", Arg.String parseApiOpt, "<list> comma-separated list of blocking functions which constitute the API of the platform abstraction layer.")
	];
    fd_doit = doit;
    fd_post_check = false;
} 
