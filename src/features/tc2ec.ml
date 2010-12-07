open Cil
open Cg

let feature : featureDescr = 
  { fd_name = "tc2ec";
    fd_enabled = ref true;
    fd_description = "generation of event-based code from thread-based code";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      let graph:callgraph = computeGraph f in
      printGraph stdout graph);
    fd_post_check = false;
  } 
