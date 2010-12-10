type call = { location: Cil.location; name: string }

and func = {
  info: Cil.varinfo; 
  mutable defined: bool; 
  mutable critical: bool;
  mutable callees: call list;
  mutable callers: call list;
}

and callgraph

val compute: Cil.file -> callgraph
val getFunction: callgraph -> string -> func option
val print: callgraph -> out_channel -> unit
