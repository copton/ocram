type call = { loc: Cil.location; func: string }

type func = {
  info: Cil.varinfo; 
  defined: bool ref; 
  critical: bool ref;
  callees: call list ref;
  callers: call list ref;
}

class type virtual callgraph = object
	method virtual get: string -> func option
	method virtual print: out_channel -> unit
end

val compute: Cil.file -> callgraph
