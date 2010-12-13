type call = { location: Cil.location; fctName: string }

and func = {
	dec: Cil.fundec option;
	name: string;
	mutable critical: bool;
	mutable entry: bool;
	mutable api: bool;
	mutable callees: call list;
	mutable callers: call list;
}

and functions = (string, func) Hashtbl.t

and context = {
	file: Cil.file;
	functions: functions;
}

val init: Cil.file -> context
val getFunction: context -> string -> func option
val getEntryFunctions: context -> func list
val getApiFunctions: context -> func list 
val print: context -> out_channel -> unit

