open Cil
open Printf

type call = { location: Cil.location; fctName: string }

and func = {
	dec: fundec option;
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

class collector (functions: functions) = object
	inherit nopCilVisitor

	method vfunc(dec:fundec):fundec visitAction =
		let name = dec.svar.vname in
		Hashtbl.add functions name {
			dec=Some(dec);
			name=name;
			critical=false;
			entry=false;
			api=false;
			callees=[];
			callers=[];
		};
		SkipChildren
end

let parseApiOpt(apiOpt:string): string list = 
	Str.split (Str.regexp ",") apiOpt
			
let init (file: file): context = begin
	let functions = Hashtbl.create 20 in
	visitCilFileSameGlobals (new collector functions) file;
	{
		file = file; 
		functions = functions;
	}
end	

let getFunction (ctx:context) (name:string):func option =
	try
		let f = Hashtbl.find ctx.functions name in
		Some f
	with Not_found ->
		None

let getFunctions (ctx:context) (pred: func -> bool): func list =
	let result: func list ref = ref [] in
	let process name func: unit =
		if (pred func) then
			result := func :: !result
	in
	Hashtbl.iter process ctx.functions;
	!result

let getApiFunctions (ctx:context): func list =
	getFunctions ctx (fun func -> func.api = true)	

let getEntryFunctions (ctx:context): func list =
	getFunctions ctx (fun func -> func.entry = true)	

let print(ctx:context)(out:out_channel) : unit = begin 
	let printFile(file:file): unit =
		fprintf out "# begin %s\n" file.fileName;
		dumpFile defaultCilPrinter out file.fileName file;
		fprintf out "# end %s\n" file.fileName;
	in 
	let printCall (c:call) : unit =
		fprintf out " %s %d," c.fctName c.location.line
	in 
	let properties(f: func): string list =
		let defined (): string list =
			match f.dec with Some(_) -> ["defined"] | None -> [];
		in
		List.concat([
			if f.api then ["api"] else [];
			if f.critical then ["critical"] else [];
			defined ();
			if f.entry then ["entry"] else []
		])
	in 
	let printCalls (f:func) : unit =
		(fprintf out "\n  calling:");
		(List.iter printCall f.callees);
		(fprintf out "\n  is called by:");
		(List.iter printCall f.callers);
	in 
	let printFunction(name:string) (f:func): unit =
		assert (name = f.name);
		(fprintf out "%s (%s)" name (String.concat ", " (properties f)));
		printCalls f;
		fprintf out "\n"
	in

	printFile ctx.file;
	Hashtbl.iter printFunction ctx.functions
end
