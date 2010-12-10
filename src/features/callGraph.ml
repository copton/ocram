open Cil
open Trace
open Printf
module P = Pretty
module H = Hashtbl
module E = Errormsg

type call = { location: location; name: string }

type func = {
  info: Cil.varinfo; 
  mutable defined: bool; 
  mutable critical: bool;
  mutable callees: call list;
  mutable callers: call list;
}

let funcName (f:func):string = f.info.vname
let newFunc (i:varinfo):func = {
	info = i;
	defined = false;
	critical = false;
	callees = [];
	callers = [];
}	

let addEdge (caller: func) (callee: func) (location: location):unit = 
	caller.callees <- {location = location; name = funcName callee} :: caller.callees;
	callee.callers <- {location = location; name = funcName caller} :: callee.callers

type callgraph = (string, func) H.t

let newCallgraph = H.create 20

let getFunction (cg:callgraph) (name:string):func option =
	try
		let f = H.find cg name in
		Some f
	with Not_found ->
		None

let getOrCreateFunction (cg:callgraph) (i: varinfo):func =
	try
		let f = H.find cg i.vname in
		assert (f.info = i);
		f
	with Not_found ->
		let f = newFunc i in
		H.add cg (funcName f) f;
		f

let print(cg:callgraph)(out:out_channel) : unit = begin 
	let printItem (c:call) : unit =
		(fprintf out " %s %d" c.name c.location.line)
	in
	
	let printCalls (f:func) : unit =
		(fprintf out "  calls:");
		(List.iter printItem f.callees);
		(fprintf out "\n  is called by:");
		(List.iter printItem f.callers);
		(fprintf out "\n")
	in

	let printEntry (name:string) (f:func): unit =
		fprintf out "%s (%s, %s):\n" name (if f.defined then "defined" else "external") (if f.critical then "critical" else "non-critical");
		printCalls f
	in

	H.iter printEntry cg
end

class computer (cg: callgraph) = object
  inherit nopCilVisitor

  (* the current function we're in, so when we visit a call node
   * we know who is the caller *)
  val mutable curFunc: func option = None

  method vfunc (f:fundec) : fundec visitAction = begin 
(*    (trace "callgraph" (P.dprintf "entering function %s\n" f.svar.vname)); *)
  	let f = getOrCreateFunction cg f.svar in 
		f.defined <- true;
   	curFunc <- (Some f);
		DoChildren
  end

  method vinst (i:instr) : instr list visitAction = begin
    let caller : func = 
      match curFunc with 
        None -> assert false
      | Some c -> c
    in match i with
      Call(_,Lval(Var(vi),NoOffset),_,location) ->
(*		  (trace "callgraph" (P.dprintf "I see a call by %s to %s\n" caller.meta.info.vname vi.vname)); *)
				let callee = getOrCreateFunction cg vi in
				addEdge caller callee location;
				DoChildren
			| _ -> 
				DoChildren
	end
end

let compute (f:file) : callgraph = begin
  let cg = newCallgraph in
  let obj = new computer cg in
  visitCilFileSameGlobals obj f;
  cg
end
