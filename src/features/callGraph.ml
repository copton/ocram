open Cil
open Trace
open Printf
module P = Pretty
module H = Hashtbl
module E = Errormsg

type call = { loc: location; func: string }

type func = {
  info: Cil.varinfo; 
  defined: bool ref; 
  critical: bool ref;
  callees: call list ref;
  callers: call list ref;
}

let funcName (f:func):string = f.info.vname
let funcNew(i:varinfo):func = {
	info=i;
	defined = ref false;
	critical = ref false;
	callees = ref [];
	callers = ref [];
}	

let addEdge (caller: func) (callee: func) (loc: location):unit = 
	caller.callees := {loc = loc; func = funcName callee} :: !(caller.callees);
	callee.callers := {loc = loc; func = funcName caller} :: !(callee.callers)

class virtual callgraph = object
	method virtual get: string -> func option
	method virtual print: out_channel -> unit
end

class callgraphImpl = object
	inherit callgraph

	val functions: (string, func) H.t = H.create 20

	method get(name:string):func option =
		try
			let f = H.find functions name in
			Some f
		with Not_found ->
			None

	method getFunction (i: varinfo):func =
		try
			let f = H.find functions i.vname in
			assert (f.info = i);
			f
		with Not_found ->
			let f = funcNew i in
			H.add functions (funcName f) f;
			f

	method print(out:out_channel) : unit = begin 
		let printItem (c:call) : unit =
			(fprintf out " %s" c.func)
		in
		
		let printCalls (f:func) : unit =
			(fprintf out "  calls:");
			(List.iter printItem !(f.callees));
			(fprintf out "\n  is called by:");
			(List.iter printItem !(f.callers));
			(fprintf out "\n")
		in

		let printEntry (name:string) (f:func): unit =
			fprintf out "%s (%s):\n" name (if !(f.defined) then "defined" else "external");
			printCalls f
		in

		H.iter printEntry functions

		end
end

class computer (graph: callgraphImpl) = object
  inherit nopCilVisitor

  (* the current function we're in, so when we visit a call node
   * we know who is the caller *)
  val mutable curFunc: func option = None

  method vfunc (f:fundec) : fundec visitAction = begin 
(*    (trace "callgraph" (P.dprintf "entering function %s\n" f.svar.vname)); *)
  	let f = graph#getFunction f.svar in 
		f.defined := true;
   	curFunc <- (Some f);
		DoChildren
  end

  method vinst (i:instr) : instr list visitAction = begin
    let caller : func = 
      match curFunc with 
        None -> assert false
      | Some c -> c
    in match i with
      Call(_,Lval(Var(vi),NoOffset),_,loc) ->
(*		  (trace "callgraph" (P.dprintf "I see a call by %s to %s\n" caller.meta.info.vname vi.vname)); *)
				let callee = graph#getFunction vi in
				addEdge caller callee loc;
				DoChildren
			| _ -> 
				DoChildren
	end
end

let compute (f:file) : callgraph = begin
  let graph = new callgraphImpl in
  let obj = new computer graph in
  visitCilFileSameGlobals obj f;
  (graph :> callgraph)
end
