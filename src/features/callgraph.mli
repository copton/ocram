type nodeinfo = 
    NIVar of Cil.varinfo * bool ref 
                         (* Node corresponding to a function. If the boolean 
                          * is true, then the function is defined, otherwise 
                          * it is external *)

  | NIIndirect of string (* Indirect nodes have a string associated to them. 
                          * These strings must be invalid function names *)
               * Cil.varinfo list ref 
                         (* A list of functions that this indirect node might 
                          * denote *)

type callnode = {
  (* An id *)
  cnid: int;
  
  (* the function this node describes *)
  cnInfo: nodeinfo;

  (* set of functions this one calls, indexed by the node id *)
  cnCallees: callnode Inthash.t;

  (* set of functions that call this one , indexed by the node id *)
  cnCallers: callnode Inthash.t;
}

type callgraph = (string, callnode) Hashtbl.t

val computeGraph (f:Cil.file) : callgraph

val printGraph (out:out_channel) (g:callgraph) : unit
