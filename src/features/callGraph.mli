type nodeinfo
type callnode
type callgraph 

val compute: Cil.file -> callgraph

val print: out_channel -> callgraph -> unit
