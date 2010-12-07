type nodeinfo
type callnode
type callgraph 

val computeGraph: Cil.file -> callgraph

val printGraph: out_channel -> callgraph -> unit
