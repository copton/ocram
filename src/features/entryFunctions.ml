open Cil
module E = Errormsg

class analyzer(entryFunctions: varinfo list ref) = object(self)
	inherit nopCilVisitor

	method vinst (i:instr): instr list visitAction = begin
		match i with
			Call(_, Lval(Var(rt),_),[AddrOf(Var(ef), _)],_) when rt.vname = "RUN_THREAD"-> 
				entryFunctions := ef :: !entryFunctions;
				SkipChildren
			| _ -> DoChildren
	end	
end

let analyze (f:file): varinfo list = begin
	let entryFunctions: varinfo list ref = ref [] in
	let analyzer = new analyzer (entryFunctions) in
	visitCilFileSameGlobals analyzer f;
	!entryFunctions
end
