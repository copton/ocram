open Cil
module E = Errormsg

class step2(entryFunctions: varinfo list ref) = object(self)
	inherit nopCilVisitor

	method vinst (i:instr): instr list visitAction =
		match i with
			Call(_, Lval(Var(rt),_),[AddrOf(Var(ef), _)],_) when rt.vname = "RUN_THREAD"-> 
				entryFunctions := ef :: !entryFunctions;
				SkipChildren
			| _ -> DoChildren
end
		
class step1(entryFunctions: varinfo list ref) = object(self)
	inherit nopCilVisitor

	val s2 = new step2 entryFunctions

	method vfunc (f: fundec): fundec visitAction = 
		if (f.svar.vname = "main") then begin
			let s2 = new step2 entryFunctions in
			ignore(visitCilFunction s2 f)
		end;
		SkipChildren
				
end

let analyze (f:file): varinfo list = begin
	let entryFunctions: varinfo list ref = ref [] in
	let analyzer = new step1 entryFunctions in
	visitCilFileSameGlobals analyzer f;
	!entryFunctions
end
