open Cil
open Ctx
module E = Errormsg

class step2(ctx: context) = object
	inherit nopCilVisitor

	method vinst (i:instr): instr list visitAction =
		match i with
			Call(_,Lval(Var(rt),_),[CastE(_, AddrOf(Var(ef), NoOffset))], _) when rt.vname = "RUN_THREAD" -> begin
				match (getFunction ctx ef.vname) with
					None -> E.s (E.error "function %s is used as an entry function but is not defined" ef.vname)
				  | Some(func) -> 
						func.entry <- true;
						SkipChildren
			end
			| _ -> DoChildren
end
		
class step1(ctx: context) = object
	inherit nopCilVisitor

	val s2 = new step2 ctx

	method vfunc (f: fundec): fundec visitAction = 
		if (f.svar.vname = "main") then begin
			ignore(visitCilFunction s2 f)
		end;
		SkipChildren
end

let determine (ctx:context): unit =
	visitCilFileSameGlobals (new step1 ctx) ctx.file
