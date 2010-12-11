open Cil
open Ctx

class computer (ctx: context) = object
  inherit nopCilVisitor

  val mutable curFunc: func option = None

  method vfunc (f:fundec) : fundec visitAction = begin 
		curFunc <- getFunction ctx f.svar.vname;
		DoChildren
  end

  method vinst (i:instr) : instr list visitAction = begin
    let caller : func = 
      match curFunc with 
        None -> assert false
      | Some c -> c
    in match i with
      Call(_,Lval(Var(vi),NoOffset),_,location) -> begin
				let callee = match (getFunction ctx vi.vname) with
					None -> let c = {
							dec = None;
							name = vi.vname;
							critical = false;
							entry = false;
							api = false;
							callers = [];
							callees = [];
						} in
						Hashtbl.add ctx.functions vi.vname c;
						c
					| Some(callee) -> callee
				in
						caller.callees <- {location=location; fctName=callee.name} :: caller.callees;
						callee.callers <- {location=location; fctName=caller.name} :: callee.callers;
						DoChildren
				end
			| _ -> 
				DoChildren
	end
end

let compute (ctx:context) : unit =
  visitCilFileSameGlobals (new computer ctx) ctx.file;
