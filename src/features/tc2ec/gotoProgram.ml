open Cil
open Ctx

class translator (ctx: context) = object
	inherit nopCilVisitor

	method vstmt(stmt:stmt): stmt visitAction = DoChildren
		
end

let translate (ctx: context) : unit =
	visitCilFileSameGlobals (new translator ctx) ctx.file	
