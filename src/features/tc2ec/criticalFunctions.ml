open Ctx

let determine (ctx: context): unit =
	let rec procUserFunction (call:call): unit =
		match (getFunction ctx call.fctName) with
			None -> assert false		
			| Some(func) ->
				func.critical <- true;
				List.iter procUserFunction func.callers
	in
	let procApiFunction (func: func): unit =
		func.critical <- true;
		List.iter procUserFunction func.callers
	in
	List.iter procApiFunction (getApiFunctions ctx)
