open CallGraph

let determine (api: string list) (cg: callgraph): unit = 
	let rec procUserFunction (c: call): unit =
		match (getFunction cg c.name) with
			None -> assert false		
			| Some(f) ->
				if (f.critical = false) then
					f.critical <- true;
					List.iter procUserFunction f.callers
	in
	let procApiFunction (name: string): unit =
		match (getFunction cg name) with
   			None -> ()
			| Some(f) -> 
				f.critical <- true;				
				List.iter procUserFunction f.callers
	in
	List.iter procApiFunction api
