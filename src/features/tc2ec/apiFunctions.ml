open Ctx
open Trace
open Pretty

let parse(infile:string): string list =
	let channel = open_in infile in
	let result: string list ref = ref [] in
	begin 
	try
		while true do
			result := (input_line channel) :: !result
		done
	with End_of_file -> () 
	end;
	!result

let determine (ctx:context) (infile:string): unit =
	let setFlag (name:string): unit =
		match (getFunction ctx name) with
			Some(func) -> func.api <- true
		| None -> trace "api" (dprintf "API function %s is not used" name)
	in
	List.iter setFlag (parse infile)
