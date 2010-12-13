open Cil

(*
let (%) = Printf.sprintf

let pBool(value:bool): string = if (value) then "true" else "false"

let pIkind(ikind:ikind): string =
	match ikind with
		  	IChar 	-> "char"
		| 	ISChar 	-> "signed char"
		| 	IUChar 	-> "unsigned char"
		| 	IBool 	-> "_Bool (C99)"
		| 	IInt 	-> "int"
		| 	IUInt 	-> "unsigned int"
		| 	IShort 	-> "short"
		| 	IUShort 	-> "unsigned short"
		| 	ILong 	-> "long"
		| 	IULong 	-> "unsigned long"
		| 	ILongLong 	-> "long long"
		| 	IULongLong 	-> "unsigned long long"

let pLocation(location:location): string list =
	[("<location line=\"%d\" file=\"%s\" byte=\"%d\"/>" % location.line) location.file location.byte]

let pTyp(typ:typ): string list = 
	match typ with
		TVoid(_) -> ["<typ type=\"TVoid\"/>"]
	| TInt(ikind, _) -> ["<typ type=\"TInt\" ikind=\"%s\"/>" % (pIkind ikind)]
	| _ -> ["<typ type=other>"]

let pTypeinfo(typeinfo:typeinfo): string list = List.concat [
	[("<typeinfo tname=\"%s\" treferenced=\"%s\">" % typeinfo.tname) (pBool typeinfo.treferenced)];
	pTyp typeinfo.ttype;
	["</typeinfo>"]
]

let pFieldinfo(fieldinfo:fieldinfo): string list

let pCompinfo(compinfo:compinfo): string list = List.concat [
	[("<compinfo cstruct=\"%s\" cname=\"%s\" ckey=\"%d\" cdefined=\"%s\" creferenced=\"%s\">" % (pBool compinfo.cstruct)) compinfo.cname compinfo.ckey (pBool compinfo.cdefined) (pBool compinfo.creferenced)];
	List.concat (List.map pFieldinfo compinfo.cfields);	
	(* skipping attributes *)
	["</compinfo>"]
]

let pGlobal(global:global): string list = 
	match global with
		 GType(typeinfo,location) -> List.concat [
				["<global type=\"GType\">"]; 
				pTypeinfo typeinfo; 
				pLocation location; 
				["</global>"]
		]  
	 | GCompTag(compinfo, location) -> List.concat [
			["<global type=\"GCompTag\">"];
			pCompinfo compinfo;
			pLocation location;
			["</global>"]
		]
	| _ -> ["<other/>"]
	

let pFile(file:file):string list = List.concat [
	["<file fileName=\"%s\">" % file.fileName];
	List.concat (List.map pGlobal file.globals);
	["</file>"]
]
	
let doit(file:file) = 
	let s:string = String.concat "\n" (pFile file)
	in Printf.printf "%s\n" s
*)

let doit(file:file) =
	dumpFile plainCilPrinter stdout "file" file

let feature : featureDescr = { 
	fd_name = "printast";
	fd_enabled = ref false;
	fd_description = "print the AST of a source file";
	fd_extraopt = [];
	fd_doit = doit;
	fd_post_check = false;
} 
