open Cil

let doit(file:file) = 
	let buf: Buffer.t = Buffer.create 1024 in
	let out(string: string): unit =
		Buffer.add_string buf string
	in

	let comma = out ", "
	and semicolon = out "; "
	and pOpen = out "("
	and pClose = out ")"
	and sOpen = out "["
	and sClose = out "]"
	and cOpen = out "{"
	and cClose = out "}"
	in

	let rec pBool(value:bool): unit= if (value) then (out "true") else (out "false")

(*	and pOption(print:'a->unit)(value: 'a option):unit =*)
	and pOption print value =
		match value with
			Some(x) -> out "Some("; print x; pClose
		| None -> out "None"

(*	and pList(print:'a->unit)(values: 'a list): unit= *)
	and pList print values = 
		match values with
			[] -> out "[]"
		| xs -> sOpen; List.iter (fun x -> out "; "; print x) xs; sClose

	and pString(s:string):unit = out "\""; out s; out "\""

	and pInt(i:int):unit = out (string_of_int i)

	and pInt64(i:int64):unit = out (Int64.to_string i)

	and pFloat(f:float):unit = out (string_of_float f)

	and pChar(char:char):unit = out "'"; out (String.make 1 char); out "'"

	and pTuple(printa:'a->unit)(printb:'b->unit)(tuple:('a * 'b)):unit =
		pOpen; printa (fst tuple); comma ; printb (snd tuple); pClose

	and pTriple(printa:'a->unit)(printb:'b->unit)(printc:'c->unit)(triple:('a*'b*'c)):unit =
		match triple with
			(a,b,c) -> pOpen; printa a; comma; printb b; comma; printc c; pClose

	and pAttributes(attributes:attributes):unit= 
		pList pAttribute attributes

	and pAttribute(attribute:attribute):unit= out "_" (* TODO *)

	and pIkind(ikind:ikind):unit = out (
		match ikind with
			IChar 	-> "IChar"
		|	ISChar 	-> "ISChar"
		|	IUChar 	-> "IUChar"
		|	IBool 	-> "IBool"
		|	IInt 	-> "IInt"
		|	IUInt 	-> "IUInt"
		|	IShort 	-> "IShort"
		|	IUShort 	-> "IUShort"
		|	ILong 	-> "ILong"
		|	IULong 	-> "IULong"
		|	ILongLong 	-> "ILongLong"
		|	IULongLong 	-> "IULongLong"
	)

	and pFkind(fkind:fkind):unit = out (
		match fkind with
			FFloat -> "FFloat"
		| FDouble -> "FDouble"
		| FLongDouble -> "FLongDouble"
	)

	and pBinop(binop:binop):unit = out (
		match binop with
			PlusA 	-> "PlusA"
		|	PlusPI 	-> "PlusPI"
		|	IndexPI 	-> "IndexPI"
		|	MinusA 	-> "MinusA"
		|	MinusPI 	-> "MinusPI"
		|	MinusPP 	-> "MinusPP"
		|	Mult -> "Mult"
		|	Div 	-> "Div"
		|	Mod 	-> "Mod"
		|	Shiftlt 	-> "Shiftlt"
		|	Shiftrt 	-> "Shiftrt"
		|	Lt 	-> "Lt"
		|	Gt 	-> "Gt"
		|	Le 	-> "Le"
		|	Ge 	-> "Ge"
		|	Eq 	-> "Eq"
		|	Ne 	-> "Ne"
		|	BAnd 	-> "BAnd"
		|	BXor 	-> "BXor"
		|	BOr 	-> "BOr"
		|	LAnd 	-> "LAnd"
		|	LOr 	-> "LOr"
	)

	and pUnop(unop:unop):unit = out (
		match unop with
			Neg -> "Neg"
		| BNot -> "BNot"
		| LNot -> "LNot"
	)

	and pConstant(constant:constant):unit =
		match constant with
			CInt64(int64, ikind, string) -> out "CInt64("; pInt64 int64; comma; pIkind ikind; comma; pOption pString string; pClose
		| CStr(string) -> out "CStr("; pString string; pClose
		| CWStr(int64s) -> out "CWStr("; pList pInt64 int64s; pClose
		| CChr(char) -> out "CChr("; pChar char; pClose
		| CReal(float, fkind, string) -> out "CReal("; pFloat float; comma; pFkind fkind; comma; pOption pString string; pClose
		| CEnum(exp, string, enuminfo) -> out "CEnum("; pExp exp; comma; pString string; comma; pEnuminfo enuminfo; pClose

	and pExp(exp:exp):unit = 
		match exp with
			Const(constant) -> out "Const("; pConstant constant; pClose
		| Lval(lval) -> out "Lval("; pLval lval; pClose
		| SizeOf(typ) -> out "SizeOf("; pTyp typ; pClose
		| SizeOfE(exp) -> out "SizeOfE("; pExp exp; pClose
		| SizeOfStr(str) -> out "SizeOfStr("; pString str; pClose
		| AlignOf(typ) -> out "AlignOf("; pTyp typ; pClose
		| AlignOfE(exp) -> out "AlignOfE("; pExp exp; pClose
		| UnOp(unop, exp, typ) -> out "UnOp("; pUnop unop; comma; pExp exp; comma; pTyp typ; pClose
		| BinOp(binop, exp1, exp2, typ) -> out "BinOp("; comma; pBinop binop; comma; pExp exp1; comma; pExp exp2; pTyp typ; pClose
		| CastE(typ, exp) -> out "CastE(", pTyp typ; comma; pExp exp; pClose
		| AddrOf(lval) -> out "AddrOf("; pLval lval; pClose
		| StartOf(lval) -> out "StartOf("; pLval lval; pClose

	and pCompinfo(compinfo:compinfo):unit =
		cOpen;
		out "cstruct="; pBool compinfo.cstruct; semicolon;
		out "cname="; pBool compinfo.cstruct; semicolon;
		out "ckey="; pInt compinfo.ckey; semicolon;
		out "cfields="; pList pFieldinfo compinfo.cfields; semicolon;
		out "cattr="; pAttributes compinfo.cattr; semicolon;
		out "cdefined="; pBool compinfo.cdefined; semicolon;
		out "creferenced="; pBool compinfo.creferenced;
		cClose

	and pEnuminfo(enuminfo:enuminfo):unit =
		cOpen
		("{ename=%s; eitems=%s; eattr=%s; ereferenced=%s; ekind=%s}" %
		out "ename="; pString enuminfo.ename); semicolon;
		out "eitems="; pList (pTriple pString pExp pLocation) enuminfo.eitems; semicolon;
		out "eattr="; pAttributes enuminfo.eattr; semicolon;
		out "ereferenced="; pBool enuminfo.ereferenced; semicolon;
		out "ekind="; pIkind enuminfo.ekind;
		cClose

	and pStorage(storage:storage):unit = out(
		match storage with
			NoStorage -> "NoStorage"
		| Static -> "Static"
		| Register -> "Register"
		| Extern -> "Extern"
	)

	and pVarinfo(varinfo:varinfo):unit = 
		cOpen;
		out "vname="; pString varinfo.vname; semicolon;
		out "vtype="; pTyp varinfo.vtype; semicolon;
		out "vattr="; pAttributes varinfo.vattr; semicolon;
		out "vstorage="; pStorage varinfo.vstorage; semicolon;
		out "vglob="; pBool varinfo.vglob; semicolon;
		out "vinline="; pBool varinfo.vinline; semicolon;
		out "vdecl="; pLocation varinfo.vdecl; semicolon;
		out "vid="; pInt varinfo.vid; semicolon;
		out "vaddrof="; pBool varinfo.vaddrof; semicolon;
		out "vreferenced="; pBool varinfo.vreferenced; semicolon;
		out "_" (* no support for Pretty.doc parts *); semicolon;
		out "vdescrpure="; pBool varinfo.vdescrpure;
		cClose

	and pInit(init:init):unit = 
		match init with
			SingleInit(exp) -> out "SingleInit("; pExp exp; pClose
		| CompoundInit(typ, inits) -> out "CompoundInit("; pTyp typ; comma; pList (pTuple pOffset pInit) inits; pClose

	and pInitinfo(initinfo:initinfo):unit = 
		cOpen;
		out "init="; pOption pInit initinfo.init;
		pClose

	and pFieldinfo(fieldinfo:fieldinfo):unit =
		cOpen;
		out "fcomp="; pCompinfo fieldinfo.fcomp; semicolon;
		out "fname="; pString fieldinfo.fname; semicolon;
		out "ftype="; pTyp fieldinfo.ftype; semicolon;
		out "fbitfield="; pOption pInt fieldinfo.fbitfield; semicolon;
		out "fattr="; pAttributes fieldinfo.fattr; semicolon;
		out "floc="; pLocation fieldinfo.floc;
		cClose

	and pOffset(offset:offset):unit = 
		match offset with
			NoOffset -> out "NoOffset"
		| Field(fieldinfo, offset) -> out "Field("; pFieldinfo fieldinfo; comma; pOffset offset; pClose
		| Index(exp, offset) -> out "Index("; pExp exp; comma; pOffset offset; pClose

	and pLhost(lhost:lhost):unit = 
		match lhost with
			Var(varinfo) -> out "Var("; pVarinfo varinfo; pClose
		| Mem(exp) -> out "Mem("; pExp exp; pClose

	and pLval(lval:lval) = pTuple pLhost pOffset lval

	and pInstr(instr:instr):unit = 
		match instr with
			Set(lval, exp, location) -> out "Set("; pLval lval; comma; pExp exp; comma; pLocation location; pClose
		| Call(lval, exp, exps, location) -> out "Call("; pOption pLval lval; comma; pExp exp; comma; pList pExp exps; comma; pLocation location; pClose
		| Asm _ -> out "Asm(_)" (* no support for assembler *)

	and pStmtkind(stmtkind:stmtkind):unit = 
		match stmtkind with
			Instr(instr) -> out "Instr("; pList pInstr instr; pClose
		| Return(exp, location) -> out "Return("; pOption pExp exp; comma; pLocation location; pClose
		| Goto(stmt, location) -> out "Goto("; pLocation location; comma; pList pLabel (!stmt).labels; pClose
		| Break(location) -> out "Break("; pLocation location; pClose
		| Continue(location) -> out "Continue(";  pLocation location; pClose
		| If(exp, block1, block2, location) -> out "If("; pExp exp; comma; pBlock block1; comma; pBlock block2; comma; pLocation location; pClose
		| Switch(exp, block, stmts, location) -> out "Switch("; pExp exp; comma; pBlock block; comma; pList pStmt stmts; comma; pLocation location; pClose
		| Loop(block, location, stmt1, stmt2) -> out "Loop("; pBlock block; comma; pLocation location; comma; pOption pStmt stmt1; comma; pOption pStmt stmt2; pClose
		| Block(block) -> out "Block("; pBlock block; pClose
		| TryFinally _  -> out "TryFinally(_)" (* not supporting C++ *)
		| TryExcept _ -> out "TryExcept(_)" (* not supporting C++ *)

	and pLabel(label:label):unit = 
		match label with
			Label(name, location, flag) -> out "Label("; pString name; comma; pLocation location; comma; pBool flag; pClose
		| Case(exp, location) -> out "Case("; pExp exp; comma; pLocation location; pClose
		| Default(location) -> out "Default("; pLocation location; pClose

	and pBlock(block:block):unit = 
		cOpen;
		out "battrs="; pAttributes block.battrs; semicolon;
		out	"bstmts="; pList pStmt block.bstmts; semicolon;
		cClose

	and pStmt(stmt:stmt):unit = 
		cOpen;
		out "labels="; pList pLabel stmt.labels; semicolon;
		out "skind="; pStmtkind stmt.skind; semicolon;
		out "sid="; pInt stmt.sid; semicolon;
		out "succs=_"; semicolon;
		out "pred=_";  (* pred and succs could lead to infinite recuresion (?) *)
		cClose

	and pTyp(typ:typ):unit = 
		match typ with
			TVoid (attributes) -> out "TVoid("; pAttributes attributes; pClose
		| TInt (ikind, attributes) -> out "TInt("; comma; pIkind ikind; comma; pAttributes attributes; pClose
		| TFloat (fkind, attributes) -> out "TFloat("; comma; pFkind fkind; comma; pAttributes attributes; pClose
		| TPtr (typ, attributes) -> out "TPtr("; pTyp typ; comma; pAttributes attributes; pClose
		| TArray (typ, exp, attributes) -> out "TArray("; pTyp typ; comma; pOption pExp exp; comma; pAttributes attributes; pClose
		| TFun (typ, params, variadic, attributes) -> out "TFun("; pTyp typ; comma; pOption (pList (pTriple pString pTyp pAttributes)) params; comma; pBool variadic;comma; pAttributes attributes; pClose
		| TNamed (typeinfo, attributes) -> out "TNamed("; pTypeinfo typeinfo; comma; pAttributes attributes; pClose
		| TComp (compinfo, attributes) -> out "TComp("; pCompinfo compinfo; comma; pAttributes attributes; pClose
		| TEnum (enuminfo, attributes) -> out "TEnum("; pEnuminfo enuminfo; comma; pAttributes attributes; pClose
		| TBuiltin_va_list (attributes) -> out "TBuiltin_va_list("; pAttributes attributes; pClose

	and pTypeinfo(typeinfo:typeinfo):unit = 
		cOpen;
		out "tname="; typeinfo.tname; semicolon;
		out "ttype="; pTyp typeinfo.ttype; semicolon;
		out "treferenced="; pBool typeinfo.treferenced;
		cClose

	and pLocation(location:location):unit =
		cOpen;
		out "line="; pInt location.line; semicolon;
		out "file="; pString location.file; semicolon;
		out "byte="; pInt location.byte;
		cClose

	and pGlobal(global:global):unit = 
		match global with
			GType(typeinfo, location) -> out "GType("; pTypeinfo typeinfo; comma; pLocation location; pClose
		| GCompTag(compinfo, location) -> out "TCompTag("; pCompinfo compinfo; comma; pLocation location; pClose
		| GCompTagDecl(compinfo, location) -> out "GCompTagDecl("; pCompinfo compinfo; comma; pLocation location; pClose
		| GEnumTag(enuminfo, location) -> out "GEnumTag("; pEnuminfo enuminfo; comma; pLocation location; pClose
		| GEnumTagDecl(enuminfo, location) -> out "GEnumTagDecl("; pEnuminfo enuminfo; comma; pLocation location; pClose
		| GVarDecl(varinfo, location) -> out "GVarDecl("; pVarinfo varinfo; comma; pLocation location; pClose
		| GVar(varinfo, initinfo, location) -> out "GVar("; pVarinfo varinfo; comma; pInitinfo initinfo; comma; pLocation location; pClose
		| GFun(fundec, location) -> out "GFun("; pFundec fundec; comma; pLocation location; pClose
		| GAsm(string, location) -> out "GAsm("; pString string; comma; pLocation location; pClose
		| GPragma(attribute, location) -> out "GPragma("; pAttribute attribute; comma; pLocation location; pClose
		| GText(string) -> out "GText("; pString string; pClose

	and pFundec(fundec:fundec):unit = 
		cOpen;
		out "svar="; pVarinfo fundec.svar; semicolon;
		out "sformals="; pList pVarinfo fundec.sformals; semicolon;
		out "slocals="; pList pVarinfo fundec.slocals; semicolon;
		out "smaxid="; pInt fundec.smaxid; semicolon;
		out "sbody="; pBlock fundec.sbody; semicolon;
		out "smaxstmtid="; pOption pInt fundec.smaxstmtid; semicolon;
		out "sallstmts="; pList pStmt fundec.sallstmts;
		cClose

	and pFile(file:file):unit =
		cOpen;
		out "globals	="; pList pGlobal file.globals; semicolon;
		out "globinit	="; pOption pFundec file.globinit; semicolon;
		out "globinitcalled	="; pBool file.globinitcalled;
		cClose
	
	in
	let s:string = pFile file
	in Printf.printf "%s\n" s

let feature : featureDescr = { 
	fd_name = "printast";
	fd_enabled = ref false;
	fd_description = "print the AST of a source file";
	fd_extraopt = [];
	fd_doit = doit;
	fd_post_check = false;
} 
