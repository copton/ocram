open Cil

let (%) = Printf.sprintf

let pBool(value:bool): string = if (value) then "true" else "false"

let pOption(print:'a->string)(value: 'a option):string =
	match value with
		Some(x) -> ("Some(%s)" % (print x))
	| None -> "None"

let pList(print:'a->string)(values: 'a list): string =
	"[" ^ (String.concat "; " (List.map print values)) ^ "]"

let pString(s:string):string = "\"" ^ s ^ "\""

let pInt = string_of_int

let pInt64 = Int64.to_string

let pFloat = string_of_float

let pChar(char:char) = "'" ^ (String.make 1 char) ^ "'"

let pTuple(printa:'a->string)(printb:'b->string)(tuple:('a * 'b)):string =
	("(%s, %s)" % (printa (fst tuple))) (printb (snd tuple))

let pTriple(printa:'a->string)(printb:'b->string)(printc:'c->string)(triple:('a*'b*'c)):string =
	match triple with
		(a,b,c) -> ("(%s, %s, %s)" % (printa a))(printb b)(printc c)

let rec pAttributes(attributes:attributes):string = 
	pList pAttribute attributes

and pAttribute(attribute:attribute):string = "_" (* TODO *)

and pIkind(ikind:ikind):string =
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

and pFkind(fkind:fkind):string = 
	match fkind with
		FFloat -> "FFloat"
	| FDouble -> "FDouble"
	| FLongDouble -> "FLongDouble"

and pBinop(binop:binop):string =
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

and pUnop(unop:unop):string = 
	match unop with
		Neg -> "Neg"
	| BNot -> "BNot"
	| LNot -> "LNot"

and pConstant(constant:constant):string = 
	match constant with
		CInt64(int64, ikind, string) -> ("CInt64(%s, %s, %s)" % (pInt64 int64)) (pIkind ikind) (pOption pString string)
	| CStr(string) -> "CStr(%s)" % (pString string)
	| CWStr(int64s) -> "CWStr(%s)" % (pList pInt64 int64s)
	| CChr(char) -> "CChr(%s)" % (pChar char)
	| CReal(float, fkind, string) -> ("CReal(%s, %s, %s)" % (pFloat float)) (pFkind fkind) (pOption pString string)
	| CEnum(exp, string, enuminfo) -> ("CEnum(%s, %s, %s)" % (pExp exp)) (pString string) (pEnuminfo enuminfo)

and pExp(exp:exp):string = 
	match exp with
		Const(constant) -> "Const(%s)" % (pConstant constant)
	| Lval(lval) -> "Lval(%s)" % (pLval lval)
	| SizeOf(typ) -> "SizeOf(%s)" % (pTyp typ)
	| SizeOfE(exp) -> "SizeOfE(%s)" % (pExp exp)
	| SizeOfStr(str) -> "SizeOfStr(%s)" % (pString str)
	| AlignOf(typ) -> "AlignOf(%s)" % (pTyp typ)
	| AlignOfE(exp) -> "AlignOfE(%s)" % (pExp exp)
	| UnOp(unop, exp, typ) -> ("UnOp(%s, %s %s)" % (pUnop unop)) (pExp exp) (pTyp typ)
	| BinOp(binop, exp1, exp2, typ) -> ("BinOp(%s, %s, %s, %s)" % (pBinop binop)) (pExp exp1) (pExp exp2) (pTyp typ)
	| CastE(typ, exp) -> ("CastE(%s, %s)" % (pTyp typ)) (pExp exp)
	| AddrOf(lval) -> "AddrOf(%s)" % (pLval lval)
	| StartOf(lval) -> "StartOf(%s)" % (pLval lval)

and pCompinfo(compinfo:compinfo):string =
	("{cstruct=%s; cname=%s; ckey=%s; cfields=%s; cattr=%s; cdefined=%s; creferenced=%s}" %
		(pBool compinfo.cstruct))
		(pString compinfo.cname)
		(pInt compinfo.ckey)
		(pList pFieldinfo compinfo.cfields)
		(pAttributes compinfo.cattr)
		(pBool compinfo.cdefined)
		(pBool compinfo.creferenced)

and pEnuminfo(enuminfo:enuminfo): string =
	("{ename=%s; eitems=%s; eattr=%s; ereferenced=%s; ekind=%s}" %
		(pString enuminfo.ename))
		(pList (pTriple pString pExp pLocation) enuminfo.eitems)
		(pAttributes enuminfo.eattr)
		(pBool enuminfo.ereferenced)
		(pIkind enuminfo.ekind)

and pStorage(storage:storage): string = 
	match storage with
		NoStorage -> "NoStorage"
	| Static -> "Static"
	| Register -> "Register"
	| Extern -> "Extern"

and pVarinfo(varinfo:varinfo):string = 
	("{vname=%s; vtype=%s; vattr=%s; vstorage=%s; vglob=%s; vinline=%s; vdecl=%s; vid=%s; vaddrof=%s; vreferenced=%s; vdescr=%s; vdescrpure=%s}" %
		(pString varinfo.vname))
		(pTyp varinfo.vtype)
		(pAttributes varinfo.vattr)
		(pStorage varinfo.vstorage)
		(pBool varinfo.vglob)
		(pBool varinfo.vinline)
		(pLocation varinfo.vdecl)
		(pInt varinfo.vid)
		(pBool varinfo.vaddrof)
		(pBool varinfo.vreferenced)
		("_") (* no support for Pretty.doc parts *)
		(pBool varinfo.vdescrpure)

and pInit(init:init):string = 
	match init with
		SingleInit(exp) -> "SingleInit(%s)" % (pExp exp)
	| CompoundInit(typ, inits) -> ("CompoundInit(%s, %s)" % (pTyp typ)) (pList (pTuple pOffset pInit) inits)

and pInitinfo(initinfo:initinfo):string = 
	"{init=%s}" % (pOption pInit initinfo.init)

and pFieldinfo(fieldinfo:fieldinfo):string =
	("{fcomp=%s; fname=%s; ftype=%s; fbitfield=%s; fattr=%s; floc=%s}" %
		(pCompinfo fieldinfo.fcomp))
		(pString fieldinfo.fname)
		(pTyp fieldinfo.ftype)
		(pOption pInt fieldinfo.fbitfield)
		(pAttributes fieldinfo.fattr)
		(pLocation fieldinfo.floc)

and pOffset(offset:offset):string = 
	match offset with
		NoOffset -> "NoOffset"
	| Field(fieldinfo, offset) -> ("Field(%s, %s)" % (pFieldinfo fieldinfo)) (pOffset offset)
	| Index(exp, offset) -> ("Index(%s, %s)" % (pExp exp)) (pOffset offset)

and pLhost(lhost:lhost):string = 
	match lhost with
		Var(varinfo) -> "Var(%s)" % (pVarinfo varinfo)
	| Mem(exp) -> "Mem(%s)" % (pExp exp)

and pLval(lval:lval) = pTuple pLhost pOffset lval

and pInstr(instr:instr):string = 
	match instr with
		Set(lval, exp, location) -> ("Set(%s, %s, %s" % (pLval lval)) (pExp exp) (pLocation location)
	| Call(lval, exp, exps, location) -> ("Call(%s, %s, %s, %s)" % (pOption pLval lval)) (pExp exp) (pList pExp exps) (pLocation location)
	| Asm _ -> "" (* no support for assembler *)

and pStmtkind(stmtkind:stmtkind):string = 
	match stmtkind with
		Instr(instr) -> "Instr(%s)" % (pList pInstr instr)
	| Return(exp, location) -> ("Return(%s, %s)" % (pOption pExp exp)) (pLocation location)
	| Goto(stmt, location) -> ("Goto(stmt, %s)(* %s *)" % (pLocation location)) (pList pLabel (!stmt).labels)
  | Break(location) -> "Break(%s)" % (pLocation location)
  | Continue(location) -> "Continue(%s)" % (pLocation location)
	| If(exp, block1, block2, location) -> ("If(%s, %s, %s, %s)" % (pExp exp)) (pBlock block1) (pBlock block2) (pLocation location)
	| Switch(exp, block, stmts, location) -> ("Switch(%s, %s, %s, %s)" % (pExp exp)) (pBlock block) (pList pStmt stmts) (pLocation location)
	| Loop(block, location, stmt1, stmt2) -> ("Loop(%s, %s, %s, %s)" % (pBlock block)) (pLocation location) (pOption pStmt stmt1) (pOption pStmt stmt2)
	| Block(block) -> "Block(%s)" % (pBlock block)
	| TryFinally _  -> "" (* not supporting C++ *)
	| TryExcept _ -> "" (* not supporting C++ *)

and pLabel(label:label):string = 
	match label with
		Label(name, location, flag) -> ("Label(%s, %s, %s)" % (pString name)) (pLocation location) (pBool flag)
	| Case(exp, location) -> ("Case(%s, %s)" % (pExp exp)) (pLocation location)
	| Default(location) -> "Default(%s)" % (pLocation location)

and pBlock(block:block):string = 
	("{battrs=%s; bstmts=%s}" % (pAttributes block.battrs)) (pList pStmt block.bstmts)

and pStmt(stmt:stmt): string = 
	("{labels=%s; skind=%s; sid=%s; succs=_; pred=_}" % (* pred and succs could lead to infinite recuresion (?) *)
		(pList pLabel stmt.labels))
		(pStmtkind stmt.skind)
		(pInt stmt.sid)

and pTyp(typ:typ):string = 
	match typ with
		TVoid (attributes) -> "TVoid(%s)" % (pAttributes attributes)
	| TInt (ikind, attributes) -> ("TInt(%s, %s)" % (pIkind ikind)) (pAttributes attributes)
	| TFloat (fkind, attributes) -> ("TFloat(%s, %s)" % (pFkind fkind)) (pAttributes attributes)
	| TPtr (typ, attributes) -> ("TPtr(%s, %s" % (pTyp typ)) (pAttributes attributes)
	| TArray (typ, exp, attributes) -> ("TArray(%s, %s, %s)" % (pTyp typ)) (pOption pExp exp) (pAttributes attributes)
	| TFun (typ, params, variadic, attributes) -> ("TFun(%s, %s, %s, %s)" % (pTyp typ)) (pOption (pList (pTriple pString pTyp pAttributes)) params) (pBool variadic) (pAttributes attributes)
	| TNamed (typeinfo, attributes) -> ("TNamed(%s, %s)" % (pTypeinfo typeinfo)) (pAttributes attributes)
	| TComp (compinfo, attributes) -> ("TComp(%s, %s)" % (pCompinfo compinfo)) (pAttributes attributes)
	| TEnum (enuminfo, attributes) -> ("TEnum(%s, %s)" % (pEnuminfo enuminfo)) (pAttributes attributes)
	| TBuiltin_va_list (attributes) -> ("TBuiltin_va_list(%s)" % (pAttributes attributes))

and pTypeinfo(typeinfo:typeinfo):string = 
	("{tname=\"%s\"; ttype=%s; treferenced=%s}" % typeinfo.tname) (pTyp typeinfo.ttype) (pBool typeinfo.treferenced)

and pLocation(location:location):string = 
	("{line=%s; file=%s; byte=%s}" % (pInt location.line)) (pString location.file) (pInt location.byte)

and pGlobal(global:global):string = 
	match global with
	  GType(typeinfo, location) -> ("GType(%s, %s)" % (pTypeinfo typeinfo)) (pLocation location)
	| GCompTag(compinfo, location) -> ("TCompTag(%s, %s)" % (pCompinfo compinfo)) (pLocation location)
	| GCompTagDecl(compinfo, location) -> ("GCompTagDecl(%s, %s)" % (pCompinfo compinfo)) (pLocation location)
	| GEnumTag(enuminfo, location) -> ("GEnumTag(%s, %s)" % (pEnuminfo enuminfo)) (pLocation location)
	| GEnumTagDecl(enuminfo, location) -> ("GEnumTagDecl(%s, %s)" % (pEnuminfo enuminfo)) (pLocation location)
	| GVarDecl(varinfo, location) -> ("GVarDecl(%s, %s)" % (pVarinfo varinfo)) (pLocation location)
	| GVar(varinfo, initinfo, location) -> ("GVar(%s, %s %s)" % (pVarinfo varinfo)) (pInitinfo initinfo) (pLocation location)
	| GFun(fundec, location) -> ("GFun(%s, %s)" % (pFundec fundec)) (pLocation location)
	| GAsm(string, location) -> ("GAsm(%s, %s)" % (pString string)) (pLocation location)
	| GPragma(attribute, location) -> ("GPragma(%s, %s)" % (pAttribute attribute)) (pLocation location)
	| GText(string) -> ("GText(%s)" % (pString string))

and pFundec(fundec:fundec):string = 
	("{svar=%s; sformals=%s; slocals=%s; smaxid=%s; sbody=%s; smaxstmtid=%s; sallstmts=%s}" % 
		(pVarinfo fundec.svar))
		(pList pVarinfo fundec.sformals)
		(pList pVarinfo fundec.slocals)
		(pInt fundec.smaxid)
		(pBlock fundec.sbody)
		(pOption pInt fundec.smaxstmtid)
		(pList pStmt fundec.sallstmts)

and pFile(file:file):string =
	("{fileName=\"%s\"; globals=%s; globinit=%s, globinitcalled=%s}" % file.fileName) 
			(pList pGlobal file.globals)
			(pOption pFundec file.globinit) 
			(pBool file.globinitcalled)

	
let doit(file:file) = 
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
