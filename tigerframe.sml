(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

(* IMPORTANT:
		"Many source languages do not have nested function declarations; thus, 
		tigerframe should not know anything about static links. Instead, its 
		resposability of tigertrans. tigertrans knows that each frame contains 
		a static link. The static link is passed to a function in a register 
		and stored into the frame. Since the static link behaves so much like 
		a formal parameter, we will treat it as one (as much as possible). We 
		know that the static link escapes: it needs to be kept in memory" - 
		page 142 
*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int			(* nivel de anidamiento *)
datatype access = InFrame of int | InReg of tigertemp.label

val fp = "FP"				(* frame pointer *)
val sp = "SP"				(* stack pointer *)
val rv = "RV"				(* return value  *)
val ov = "OV"				(* overflow value (edx en el 386) *)

val wSz = 4					(* word size in bytes *)
val log2WSz = 2				(* base two logarithm of word size in bytes *)

val fpPrev = 0				(* offset (bytes) from fp *)
val fpPrevLev = 8			(* offset (bytes) from fp *)

val argsInicial = 0			(* words *) (* initial number of arguments *)
val localsInicial = 0		(* words *) (* initial number of local variables *)
val regInicial = 1			(* reg *)

val argsOffInicial = 0		(* words *)
val accessListInitial = [InFrame (fpPrevLev)] (* Only SL access *)

val argsGap = wSz			(* bytes *)
val localsGap = ~4 			(* bytes *)

val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = []
val callersaves = []
val calleesaves = []

type register = string

type frame = {
	name: tigertemp.label,		(* Name of the function corresponding to the frame *)
	formals: access list ref,	(* Access list of the function corresponding arguments *)
	locals: bool list,			(* List of booleans corresponding to escaped, or not escaped, function's local variables *)
	actualArg: int ref,			(* Total number of the function's arguments *)
	actualLocal: int ref,		(* Total number of the function's local variables in memory *)
	actualReg: int ref 			(* Maybe it is not used... *)
}

datatype frag = PROC of {body: tigertree.stm, frame: frame}
				| STRING of tigertemp.label * string

(* newFrame : {name: tigertemp.label, formals: bool list} -> frame *)
fun newFrame{name, formals} = {
	name=name,
	formals=ref accessListInitial, (* Each frame contains a SL *)
	locals=[],
	actualArg=ref argsInicial,
	actualLocal=ref localsInicial,
	actualReg=ref regInicial
}

(* name : frame -> tigertemp.label *)
fun name(f: frame) = #name f

(* string : tigertemp.label * string -> string *)
fun string(l, s) = l^tigertemp.makeString(s)^"\n"

(* formals : frame -> access list *)
fun formals({formals=f, ...}: frame) = !f

(* maxRegFrame : frame -> int *)
fun maxRegFrame(f: frame) = !(#actualReg f)

(* 	Alloca un nuevo argumento. b indica si se retorna InReg (caso false) 
	o InFrame (caso true) 

	allocArg : frame -> bool -> access 
*)

(* printAL : access list -> string *)
fun printAL al = 
	let
		fun printAc [] = [""]
			| printAc ((InFrame offset)::[]) = ("InFrame "^(Int.toString offset)) :: (printAc [])
			| printAc ((InReg temporary)::[]) = ("InReg "^temporary) :: (printAc [])
			| printAc ((InFrame offset)::ac) = ("InFrame "^(Int.toString offset)^", ") :: (printAc ac)
			| printAc ((InReg temporary)::ac) = ("InReg "^temporary^", ") :: (printAc ac)
		val al' = printAc al
	in
		"["^concat al'^"]"
	end

fun allocArg (f: frame) b = 
	let
		val a = case b of
			true =>
				let	
					val ret = InFrame(!(#actualLocal f)+localsGap) (* reserve space in memory *)
				in	
					#actualLocal f := (!(#actualLocal f)-1); ret 
				end
			| false => InReg(tigertemp.newtemp()) (* reserve temporary register *)

		val _ = #formals f := (!(#formals f)) @ [a]

		(* DEBUGGING *)
		val _ = print("\n**DEBUGGING from tigerframe.allocArg(). Function name:"^(name f))
		val _ = print("\nNumber of formals = "^Int.toString(List.length(!(#formals f))))
		val _ = print("\nAccess list = "^printAL (!(#formals f))^"\n")
	in
		a
	end

(* 	Alloca una nueva variable local. b indica si se retorna InReg (caso false) o 
	InFrame (caso true) 

	allocLocal : frame -> bool -> access 
*)
fun allocLocal (f: frame) b = 
	case b of
		(* local var escapes *)
		true =>
			let	
				val ret = InFrame(!(#actualLocal f)+localsGap) (* reserve space in memory *)
			in	
				#actualLocal f := (!(#actualLocal f)-1); ret 
			end
		(* local var does not escape *)
		| false => InReg(tigertemp.newtemp()) (* reserve temporary register *)

(* 	
	(Used by tigertrans to turn a tigerframe.access into the 
	Tree expression)
	For an access a such as InFrame(k), we have: 
	exp(a) (TEMP(fp)) = MEM(BINOP(PLUS, TEMP(fp), CONST(k)))
	"Why bother to pass the tree exp TEMP(fp) as an argument? 
	Because the address of the frame is the same as the current 
	frame pointer ONLY when accessing the variable from its own 
	level. When accessing a (access argument) from an inner-
	nested function, the frame address must be calculated using 
	static links, and the result of this calculation will be 
	tigertree.exp argument to tigerframe.exp. 
	If a is a register access such as InReg(t), then the frame-
	address argument to tigerframe.exp will be discarded, and 
	the result will be simply TEMP t" - page 156 
*)
(* exp : access -> tigertree.exp -> tigertree.exp *)
fun exp(InFrame k) e = MEM(BINOP(PLUS, e, CONST k))
	| exp(InReg l) _ = TEMP l

fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 (frame, body) = body
	
end