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

type level = int

datatype access = InFrame of int 
				| InReg of tigertemp.label

val fp = "rbp"				(* frame pointer register *)
val sp = "rsp"				(* stack pointer register *)
val rv = "rax"				(* return value register; also used in idiv and imul instructions *)
val rax = "rax"
val rdx = "rdx"				(* third argument register; also used in idiv and imul instructions *)

val wSz = 8					(* word size in bytes *)

val fpPrev = 0				(* offset (bytes) from fp *)
val fpPrevLev = 16			(* offset (bytes) from fp *)

val argsInitial = 0			(* initial number of arguments *)
val localsInitial = 0		(* initial number of local variables *)

val argsOffInicial = 0		(* words *)
val accessListInitial = [InFrame (fpPrevLev)] (* Just the static link access *)

val argsGap = wSz
val localsGap = ~8 			(* bytes *)

val specialregs = [rv, fp, sp]									(* special purpose registers *)
val argregs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]			(* used to pass first 6 parameters to called functions *)
val argregsNum = length argregs
val callersaves = ["r10", "r11"] 								(* registers that must be preserved by the caller *)
val calleesaves = ["rbx", "rbp", "r12", "r13", "r14", "r15"] 	(* registers that must be saved across function calls *)
val calldefs = [rv] 
				@ callersaves

val registers = specialregs 
				@ argregs 
				@ callersaves 
				@ callersaves	(* all machine registers *)

type register = string

type frame = {
	name: tigertemp.label,		(* Name of the function corresponding to the frame *)
	formals: access list ref,	(* Access list of the function corresponding arguments *)
	actualLocal: int ref		(* Total number of the function's local variables *)
}

datatype frag = PROC of {body: tigertree.stm, frame: frame}
				| STRING of tigertemp.label * string

(* newFrame : {name: tigertemp.label, formals: bool list} -> frame *)
fun newFrame{name, formals} = {
	name=name,
	formals=ref accessListInitial,
	actualLocal=ref localsInitial
}

(*
fun newFrame{name, formals} =
	let
		val f : frame = {
			name=name,
			formals=ref accessListInitial,
			actualLocal=ref localsInitial
		}

		fun allocArg _ [] = []
			| allocArg n (e::es) = (if n < argregsNum 
										then 
											allocLocal f e 
										else 
											allocLocal f true) :: (allocArg (n+1) es)
	in
		{
			name = #name f,
			formals = (!(#formals f)) @ (allocArg 0 formals),
			actualLocal = #actualLocal f
		}
	end
*)

(* name : frame -> tigertemp.label *)
fun name(f: frame) = #name f

(* string : tigertemp.label * string -> string *)
fun string(l, s) = l^tigertemp.makeString(s)^"\n"

(* formals : frame -> access list *)
fun formals({formals=f, ...}: frame) = !f

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
					val ret = InFrame(!(#actualLocal f)+localsGap)
				in	
					#actualLocal f := (!(#actualLocal f)-1); ret 
				end
			| false => InReg(tigertemp.newtemp())

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
		true =>
			let	
				val ret = InFrame(!(#actualLocal f)+localsGap)	(* local var escapes *)
			in	
				#actualLocal f := (!(#actualLocal f)-1); ret 
			end
		| false => InReg(tigertemp.newtemp())					(* local var does not escape *)

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

	exp : access -> tigertree.exp -> tigertree.exp
*)
fun exp(InFrame k) e = MEM(BINOP(PLUS, e, CONST k))
	| exp(InReg l) _ = TEMP l

fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 ({formals, ...} : frame, body) = body

(* Tell that certain registers are live at procedure exit *)
fun procEntryExit2 (frame, body) = 
	[tigerassem.OPER{
		assem="",
		src=[],
		dst=[sp, fp] @ calleesaves,
		jump=SOME []
	}]
	@ body
	@ [tigerassem.OPER{
		assem="",
		src=[sp, fp] @ calleesaves,
		dst=[],
		jump=SOME []
	}]
	
end