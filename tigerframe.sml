(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+8*(n+1)
		|    ...     |
		|    arg2    |	fp+32
		|    arg1    |	fp+24
		|	fp level |  fp+16
		|  retorno   |	fp+8
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-8
		|   local2   |	fp-4
		|    ...     |
		|   localn   |	fp-8*n
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

val localsInitial = 0		(* initial number of local variables *)
val argsInitial = 1			(* initial number of arguments (considering static link as an argument) *)

val argsOffInitial = 2		(* offset for the 1st argument in stack frame *)
val accessListInitial = [InFrame (fpPrevLev)] (* Static link access *)

val argsGap = wSz
val localsGap = ~wSz

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
	actualLocal: int ref,		(* Total number of the function's local variables *)
	argsCounter: int ref		(* Counter of incoming parameters *)
}

datatype frag = PROC of {body: tigertree.stm, frame: frame}
				| STRING of tigertemp.label * string

(* newFrame : {name: tigertemp.label, formals: bool list} -> frame *)
fun newFrame{name, formals} = {
	name=name,
	formals=ref accessListInitial,
	actualLocal=ref localsInitial,
	argsCounter=ref argsInitial
}

(* name : frame -> tigertemp.label *)
fun name(f: frame) = #name f

(* string : tigertemp.label * string -> string *)
fun string(l, s) = l^tigertemp.makeString(s)^"\n"

(* formals : frame -> access list *)
fun formals({formals=f, ...}: frame) = !f

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

(* allocArg : frame -> bool -> access *)
fun allocArg (f : frame) b =
	if b orelse !(#argsCounter f) > argregsNum 
	then 
		let
		 	val ret = InFrame ((!(#argsCounter f)+argsOffInitial)*wSz)
		 	val _ = #argsCounter f := !(#argsCounter f) + 1
		 	val _ = #formals f := (!(#formals f)) @ [ret]
		 in
		 	ret
		 end 
	else 
		let
			val ret = InReg (tigertemp.newtemp())
			val _ = #argsCounter f := !(#argsCounter f) + 1
			val _ = #formals f := (!(#formals f)) @ [ret]
		in
			ret
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

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

(* procEntryExit1 : frame * tigertree.stm -> tigertree.stm *)
fun procEntryExit1 ({formals, ...} : frame, body) = 
	let
		val fmlsLength = List.length (!formals)
		val fmlsRegisters = if fmlsLength > argregsNum 
								then 
									List.take(!formals, argregsNum) 
								else 
									!formals
		val regPairs = ListPair.zip (fmlsRegisters, argregs)

		(* For each incoming register parameter, move it to the place from which it's 
		seen from within the function - page 261 *)
		val parametersMoves = map (fn (access, argreg) => 
			case access of
				InReg r => MOVE(TEMP r, TEMP argreg)
				| InFrame offset => MOVE(exp (InFrame offset) (TEMP fp), TEMP argreg)) regPairs

		
		(* Store instructions for saving and restoring callee-saves *)
		val (saveCalleesaves, restoreCalleesaves) = foldl
	          (fn (t, (a, b)) => let val fresh = tigertemp.newtemp()
	                              in (MOVE (TEMP fresh, TEMP t) :: a,
	                                  MOVE (TEMP t, TEMP fresh) :: b) end)
	          ([], []) calleesaves

	in
		seq(parametersMoves
			@ saveCalleesaves
			@ [body]
			@ restoreCalleesaves)
	end

(* Tell that certain registers are live at procedure exit
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
	
*)
end