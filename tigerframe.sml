(*
	x64 frame architecture:

		|    argn    |	fp+8*(n+1)
		|    ...     |
		|    arg8    |	fp+32
		|    arg7    |	fp+24
		|	 arg6    |  fp+16
		|  retorno   |	fp+8
		|   fp ant   |	fp
		--------------	fp
		|   s-link   |	fp-8
		|   local1   |	fp-16
		|    ...     |
		|   localn   |	fp-8*n


		rdi -> static link
		rsi -> arg1
		rdx -> arg2
		rcx -> agr3
		r8  -> arg4
		r9  -> arg5
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

structure tigerframe :> tigerframe = 
struct

	open tigertree

	type level = int

	datatype access = InFrame of int 
					| InReg of tigertemp.label

	fun getOffsetFromAccess (InFrame offset) = offset
		| getOffsetFromAccess _ = raise Fail "Error - frame. getOffsetFromAccess(): no debería pasar"

	fun getLabelFromAccess (InReg label) = label
		| getLabelFromAccess _ = raise Fail "Error - frame. getLabelFromAccess(): no debería pasar"

	(* 16 64-bits long general-purpose registers *)
	val rax = "%rax"	(* result register; also used in idiv and imul instructions *)
	val rbx = "%rbx"	(* miscellaneous register *)
	val rcx = "%rcx"	(* 4th argument register *)
	val rdx = "%rdx"	(* 3rd argument register; also used in idiv and imul instructions *)
	val rsp = "%rsp"	(* stack pointer register *)
	val rbp = "%rbp"	(* frame pointer register *)
	val rsi = "%rsi"	(* 2nd argument register *)
	val rdi = "%rdi"	(* 1st argument register *)
	val r8 	= "%r8"		(* 5th argument register *)
	val r9 	= "%r9"		(* 6th argument register *)
	val r10 = "%r10"	(* miscellaneous register *)
	val r11 = "%r11"	(* miscellaneous register *)
	val r12 = "%r12"	(* miscellaneous register *)
	val r13 = "%r13"	(* miscellaneous register *)
	val r14 = "%r14"	(* miscellaneous register *)
	val r15 = "%r15"	(* miscellaneous register *)

	val fp = rbp				(* frame pointer register *)
	val sp = rsp				(* stack pointer register *)
	val rv = rax				(* return value register; also used in idiv and imul instructions *)

	val wSz = 8					(* word size in bytes *)

	val fpPrev = 0				(* offset (bytes) from fp *)
	val fpPrevLev = ~wSz		(* offset (bytes) from fp *)

	val localsInitial = 1		(* initial number of local variables (considering static link) *)

	val accessListInitial = [InFrame (fpPrevLev)] (* Static link access *)

	val localsGap = wSz

	val specialregs = [fp, sp]						(* special purpose registers *)
	val argregs = [rdi, rsi, rdx, rcx, r8, r9]		(* used to pass first 6 parameters to called functions *)
	val callersaves = [r10, r11]					(* registers that must be preserved by the caller *)
	val calleesaves = [rbx, r12, r13, r14, r15]		(* registers that must be saved across function calls *)
	val calldefs = [rv] 
					@ callersaves

	val registers = [rv]
					@ specialregs 
					@ argregs 
					@ callersaves 
					@ calleesaves	(* all machine registers *)

	val argregsNum = List.length argregs

	type register = string

	type frame = {
		name: tigertemp.label,		(* Name of the function corresponding to the frame *)
		formals: access list ref,	(* Access list of the function corresponding arguments *)
		actualLocal: int ref		(* Total number of the function's local variables *)
	}

	datatype frag = PROC of {body: tigertree.stm, frame: frame}
					| STRING of tigertemp.label * string

	(* getNumOfMachineRegisters : unit -> int *)
	fun getNumOfMachineRegisters() = List.length registers

	(* newFrame : {name: tigertemp.label, formals: bool list} -> frame *)
	fun newFrame{name, formals} = {
		name=name,
		formals=ref accessListInitial,
		actualLocal=ref localsInitial
	}

	(* name : frame -> tigertemp.label *)
	fun name(f: frame) = #name f

	(* string : tigertemp.label * string -> string *)
	fun string(l, s) = l^tigertemp.makeString(s)^"\n"

	(* formals : frame -> access list *)
	fun formals({formals=f, ...}: frame) = !f

	(* allocArg : frame -> bool -> access *)
	fun allocArg (f: frame) b =
		case b of
			true => let
					 	val ret = InFrame (~(!(#actualLocal f)*wSz + localsGap))
					 	val _ = #actualLocal f := (!(#actualLocal f) + 1)
					 	val _ = #formals f := (!(#formals f)) @ [ret]
					 in
					 	ret
					 end 
			| false => let
							val ret = InReg (tigertemp.newtemp())
							val _ = #formals f := (!(#formals f)) @ [ret]
						in
							ret
						end

	(* accessToString : access -> string *)
	fun accessToString (InFrame offset) = "InFrame "^(Int.toString offset)
		| accessToString (InReg temporary) = "InReg "^temporary	

	(* allocLocal : frame -> bool -> access *)
	fun allocLocal (f: frame) b = 
		case b of
			true =>
				let	
					val ret = InFrame (~(!(#actualLocal f)*wSz + localsGap))
				in	
					#actualLocal f := (!(#actualLocal f) + 1); ret 
				end
			| false => InReg (tigertemp.newtemp())

	(* 	
		Turns a tigerframe.access into Tree expression.
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

	(* externalCall : string * tigertree.exp list -> tigertree.exp *)
	fun externalCall(s, l) = CALL(NAME s, l)

	(* seq : tigertree.stm list -> tigertree.stm *)
	fun seq [] = EXP (CONST 0)
		| seq [s] = s
		| seq (x::xs) = SEQ (x, seq xs)

	(* procEntryExit1 : frame * tigertree.stm -> tigertree.stm *)
	fun procEntryExit1 (f as {formals, ...} : frame, body) = 
		let
			(* body : tigertree.stm
			If function returns a value, "body" is a statement that moves the 
			indicated value to RV register *)

			(* DEBUGGING *)
			(*val _  = print("\n**DEBUGGING from tigerframe.procEntryExit1():\nformals list of "^name f^" = "^listToString (!formals) (fn x => x)^"\n")*)

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
			val parametersMoves' = 
				let
					fun calculateNewOffset offset = offset*wSz + 2*wSz

					val formals' = List.drop(!formals, argregsNum)
					val tabs = List.tabulate(List.length formals', fn x => x)
					val accOff = ListPair.zip(formals', tabs)
					val accOff' = List.map (fn (access, offset) => 
						(access, calculateNewOffset offset)) accOff
				in
					map (fn (access, offset) => 
					case access of
						InReg r => MOVE(TEMP r, exp (InFrame offset) (TEMP fp))
						| InFrame offset' => MOVE(exp (InFrame offset') (TEMP fp), exp (InFrame offset) (TEMP fp))) accOff'
				end
				handle Subscript => []

			(* Store instructions for saving and restoring callee-saves *)
			val (saveCalleesavesMoves, restoreCalleesavesMoves) = foldl
		          (fn (t, (a, b)) => let val fresh = tigertemp.newtemp()
		                              in (MOVE (TEMP fresh, TEMP t) :: a,
		                                  MOVE (TEMP t, TEMP fresh) :: b) end)
		          ([], []) calleesaves

		in
			seq(parametersMoves
				@ parametersMoves'
				@ saveCalleesavesMoves
				@ [body]
				@ restoreCalleesavesMoves)
		end

	(* Tell that certain registers are live at procedure exit *)
	fun procEntryExit2 (frame, body) = 
		let
			val exitLab = "EXIT_LAB"
		in
			body
			@ [tigerassem.OPER{
				assem="\n",
				src=[rv] @ calleesaves,
				dst=[],
				jump=NONE
			}]
			@ [tigerassem.LABEL{
				assem=exitLab^":\n",
				lab=exitLab
			}]
		end

	(* val procEntryExit3 : frame -> string -> string *)
	(* SCAFFOLD version... *)
	fun procEntryExit3 (f as {name, ...} : frame) body =
		let
			val prolog = "PROCEDURE "^name^"\n"
			val body = body^"\n"
			val epilog = "END "^name^"\n"
		in
			concat [
				prolog,
				body,
				epilog
			]
		end

end