structure tigercodegen :> tigercodegen 
struct

(* NOTE: instruction selection of x86_64 architecture (AT&T syntax) *)

open tigerassem
open tigertree

fun intToString i = if i < 0 then "-"^Int.toString(~i) else Int.toString(i)

(* codegen : tigerframe.frame -> tigertree.stm -> tigerassem.instr list *)
(* Assuming that 'stm' parameter is canonized *)
fun codegen frame (stm: tigertree.stm) =
	let
		val ilist = ref ([] : instr list) 			(* list of assembly-lang instructions *)
		fun emit instr = (ilist := instr::(!ilist)) (* Accumulates a list of instructions to 
													be returned later *)
		fun generateTmp gen = 
			let
				val t = tigertemp.newtemp()
			in
				(gen t; t)
			end

		(* munchStm : tigertree.stm -> unit *)
		(* When MEM is used as the left child of a MOVE, it means "store", 
		but anywhere else it means "fetch" *)
		(* - Move to memory location cases, like:
		MOVE(MEM(e1), e2): Evaluate e1, yielding address a. Then evaluate 
		e2, and store the result into wordSize bytes of memory starting at 
		a *)
		fun munchStm(MOVE(MEM(BINOP(PLUS, e1, CONST i)), e2)) = 
				emit(OPER{
						assem="movq `s0, "^intToString i^"(`s1)\n",
						src=[munchExp e2, munchExp e1],
						dst=[],
						jump=NONE
					})

			| munchStm(MOVE(MEM(BINOP(PLUS, CONST i, e1)), e2)) =
				emit(OPER{
						assem="movq `s0, "^intToString i^"(`s1)\n",
						src=[munchExp e2, munchExp e1],
						dst=[],
						jump=NONE
					})

			| munchStm(MOVE(MEM(CONST i), e)) =
				emit(OPER{
						assem="movq `s0, "^intToString i^"($0)\n",
						src=[munchExp e],
						dst=[],
						jump=NONE
					})

			| munchStm(MOVE(MEM e1, e2)) =
				emit(OPER{
						assem="movq `s0, 0(`s1)\n",
						src=[munchExp e2, munchExp e1],
						dst=[],
						jump=NONE
					})

			(* Move to temporary cases, like:
			MOVE(TEMP t, e): Evaluate e and move it to temporary t *)
			| munchStm(MOVE(TEMP t, MEM(BINOP(PLUS, CONST i, e)))) =
				emit(OPER{
						assem="movq "^intToString i^"(`s0), `d0\n",
						src=[munchExp e],
						dst=[t],
						jump=NONE
					})

			| munchStm(MOVE(TEMP t, MEM(BINOP(PLUS, e, CONST i)))) =
				emit(OPER{
						assem="movq "^intToString i^"(`s0), `d0\n",
						src=[munchExp e],
						dst=[t],
						jump=NONE
					})

			| munchStm(MOVE(TEMP t, MEM e)) =
				emit(OPER{
						assem="movq (`s0), 'd0\n",
						src=[munchExp e],
						dst=[t],
						jump=NONE
					})

			| munchStm(MOVE(TEMP t, CONST i)) =
				emit(OPER{
						assem="movq $"^intToString i^", `d0\n",
						src=[],
						dst=t,
						jump=NONE
					})

			| munchStm(MOVE(TEMP t, NAME l)) =
				emit(OPER{
						assem="movq $"^l^", `d0\n",
						src=[],
						dst=[t],
						jump=NONE
					})

			| munchStm(MOVE(TEMP t1, TEMP t2)) =
				emit(OPER{
						assem="movq `s0, `d0\n",
						src=[t2],
						dst=[t1],
						jump=NONE
					})

			| munchStm(MOVE(TEMP t, e)) =
				emit(tigerassem.MOVE{
						assem="movq `s0, `d0\n",
						src=[munchExp e],
						dst=[t],
						jump=NONE
					})

			| munchStm(MOVE(_, _)) = raise Fail "Error - munchStm(): MOVE inválido a un no temporario/memoria"

			| munchStm(LABEL lab) =
				emit(tigerassem.LABEL{
						assem=lab^":\n",
						lab=lab
					})

			(* This case should not happen because of canonization... *)
			| munchStm(SEQ(s1, s2)) =
				(munchStm s1; munchStm s2)

			| munchStm(JUMP(NAME n, labs)) = 
				emit(OPER{
						assem="jmp "^n^"\n",
						src=[],
						dst=[],
						jump=SOME labs
					})

			| munchStm(JUMP _) = raise Fail "Error - munchStm(): JUMP sin label"

			| munchStm(CJUMP(relop, e1, e2, l1, l2)) =
				let
					val relInstr = case relop of
						EQ => "je"
						| NE => "jne"
						| LT => "jl"
						| GT => "jg"
						| LE => "jle"
						| GE => "jge"
						| ULT => "jb"
						| ULE => "jbe"
						| UGT => "jae"
						| UGE => "jge"

					val _ = emit(OPER{
							assem="cmpq `s0 `s1\n",
							src=[munchExp e1, munchExp e2],
							dst=[],
							jump=NONE
						})
				in
					emit(OPER{
							assem=relInstr^" "^l1^"\n",
							src=[],
							dst=[],
							jump=SOME [l1, l2]
						})
				end

			(*
				To invoke a function, we must first compute the arguments and place the in the 
				desired registers (or memory stack). Then, we must push the two caller-saved 
				registers on the stack, to save their values. We then issue the CALL instr, 
				which pushes the current instr pointer on to the stack then jumps to the code 
				location of the function. Upon return of the function, we pop the two caller-
				saved registers off of the stack, and look for the return value of the 
				function in the %rax register
			*)
			| munchStm(EXP(CALL(NAME f, args))) =
				let
					val diff = length args - length (tigerframe.argregs)
				in
					(saveCallerSaves();
					emit(OPER{
							assem="call "^f^"\n",
							src=munchArgs args,
							dst=tigerframe.calldefs
							jump=NONE
						});
					if diff > 0 
					then 
						emit(OPER{
								assem="addq $"^intToString(diff*(tigerframe.wSz))^", %rsp",
								src=[],
								dst=[],
								jump=NONE
							}) 
					else 
						();
					restoreCallerSaves())
				end

			| munchStm(EXP(CALL _)) = raise Fail "Error - munchStm(): CALL sin label"

			| munchStm(EXP e) = (munchExp e; ())	(* Evaluate e and discard the result *)

		and fun saveCallerSaves() =
					map (fn reg => OPER{
							assem="pushq `s0 # SAVE CALLER-SAVED REGISTER\n",
							src=[reg],
							dst=[],
							jump=NONE
						}) tigerframe.callersaves

		and fun restoreCallerSaves() =
					map (fn reg => 
						OPER {
							assem="popq `s0 # RESTORE CALLER-SAVED REGISTER\n",
							src=[reg],
							dst=[],
							jump=NONE
						}) tigerframe.callersaves

		and fun munchArgs args =
					let
						fun munchArgsReg [] _ = []
							| munchArgsReg ass [] = munchArgsStack (rev ass)	(* Runned out of registers to allocate arguments, so store them in memory applying C calling convention *)
							| munchArgsReg (a::ass) (r::rss) = 
								let
									val _ = munchStm(MOVE(TEMP r, a))			(* Move argument to register *)
								in
									r::(munchArgsReg ass rss)
								end

						fun munchArgsStack [] = []
							| munchArgsStack (a::ass) = case a of
								CONST i => emit(OPER{
										assem="pushq $"^intToString i^"\n",
										src=[],
										dst=[],
										jump=NONE
									})
								| NAME l => emit(OPER{
										assem="pushq $"^l^"\n",
										src=[],
										dst=[],
										jump=NONE
									})
								| TEMP t => emit(OPER{
										assem="pushq `s0\n",
										src=[t],
										dst=[],
										jump=NONE
									})
								| MEM(TEMP t) => emit(OPER{
										assem="pushq (`s0)\n",
										src=[t],
										dst=[],
										jump=NONE
									})
								| MEM(BINOP(PLUS, e, CONST i)) => emit(OPER{
										assem="pushq "^intToString i^"(`s0)\n",
										src=[munchExp e],
										dst=[],
										jump=NONE
									})
								| MEM(e) => emit(OPER{
										assem="pushq (`s0)\n",
										src=[munchExp e],
										dst=[],
										jump=NONE
									})
								| _ => raise Fail "Error - munchArgs()"
					in
						munchArgsReg args tigerframe.argregs					(* Attempt to store function's arguments at machine available registers if possible *)
					end

		(* Generate aassembly code to evaluate the input expression tree, then 
			returns the register with the final result in it *)
		(* munchExp : tigertree.stm -> tigertemp.temp *)
		and munchExp(MEM(BINOP(PLUS, e, CONST i))) =
				generateTmp(fn r => 
					emit(OPER{
							assem="addq "^intToString i^"(`s0)\n",
							src=[munchExp e],
							dst=[r],
							jump=NONE
						}))	

			| munchExp(MEM(BINOP(PLUS, CONST i, e))) =
				generateTmp(fn r => 
					emit(OPER{
							assem="addq "^intToString i^"(`s0)\n",
							src=[munchExp e],
							dst=[r],
							jump=NONE
						}))

			| munchExp(MEM(e)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, MEM e)))

			| munchExp(CONST i) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, CONST i)))

			| munchExp(TEMP t) = t

			| munchExp(NAME n) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, NAME n)))

			| munchExp(CALL _) = raise Fail "Error - munchExp(): CALL no debería aparecer después de canon"

			| munchExp(ESEQ _) = raise Fail "Error - munchExp(): ESEQ no debería aparecer después de canon"

			(* NOTE: Some x86_64 have arithmetic instructions with two operands, where one 
				of the operands is both a source and a destination *)

			(* PLUS cases *)

			| munchExp(BINOP(PLUS, CONST i, e)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, e));
					emit(OPER{
							assem="addq $"^intToString i^", `d0\n"
							src=[r],
							dst=[r],
							jump=NONE
						}))

			| munchExp(BINOP(PLUS, NAME l, e)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, e));
					emit(OPER{
							assem="addq $"^l^", `d0\n",
							src=[r],
							dst=[r],
							jump=NONE
						}))

			| munchExp(BINOP(PLUS, TEMP t, e)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, e));
					emit(OPER{
							assem="addq `s1, `d0\n",
							src=[r, t],
							dst=[r],
							jump=NONE
						}))

			| munchExp(BINOP(PLUS, MEM(TEMP t), e)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, e));
					emit(OPER{
							assem="addq (`s1), `d0\n",
							src=[r, t],
							dst=[r],
							jump=NONE
						}))

			| munchExp(BINOP(PLUS, MEM(BINOP(PLUS, CONST i, TEMP t)), e)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, e));
					emit(OPER{
							assem="addq "^intToString i^"(`s1), `d0\n",
							src=[r, t],
							dst=[r],
							jump=NONE
						}))

			| munchExp(BINOP(PLUS, MEM(BINOP(PLUS, TEMP t, CONST i)), e)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, e));
					emit(OPER{
							assem="addq "^intToString i^"(`s1), `d0\n",
							src=[r, t],
							dst=[r],
							jump=NONE
						}))

			| munchExp(BINOP(PLUS, MEM e1, e2)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, e2));
					emit(OPER{
							assem="addq (`s1), `d0\n",
							src=[r, munchExp e1],
							dst=[r],
							jump=NONE
						}))

			| munchExp(BINOP(PLUS, _, e)) =
				generateTmp(fn r => 
					munchStm(MOVE(TEMP r, e));
					emit(OPER{
							assem="addq `s1, `d0\n",
							src=[r, munchExp e],
							dst=[r],
							jump=NONE
						}))


			(* MINUS cases *)

			| munchExp(BINOP(MINUS, e, CONST i))


	in
		(munchStm stm;
		rev (!ilist))
	end
	
end