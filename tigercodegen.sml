structure tigercodegen :> tigercodegen =
struct

	(* NOTE: x86_64 instruction selection; AT&T syntax (GAS syntax)
	The main advantage of using this syntax is its compatibility with the GCC inline assembly syntax

	Remember: In the Tiger language, an int is a 64 bits long using sign convention

	The fundamental data types are: 
		- byte = 8 bits
		- word = 2 bytes (16 bits)
		- doubleword = 4 bytes (32 bits)
		- quadword = 8 bytes (64 bits) 
		- double quadword (octword) = 16 bytes (128 bits)
		- double octword = 32 bytes (256 bits)
	*)

	open tigerassem
	open tigertree

	(* codegen : tigerframe.frame -> tigertree.stm -> tigerassem.instr list *)
	(* Assuming that 'stm' parameter is canonized *)
	fun codegen frame (stm : tigertree.stm) : instr list =
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
			fun munchStm(MOVE(MEM(BINOP(PLUS, CONST i, e)), NAME l)) =
				emit(OPER{
						assem="movq $"^l^", "^utils.intToString i^"(`s0)\n",
						src=[munchExp e],
						dst=[],
						jump=NONE
					})

				| munchStm(MOVE(MEM(BINOP(PLUS, CONST i, e)), CONST j)) =
					emit(OPER{
							assem="movq $"^utils.intToString j^", "^utils.intToString i^"(`s0)\n",
							src=[munchExp e],
							dst=[],
							jump=NONE
						})

				| munchStm(MOVE(MEM(BINOP(PLUS, CONST i, e)), e')) =
					emit(OPER{
							assem="movq `s0, "^utils.intToString i^"(`s1)\n",
							src=[munchExp e',
								munchExp e],
							dst=[],
							jump=NONE
						})

				| munchStm(MOVE(MEM(BINOP(PLUS, e, CONST i)), e')) =
					munchStm(MOVE(MEM(BINOP(PLUS, CONST i, e)), e'))

				| munchStm(MOVE(MEM e, CONST i)) =
					emit(OPER{
							assem="movq $"^utils.intToString i^", (`s0)\n",
							src=[munchExp e],
							dst=[],
							jump=NONE
						})

				| munchStm(MOVE(MEM e1, e2)) = 
					emit(OPER{
							assem="movq `s0, (`s1)\n",
							src=[munchExp e2,
								munchExp e1],
							dst=[],
							jump=NONE
						})

				(* Move to temporary cases, like:
				MOVE(TEMP t, e): Evaluate e and move it to temporary t *)
				| munchStm(MOVE(TEMP t, MEM(BINOP(PLUS, CONST i, e)))) =
					emit(OPER{
							assem="movq "^utils.intToString i^"(`s0), `d0\n",
							src=[munchExp e],
							dst=[t],
							jump=NONE
						})

				| munchStm(MOVE(TEMP t, MEM(BINOP(PLUS, e, CONST i)))) =
					emit(OPER{
							assem="movq "^utils.intToString i^"(`s0), `d0\n",
							src=[munchExp e],
							dst=[t],
							jump=NONE
						})

				| munchStm(MOVE(TEMP t, MEM e)) =
					emit(OPER{
							assem="movq (`s0), `d0\n",
							src=[munchExp e],
							dst=[t],
							jump=NONE
						})

				| munchStm(MOVE(TEMP t, CONST i)) =
					emit(OPER{
							assem="movq $"^utils.intToString i^", `d0\n",
							src=[],
							dst=[t],
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
					emit(tigerassem.MOVE{
							assem="movq `s0, `d0\n",
							src=t2,
							dst=t1
						})

				| munchStm(MOVE(TEMP t, CALL(NAME f, args))) =
					let
						val _ = munchStm(EXP(CALL(NAME f, args)))	(* "Execute" call *)
					in
						emit(tigerassem.MOVE{
								assem="movq `s0, `d0\n",
								src=tigerframe.rv,
								dst=t
							})
					end

				| munchStm(MOVE(TEMP t, e)) =
					emit(tigerassem.MOVE{
							assem="movq `s0, `d0\n",
							src=munchExp e,
							dst=t
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
						val relToInstr = case relop of
							EQ => "je"
							| NE => "jne"
							| LT => "jl"
							| GT => "jg"
							| LE => "jle"
							| GE => "jge"
							| _ => raise Fail "Error - munchStm(): operación de comparación no soportada"

						val _ = emit(OPER{
								assem="cmpq `s0, `s1\n",
								src=[munchExp e1, munchExp e2],
								dst=[],
								jump=NONE
							})
					in
						emit(OPER{
								assem=relToInstr^" "^l1^"\n",
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
						val diff = List.length args - List.length (tigerframe.argregs)
					in
						(* preguntar si es _allocRecord *)
						emit(OPER{
								assem="movq $0, `d0 # added\n",
								src=[],
								dst=[tigerframe.rv],
								jump=NONE
							});
						emit(OPER{
								assem="call "^f^"\n",
								src=(munchArgs args) @ [tigerframe.rv],
								dst=tigerframe.calldefs,
								jump=NONE
							});
						if diff > 0 
						then 
							emit(OPER{
									assem="addq $"^utils.intToString(diff*(tigerframe.wSz))^", %rsp # diff = "^Int.toString diff^"\n",
									src=[],
									dst=[],
									jump=NONE
								}) 
						else 
							()
					end

				| munchStm(EXP(CALL _)) = raise Fail "Error - munchStm(): CALL sin label"

				| munchStm(EXP e) = (munchExp e; ())	(* Evaluate e and discard the result *)

			and saveCallerSaves() =
						let
							fun emitcdefs s =
								emit(OPER{
										assem="pushq `s0 # SAVE CALLER-SAVE REGISTER\n",
										src=[s],
										dst=[],
										jump=NONE
									})
						in
							List.map emitcdefs (tigerframe.callersaves)
						end

			and restoreCallerSaves() =
						let
							fun emitcdefs s =
								emit(OPER{
										assem="popq `d0 # RESTORE CALLER-SAVE REGISTER\n",
										src=[],
										dst=[s],
										jump=NONE
									})
						in
							List.app emitcdefs (List.rev (tigerframe.callersaves))
						end


			(* munchArgs() generates code to move all the arguments to their correct positions, 
			in outgoing parameter registers and/or in memory. Returns a list of all the temporaries 
			that are to be passed to the machine's CALL instruction; they should be listed as 
			"sources" of the instruction, so that liveness analysis can see that their values need 
			to be kept up to the point of call. *)
			and munchArgs args =
						let
							fun argsToStack [] = []													(* Push parameters in stack *)
								| argsToStack (a::ass) = 
									let 
										val _ = case a of
											CONST i => emit(OPER{
													assem="pushq $"^utils.intToString i^"\n",
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
													assem="pushq "^utils.intToString i^"(`s0)\n",
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
											| _ => emit(OPER{
													assem="pushq `s0\n",
													src=[munchExp a],
													dst=[],
													jump=NONE
												})
									in
										argsToStack ass
									end

							fun argsToReg [] _ = []
								| argsToReg ass [] = argsToStack (rev ass)			(* Runned out of machine's temporaries argument registers to move function's arguments, so push them in stack applying C calling convention *)
								| argsToReg (a::ass) (r::rss) = 
									let
										val _ = munchStm(MOVE(TEMP r, a))			(* Move argument to temporary register *)
									in
										r::(argsToReg ass rss)						(* Save list of registers *)
									end
						in
							argsToReg args tigerframe.argregs						(* Attempt to move function's arguments to machine's temporaries argument registers available if possible *)
						end

			(* Generates aassembly code to evaluate the input tree expression, then 
				returns register with final result in it *)
			(* munchExp : tigertree.stm -> tigertemp.temp *)
			and munchExp(MEM(e)) =
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

				(* PLUS op cases *)

				| munchExp(BINOP(PLUS, CONST i, e)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="addq $"^utils.intToString i^", `d0\n",
								src=[r],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(PLUS, NAME l, e)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="addq $"^l^", `d0\n",
								src=[r],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(PLUS, TEMP t, e)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="addq `s1, `d0\n",
								src=[r, t],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(PLUS, MEM(TEMP t), e)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="addq (`s1), `d0\n",
								src=[r, t],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(PLUS, MEM(BINOP(PLUS, CONST i, TEMP t)), e)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="addq "^utils.intToString i^"(`s1), `d0\n",
								src=[r, t],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(PLUS, MEM(BINOP(PLUS, TEMP t, CONST i)), e)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="addq "^utils.intToString i^"(`s1), `d0\n",
								src=[r, t],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(PLUS, MEM e1, e2)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e2));
						emit(OPER{
								assem="addq (`s1), `d0\n",
								src=[r, munchExp e1],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(PLUS, e1, e2)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e2));
						emit(OPER{
								assem="addq `s1, `d0\n",
								src=[r, munchExp e1],
								dst=[r],
								jump=NONE
							})))


				(* MINUS op cases.
				Remember: we have to meet the needs of BINOP(MINUS, e1, e2) = e1 - e2 *)

				| munchExp(BINOP(MINUS, e, CONST i)) = 
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="subq $"^utils.intToString i^", `d0\n",
								src=[r],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(MINUS, e, NAME l)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="subq $"^l^", `d0\n",
								src=[r],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(MINUS, e, TEMP t)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="subq `s1, `d0\n",
								src=[r, t],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(MINUS, e, MEM(TEMP t))) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="subq (`s1), `d0\n",
								src=[r, t],
								dst=[r],
								jump=NONE	
							})))

				| munchExp(BINOP(MINUS, e, MEM(BINOP(PLUS, CONST i, TEMP t)))) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="subq "^utils.intToString i^"(`s1), `d0\n",
								src=[r, t],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(MINUS, e, MEM(BINOP(PLUS, TEMP t, CONST i)))) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e));
						emit(OPER{
								assem="subq "^utils.intToString i^"(`s1), `d0\n",
								src=[r, t],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(MINUS, e1, MEM e2)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e1));
						emit(OPER{
								assem="subq (`s1), `d0\n",
								src=[r, munchExp e2],
								dst=[r],
								jump=NONE	
							})))

				| munchExp(BINOP(MINUS, e1, e2)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e1));
						emit(OPER{
								assem="subq `s1, `d0\n",
								src=[r, munchExp e2],
								dst=[r],
								jump=NONE
							})))


				(* DIV op cases *)

				| munchExp(BINOP(DIV, _, CONST 0)) = raise Fail "Error - munchExp(): división por cero!"	(* hacen falta casos así? creo que no *)

				| munchExp(BINOP(DIV, e1, e2)) = 
					let
						val tmpe2 = munchExp e2						(* Temporary containing expression e2 *)
					in
						generateTmp(fn r => 
							(munchStm(MOVE(TEMP tigerframe.rax, e1));
							emit(OPER{
									assem="cqto\n",					(* cqto: Sign-extends the contents of RAX to RDX:RAX (now, 128 bits) - 
																	broadcasts the sign bit of RAX into every bit of RDX *)
									src=[tigerframe.rax],
									dst=[tigerframe.rdx],
									jump=NONE
								});
							emit(OPER{
									assem="idivq `s1\n",			(* idivq: Signed divide RDX:RAX by tmpe2. Quotient stored in RAX. Remainder stored in RDX *)
									src=[tigerframe.rax,
										tmpe2],
									dst=[tigerframe.rax,
										tigerframe.rdx],
									jump=NONE
								});
							munchStm(MOVE(TEMP r, TEMP tigerframe.rax))))	(* Move result of division to result temp *)
					end

				(* MUL op cases *)

				| munchExp(BINOP(MUL, e1, e2)) =
					generateTmp(fn r => 
						(munchStm(MOVE(TEMP r, e1));
						emit(OPER{
								assem="imulq `s0, `d0\n",
								src=[munchExp e2, r],
								dst=[r],
								jump=NONE
							})))

				| munchExp(BINOP(_,_,_)) = raise Fail "Error - munchExp(): operación binaria no soportada"

		in
			(munchStm stm; rev (!ilist))
		end
end