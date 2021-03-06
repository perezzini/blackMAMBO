open tigerlex
open tigergrm
open tigerescap
open tigerseman
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")
fun main(args) =
	let	
	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)

		(* OPTIONS *)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter")
		val (debug, l8)		= arg(l7, "-debug")
		val (liveout, l9)	= arg(l8, "-liveout")
		val (color, l10)  	= arg(l9, "-color")
		val (assembly, l11) = arg(l10, "-assembly")

		val (entrada, fileName) =
			case l11 of 	(* Remember to change this value in case of new options *)
			[n] => ((open_in n, n)
					handle _ => raise Fail (n^" no existe"))
			| [] => (std_in, "")
			| _ => raise Fail "opción desconocida"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf (* AST, with escapes *)
		val _ = findEscape(expr) (* AST, without escapes *)

		(* -arbol OPTION *)
		val _ = if arbol 
				then 
					tigerpp.exprAst expr 
				else 
					()

		(* Evaluate input expression: type-checking, and translation to IR code *)
		val _ = transProg(expr);


		(* Canonize function *)
		(* canonize : tigertree.stm -> tigertree.stm list *)
		val canonize = tigercanon.traceSchedule
						o tigercanon.basicBlocks
						o tigercanon.linearize

		val fragments = tigertrans.getResult()

		(* -ir OPTION *)
		val _ = if ir 
				then 
					print(tigertrans.Ir(fragments)) 
				else 
					()

		fun fragPartition [] (procs, strings) = (List.rev procs, List.rev strings)
			| fragPartition (tigerframe.PROC p :: fl) (procs, strings) = fragPartition fl (p :: procs, strings) 
			| fragPartition (tigerframe.STRING s :: fl) (procs, strings) = fragPartition fl (procs, s :: strings)

		(* procs : tigerframe.frag list - PROC;
		strings : tigerframe.frag list - STRING *)
		val (procs, strings) : ({body : tigertree.stm, frame : tigerframe.frame} list * (tigertemp.label * string) list) = fragPartition fragments ([], [])

		val canonProcs : (tigertree.stm list * tigerframe.frame) list = List.map (fn {body, frame} => 
			(canonize body, frame)) procs

		(* -canon OPTION *)
		val _ = if canon 
				then 
					List.app (fn (c, f) => 
						(print("\n"^tigerframe.name(f)^":\n");
						 List.app (print o tigerit.tree) c)) canonProcs	(* c : tigertree.stm list *)
				else 
					()

		(* -inter OPTION *)
		val _ = if inter 
				then 
					tigerinterp.inter debug canonProcs strings 
				else 
					()

		val assem : (tigerassem.instr list * tigerframe.frame) list = List.map (fn (cbody, frame) => 
			let
				val instrList = List.concat (List.map (tigercodegen.codegen frame) cbody)	(* cbody is a tigertree.stm list;
																							tigercodegen.codegen returns a instr list. Then, we have to flat a list of lists *)
				
				val instrList' = tigerframe.procEntryExit2(frame, instrList)				(* Apply tigerframe.procEntryExit2() *)
			in
				(instrList', frame)
			end) canonProcs

		(* Format, using tigertemp.makeString, each instruction converted to assembly language *)
		fun assemToString (assem : (tigerassem.instr list * tigerframe.frame) list) : (tigertemp.label * string list) list = 
			List.map (fn (ilist, frame) => 
				(tigerframe.name frame, List.map (tigerassem.format tigertemp.makeString) ilist)) assem

		(* -code OPTION *)
		val _ = if code 
				then 
					List.app (fn (fName, instrStrList) => 
						(print("\n"^fName^":\n\n");
							List.app print instrStrList)) (assemToString assem)
				else 
					()


		(* LIVENESS ANALYSIS *)

		fun prepareLiveOutInfo() =
			let
				val liveOutInfo : (string * (string * string * string * string) list) list  = List.map (fn (instrList, frame) => 
																								let
																									val info = tigerliveness.liveOutInfoToString instrList
																									val fName = tigerframe.name frame
																								in
																									(fName, info)
																								end) assem
				(* Concat live-out info in one string *)
				val liveOutInfo' : (string * string list) list = List.map (fn (fName, liveOutInfoToStringList) => 
					(fName, List.map (fn (instrStr, nStr, succSetStr, liveOutStr) => 
						let
							val concatValues = "("^instrStr^";\tnode = "^nStr^";\tsuccs = "^succSetStr^";\tlive-out = "^liveOutStr^")\n"
						in
							concatValues
						end) liveOutInfoToStringList)) liveOutInfo
			in
				liveOutInfo'
			end


		(* -liveout OPTION *)
		val _ = if liveout 
				then 
					List.app (fn (fName, liveOutInfoToStringList) => 
						(print("\n"^fName^":\t(instr list length = "^Int.toString (List.length liveOutInfoToStringList)^")\n\n");
						List.app print liveOutInfoToStringList)) (prepareLiveOutInfo())
				else 
					()


		(* REGISTER ALLOCATION *)

		(* computeRegisterAllocation : (tigerassem.instr list * tigerframe.frame) list -> (string list * tigerframe.frame) list *)
		fun computeRegisterAllocation (procList : (tigerassem.instr list * tigerframe.frame) list) : (string list * tigerframe.frame) list =
		  let
			(* Compute register allocation per function *)
			val regAlloc : ((tigerregalloc.allocation * tigerassem.instr list) * tigerframe.frame) list = List.map (fn (instrList, frame) => 
			  (tigerregalloc.regAlloc (instrList, frame), frame)) procList

			fun coloring (dict : tigerregalloc.allocation) (instrList : tigerassem.instr list) : string list =
			  let
				fun sayTemp (str : string) : tigertemp.temp =
				  let
					val color = case Splaymap.peek(dict, str) of
					  SOME c => c
					  | NONE => let
								  val _ = print("\nKey not found in allocation = "^str^"\n")
								in
								  raise Fail "Error - main. computeRegisterAllocation(). coloring(): peek error"
								end
				  in
					color
				  end
			  in
				List.map (tigerassem.format sayTemp) instrList
			  end

			(* Replace each temporary with it's corresponding color (machine register) computed *)
			val replace : (string list * tigerframe.frame) list = List.map (fn ((colorDic, instrList), frame) => 
			  (coloring colorDic instrList, frame)) regAlloc
		  in
			replace
		  end

		(* -color OPTION *)
		val _ = if color 
				then 
				  List.app (fn (instrListStr, frame) => 
					(print("\n"^tigerframe.name frame^" (instruction list length = "^Int.toString (List.length instrListStr)^"):\n\n");
					  List.app print instrListStr)) (computeRegisterAllocation assem)
				else 
				  ()

		(* Returns a string containing entire program in assembly-lang *)
		(* createFinalOutputAssembly : (string list * tigerframe.frame) list -> (tigertemp.label * string) list -> string *)
		fun createFinalOutputAssembly (procs : (string list * tigerframe.frame) list) (strings : (tigertemp.label * string) list) : string =
		  let

			(* prepareProcs : (string list * tigerframe.frame) list *)
			fun prepareProcs procs = 
			  let
				(* Prepend a \t to each assembly instruction *)
				val procs' : (string list * tigerframe.frame) list = List.map (fn (strList, frame) => 
				  (List.map (fn str => 
					"\t"^str) strList, frame)) procs

				(* Assembly code now in just one string *)
				val procs'' : (string * tigerframe.frame) list = List.map (fn (strList, frame) => 
				  (String.concat strList, frame)) procs'
			  in
				procs''
			  end

			(* convertStringFragsToString : unit -> string *)
			fun convertStringFragsToString() : string =
			  let
				val mapped : string list = List.map (fn (label, str) => 
				  tigerframe.string(label, str)) strings
			  in
				String.concat mapped
			  end

			val procs' : (string * tigerframe.frame) list = prepareProcs procs

			(* Apply tigerframe.procEntryExit3() to each function on procs', and returns the strings' compilation 
			in just one string *)
			fun appProcEntryExit3() : string =
			  let
				val mapped = List.map (fn (body, frame) => 
							  tigerframe.procEntryExit3 frame body) procs'
			  in
				String.concat mapped
			  end
				
		  in
			String.concat [
			  "### Program assembly. x64 architecture; AT&T syntax\n\n",
			  ".file\t \""^fileName^"\""^"\n\n",
			  ".data\n",
			  convertStringFragsToString()^"\n",
			  ".text"^"\n",
			  ".globl _tigermain"^"\n",
			  appProcEntryExit3()
			]
		  end

		val finalOutputAssembly : string = createFinalOutputAssembly (computeRegisterAllocation assem) strings

		(* -assem OPTION *)
		val _ = if assembly 
				then 
					print("\n**********\tFINAL OUTPUT ASSEMBLY\t**********\n\n"^finalOutputAssembly^"\n") 
				else 
				  ()

		(* Write final program assembly into a file *)
		val _ = if fileName <> "" 
				then 
					let
					 	val out = TextIO.openOut("result.s")
					 in
					 	TextIO.output(out, finalOutputAssembly);
					 	TextIO.closeOut(out)
					 end 
				else 
					()

		(* Execute GCC, and run the input program *)
		val _ = Process.system("gcc -g -c runtime.c && gcc -g result.s runtime.o -o result.bin && ./result.bin")
	in
		print("\nSuccessful compilation. Errors not found.\n")
	end	
		handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
