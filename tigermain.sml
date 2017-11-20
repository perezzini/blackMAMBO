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
	let	fun arg(l, s) =
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
    val (color, l10)  = arg(l9, "-color")

		val entrada =
			case l10 of 	(* Remember to change this value in case of new options *)
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe"))
			| [] => std_in
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
		val (procs, strings) = fragPartition fragments ([], [])

		val procsCanonized : (tigertree.stm list * tigerframe.frame) list = List.map (fn {body, frame} => 
			(canonize body, frame)) procs

		(* -canon OPTION *)
		val _ = if canon 
                then 
                	List.app (fn (c, f) => 
                        (print("\n"^tigerframe.name(f)^":\n");
                         List.app (print o tigerit.tree) c)) procsCanonized	(* c : tigertree.stm list *)
                else 
                	()

        (* -inter OPTION *)
        val _ = if inter 
        		then 
        			tigerinterp.inter debug procsCanonized strings 
        		else 
        			()

       	val procsToAssem : (tigerassem.instr list * tigerframe.frame) list = List.map (fn (cbody, frame) => 
       		let
       			val instrList = List.concat (List.map (tigercodegen.codegen frame) cbody)	(* cbody is a tigertree.stm list;
       																						tigercodegen.codegen returns a instr list. Then, we have to flat a list of lists *)
       			
       			val instrList' = tigerframe.procEntryExit2(frame, instrList)				(* Apply tigerframe.procEntryExit2() *)
       		in
       			(instrList', frame)
       		end) procsCanonized

       	(* Now, let's format each body converted to assembly language *)
       	val assemBodyFormatted : (tigertemp.label * string list) list = List.map (fn (ilist, frame) => 
       		(tigerframe.name frame, 
       			List.map (tigerassem.format tigertemp.makeString) ilist)) procsToAssem	(* Since I have not done register assignment yet, I just pass 
       																				tigertemp.makeString to format as the translation function from 
       																				temporaries to strings *)

       	(* -code OPTION *)
       	val _ = if code 
       			then 
       				List.app (fn (fName, instrStrList) => 
       					(print("\n"^fName^":\n\n");
       						List.app print instrStrList)) assemBodyFormatted
       			else 
       				()


       	(* LIVENESS ANALYSIS *)

       	val liveOutInfo : (string * (string * string * string * string) list) list  = List.map (fn (instrList, frame) => 
       		let
       			val info = tigerliveness.liveOutInfoToString instrList
       			val fName = tigerframe.name frame
       		in
       			(fName, info)
       		end) procsToAssem

       	(* Concat live-out info in one string *)
       	val liveOutInfo' : (string * string list) list = List.map (fn (fName, liveOutInfoToStringList) => 
       		(fName, List.map (fn (instrStr, nStr, succSetStr, liveOutStr) => 
       			let
       				val concatValues = "("^instrStr^";\tnode = "^nStr^";\tsuccs = "^succSetStr^";\tlive-out = "^liveOutStr^")\n"
       			in
       				concatValues
       			end) liveOutInfoToStringList)) liveOutInfo

       	(* -liveout OPTION *)
       	val _ = if liveout 
       			then 
       				List.app (fn (fName, liveOutInfoToStringList) => 
       					(print("\n"^fName^":\t(instr list length = "^Int.toString (List.length liveOutInfoToStringList)^")\n\n");
       					List.app print liveOutInfoToStringList)) liveOutInfo'
       			else 
       				()


        (* REGISTER ALLOCATION *)

        (* Coloring of temporaries per instruction list (program) *)
        val regAllocation : (tigerregalloc.allocation * tigerframe.frame) list = List.map (fn (instrList, frame) => 
          (tigerregalloc.regAlloc (instrList, frame), frame)) procsToAssem

        (* Dictionary to list, and function name *)
        val regAllocationList : ((tigertemp.temp * tigerframe.register) list * string) list = List.map (fn (allocDict, frame) => 
          (Splaymap.listItems allocDict, tigerframe.name frame)) regAllocation


        (* -color OPTION
        val _ = if color 
                then 
                  (List.app (fn (fName, liveOutInfoToStringList) => 
                    (print("\n"^fName^"\n\n");
                    List.app print liveOutInfoToStringList)) liveOutInfo';
                  List.app (fn (pairList, _) => 
                    List.app (fn pair => 
                      print("\n"^tigerregalloc.allocPairToString pair^"\n")) pairList) regAllocationList)
                else 
                  ()
        *)
       	

	in
		print "\nyes!!, end of tigermain without any errors\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
