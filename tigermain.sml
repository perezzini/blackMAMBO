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

		val entrada =
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf (* AST, con escapes *)
		val _ = findEscape(expr) (* AST, sin escapes *)

		(* -arbol OPTION *)
		val _ = if arbol 
				then 
					tigerpp.exprAst expr 
				else 
					()

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

		val (procs, strings) = fragPartition fragments ([], [])

		(* procsCanonized : (tigertree.stm * tigertemp.label) list *)
		val procsCanonized = map (fn {body, frame} => 
			(canonize body, tigerframe.name frame)) procs

		(* -canon OPTION *)
		val _ = if canon 
                then 
                	List.app (fn (c, n) => 
                        (print("\n"^n^":\n");
                         List.app (print o tigerit.tree) c)) procsCanonized
                else 
                	()
	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
