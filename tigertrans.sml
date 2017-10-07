structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero
	
type level = {parent:frame option, 
				frame: frame, 
				level: int}
				
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)

fun getActualLev() = !actualLevel

val outermost: level = {parent = NONE,
						frame = newFrame{name="_tigermain", formals=[]}, 
										level=getActualLev()}

fun newLevel{parent={parent, frame, level}, name, formals} =
	{parent=SOME frame,
	frame=newFrame{name=name, formals=formals},
	level=level+1}

fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b

fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
	
fun formals{parent, frame, level} = tigerframe.formals frame

(* Tres tipos de expresiones *)
datatype exp =
	Ex of tigertree.exp (* Expresiones que computan un valor *)
	| Nx of tigertree.stm (* Expresiones que no computan un valor (como llamadas a 
							procedimientos, o expresiones while en Tiger) *)
	| Cx of label * label -> tigertree.stm (* Condicionales; representados como una 
											función que toma un par de labels y 
											devuelve un tigertree.stm. Si se le pasa  
											un "destino true" y un "destino false", 
											devolverá un statement que evalúa algunos 
											condicionales y luego salta a alguna de las 
											destinaciones. *)
	(* | SCAF *)

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

(*
	Las siguientes funciones se deshacen de los constructores Ex, Nx, y Cx:

	unEx : exp -> tigertree.exp
	unNx : exp -> tigertree.stm
	unCx : exp -> (tigertemp.label x tigertemp.label -> tigertree.stm)
*)

fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s, CONST 0) (* Evalua s por side effects. CONST 0: "no devuelve nada" *)
	| unEx (Cx cf) = (* Convierte un condicional en una expresión con valor *)
		let
			val r = newtemp()
			val t = newlabel()
			val f = newlabel()
		in
			ESEQ(seq [MOVE(TEMP r, CONST 1),
				cf (t, f),
				LABEL f,
				MOVE(TEMP r, CONST 0),
				LABEL t],
				TEMP r)
		end

fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx cf) =
		let
			val t = newlabel()
			val f = newlabel()
		in
			seq [cf(t,f),
				LABEL t,
				LABEL f]
		end

fun unCx (Nx s) = raise Fail ("Error (unCx(Nx..)). Nunca debe suceder")
	| unCx (Cx cf) = cf
	| unCx (Ex (CONST 0)) = (fn (t,f) => JUMP(NAME f, [f])) (* caso: false *)
	| unCx (Ex (CONST _)) = (fn (t,f) => JUMP(NAME t, [t])) (* caso: true *)
	| unCx (Ex e) = (fn (t,f) => CJUMP(NE, e, CONST 0, t, f)) (* caso: evaluar expresión e *)

fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a completar!"
		fun aux2(PROC{body, frame}) = aux(Nx body)
		| aux2(STRING(l, "")) = l^":\n"
		| aux2(STRING("", s)) = "\t"^s^"\n"
		| aux2(STRING(l, s)) = l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	
		aux3 e 
	end

fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")

(* While y for necesitan la u'ltima etiqueta para un break *)
local
	val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
	val pushSalida = tigerpila.pushPila salidas
	fun popSalida() = tigerpila.popPila salidas
	fun topSalida() =
		case tigerpila.topPila salidas of
			SOME l => l
			| NONE => raise Fail "break incorrecto!"			
end

(* Global list of frag ("fragments") *)
val datosGlobs = ref ([]: frag list)

fun procEntryExit{level: level, body} =
	let	val label = STRING(name(#frame level), "")
		val body' = PROC{frame= #frame level, body=unNx body}
		val final = STRING(";;-------", "")
	in	
		datosGlobs:=(!datosGlobs@[label, body', final]) 
	end

fun getResult() = !datosGlobs

(* Following static links - page 156 
	"When a variable x is declared at an outer level of static scope, 
	static links must be used. To construct this expression, we need 
	the level l_f of the function f in which x is used, and the level 
	l_g in which x is declared" *)
fun followSL 0 = TEMP(tigerframe.fp)
	| followSL n = MEM(BINOP(PLUS, CONST tigerframe.fpPrevLev, followSL (n-1)))

fun stringLen s =
	let	fun aux[] = 0
		| aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
		| aux(_::t) = 1+aux(t)
	in	
		aux(explode s) 
	end

(* "A string literal in Tiger is the constant addess of a segment of memory initialized 
	to the proper characters. In assembly lang a label is used to refer to this address 
	from the middle of some sequence of instructions. At some other place in the 
	assembly-lang program, the definition of that label appears, followed by the 
	assembly-lang pseudo-instruction to reserve and init a block of memory to the 
	appropriate characters" - page 163 *)
(* stringExp : string -> exp *)
fun stringExp(s: string) =
	let	val l = newlabel()
		val len = ".long "^makestring(stringLen s)
		val str = ".string \""^s^"\""
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING("", str)])
	in	
		Ex(NAME l) 
	end

fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)

(* functionDec : exp * level * bool -> exp *)
fun functionDec(e, l, proc) =
	let	val body =
				if proc then unNx e
				else MOVE(TEMP rv, unEx e)
		val body' = procEntryExit1(#frame l, body)
		val () = procEntryExit{body=Nx body', level=l}
	in	
		Ex(CONST 0) 
	end

fun postFunctionDec() = (popSalida(); actualLevel := !actualLevel-1)

(* unitExp : unit -> exp *)
fun unitExp() = Ex (CONST 0)

(* nilExp : unit -> exp *)
fun nilExp() = Ex (CONST 0)

(* intExp : int -> exp *)
fun intExp i = Ex (CONST i)

(* simpleVar: access * int -> exp *)
fun simpleVar(acc, nivel) =
	(* acc: si esta en memoria o en registro.
		nivel: nivel de la función en la cual la var es usada. *)

	(* "On each procedure call or var access, a chain of zero or more 
		fetches is required; the length of the chain is just the 
		difference in static nesting deph between the two functions 
		involved" - page 134

		For a simple var v declared in the current procedure's stack 
		frame and want to access it, we calculate the frame address as
			MEM(BINOP(PLUS, TEMP tigerframe.fp, CONST k))
		where k is the offset from the fp. When accessing acc from an 
		inner-nested function, the frame address must be calculated 
		using static links.

		tigerframe.exp funciton turns a tigerframe.access into the Tree 
		expression. 
	*)
	Ex(tigerframe.exp acc (followSL (getActualLev() - nivel)))

(* varDec : access -> exp *)
fun varDec(acc) = simpleVar(acc, getActualLev())

(* "All records and array values are pointers to record and array structures. 
	The base address of the array is really the contents of a pointer variable, 
	so MEM is required to fetch this base address" - page 159 *)

(* var.field: seleccionar campo de un record *)
(* fieldVar : exp * int -> exp *)
fun fieldVar(var, field) = 
	(* Cada field de un record ocupa un word-size *)
	let
		val v = unEx var (* puntero a la dir de memoria base (estructura) del record *)
		val rv = newtemp()
	in
		Ex(ESEQ(seq[MOVE(TEMP rv, v),
					EXP(externalCall("_checkNil", [TEMP rv]))],
					MEM(BINOP(PLUS, TEMP rv,
								BINOP(MUL, CONST field, CONST tigerframe.wSz)))))
	end

(* arr[ind]: acceso a un índice de un arreglo *)
(* subscriptVar : exp * exp -> exp *)
fun subscriptVar(arr, ind) =
	(* Cada celda de un array ocupa un word-size *)
	let
		val a = unEx arr (* puntero a la dir de memoria base (estructura) del array *)
		val i = unEx ind
		val ra = newtemp()
		val ri = newtemp()
	in
		Ex(ESEQ(seq[MOVE(TEMP ra, a),
					MOVE(TEMP ri, i),
					EXP(externalCall("_checkIndex", [TEMP ra, TEMP ri]))],
					MEM(BINOP(PLUS, TEMP ra,
						BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
		(* Devuelve MEM(...) para hacer fetch del elemento del array en cuestión *)
	end

(* recordExp : (exp * int) list -> exp *)
fun recordExp l =
	let
		(* a{f1 = e1, ..., fn = en} *)
		(* "The simplest way to create a record is to call an external memory-allocation 
		function that returns a pointer to an n-word area into a new temp r. Then a series 
		of MOVE trees can init offsets 0, 1W, 2W, ..., (n-1)W from r with translations of 
		expressions ei. Finally, the result of the whole expression is TEMP(r)" - pag 164 *)
		val ret = newtemp()

		fun genTemps 0 = []
			| genTemps n = newtemp() :: genTemps(n-1)

		val regs = genTemps(List.length l) (* se genera un temporario para cada campo del record *)

		fun aux ((e, s), t) = (MOVE(TEMP t, unEx e), s, TEMP t)

		val lexps = map aux (ListPair.zip (l, regs))
		val lexps' = map #1 lexps
		val l' = Listsort.sort (fn ((_, m, _), (_, n, _)) => 
			Int.compare(m, n)) lexps (* ordena las ternas que genera la función aux dependiendo la pos 
										de los campos en memoria *)
	in
		Ex(ESEQ(seq(lexps'),
				externalCall("_allocRecord", CONST (List.length l) :: (List.map #3 l'))))
	end

(* arrayExp : {size: exp, init: exp} -> exp *)
fun arrayExp{size, init} =
	let
		val s = unEx size
		val i = unEx init
	in
		Ex(externalCall("_allocArray", [s, i]))
	end

(* callExp : tigertemp.label * bool * bool * level * exp list -> exp *)
fun callExp (name, external, isproc, lev:level, ls) = 
	let
		(* 
			"Translate a function call f(a1, ..., an) is simple, except that the static 
			link must be added as an implicit extra argument:
				CALL(NAME l_f, [sl, e1, ..., en])" - page 166

			"CALL(f, l) -> A procedure call: the aplication of function f to argument list l. 
			The subexpression f is evaluated before the arguments which are evaluated left 
			to right" - page 151

			"Suppose a funciton g calls the function f. We say g is the CALLER and f is the 
			CALLEE" - page 128.

			In callExp (f, e, isproc, lev, args):
				- f is the callee function
				- lev is the level of the function calling f (the level of the callee function)
				- args: list of formals that f takes
				- isproc: tells if f is a proc or not
				- e: si es una external call

			"Static links: Whenever a function f is called, it is passed a pointer to the stack 
			frame of the current (most recently entered - the function statically enclosing f) 
			activation of the function g that immediatly encloses f in the text of the program" 
			- page 133. Here, f is the CALLEE function and g is the CALLER function.
		*)

		(* El principal problema aquí es generar el SL. Veamos las reglas que tendremos que 
		usar, todo depende de los niveles de la llamante y la llamada:
				1er CASO: nivel_llamante = nivel_llamada
							SL_llamada = SL_llamante

				2do CASO: nivel_llamante < nivel_llamada
							por scope debe ser:
							nivel_llamada = nivel_llamante + 1
							SL_llamada = FP_llamante

				3er CASO: nivel_llamante > nivel_llamada
							SL_llamada = n-ésimo SL anidante contenido desde el SL_llamante
										donde n es la diferencia entre los niveles
		*)
		fun traerFP 0 = TEMP tigerframe.fp
			| traerFP n = MEM(BINOP(PLUS, traerFP(n - 1), CONST fpPrevLev))

		val calleeLev = #level lev
		(* getActualLev(): level of the caller function *)

		val fplev = if calleeLev = getActualLev() then 
						MEM(BINOP(PLUS, TEMP tigerframe.fp, CONST tigerframe.fpPrevLev)) (* 1er CASO *)
					else 
						if calleeLev < getActualLev() then 
							traerFP (getActualLev() - (#level lev) + 1) (* 3er CASO *)
						else 
							TEMP tigerframe.fp (* 2do CASO *)


		fun preparaArgs [] (rt, re) = (rt, re) (* rt: constantes, labels y temps. 
													re: expresiones a evaluar (se devuelve un exp en un temp) *)
			| preparaArgs (h::t) (rt, re) = case h of
				Ex(CONST s) => preparaArgs t ((CONST s)::rt, re)
				| Ex(NAME s) => preparaArgs t ((NAME s)::rt, re)
				| Ex(TEMP s) => preparaArgs t ((TEMP s)::rt, re)
				| _ =>
					let
						val t' = newtemp()
						val e = unEx h
					in
						preparaArgs t ((TEMP t')::rt, (MOVE(TEMP t', e))::re)
					end

		val (ta, ls') = preparaArgs (rev ls) ([], [])
		
		(* external indica si es de run-time. En el caso que así lo sea, no se le pasa el static link *)
		val ta' = if external then ta else fplev::ta
	in
		if isproc then (* La función devuelve TUnit *)
			Nx(seq(ls' @ [EXP(CALL(NAME name, ta'))]))
		else
			let
				(*
					Para escribir en assembler CALL f(4+3) se debe:
						1. Computar la suma
						2. Guardarla en un temporario t
						3. Llamar CALL f con el registro t)
				*)
				val tmp = newtemp()
			in
				(* Devolvemos el resultado en rv (return value del machine-target) *)
				Ex (ESEQ (seq(ls' @ [EXP(CALL(NAME name, ta')),
                                   MOVE(TEMP tmp,TEMP tigerframe.rv)]), TEMP tmp))
			end
	end

(* letExp : tigertree.stm list * exp -> exp *)
fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits, unEx body))

(* breakExp : unit -> exp *)
fun breakExp() = 
	let
		val ts = topSalida()
	in
		Nx(JUMP(NAME ts, [ts]))
	end

(* seqExp : exp list -> exp *)
fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
	| seqExp (exps:exp list) =
		let
			fun unx [e] = []
				| unx (s::ss) = (unNx s)::(unx ss)
				| unx[] = []
		in
			case List.last exps of
				Nx s =>
					let val 
						unexps = map unNx exps
					in 
						Nx (seq unexps) end
				| Ex e => Ex (ESEQ(seq(unx exps), e))
				| cond => Ex (ESEQ(seq(unx exps), unEx cond))
		end

fun preWhileForExp() = pushSalida(SOME(newlabel()))

fun postWhileForExp() = (popSalida(); ())

(* whileExp : {test: exp, body: exp, lev:level} -> exp *)
fun whileExp {test: exp, body: exp, lev:level} =
	let
		val cf = unCx test
		val expb = unNx body
		val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
	in
		Nx(seq[LABEL l1,
				cf(l2,l3),
				LABEL l2,
					expb,
					JUMP(NAME l1, [l1]),
				LABEL l3])
	end

(* forExp : {lo: exp, hi: exp, var: exp, body: exp} -> exp *)
fun forExp {lo, hi, var, body} =
	let
		val var' = unEx var
		val (l1, l2, lsal) = (newlabel(), newlabel(), newlabel())
	in
		case hi of
			Ex(CONST n) => Nx(seq[MOVE(var', unEx lo),
									CJUMP(LE, var', CONST n, l2, lsal),
									LABEL l2,
										unNx body,
										CJUMP(EQ, var', CONST n, lsal, l1),
									LABEL l1,
										MOVE(var', BINOP(PLUS, var', CONST 1)),
										JUMP(NAME l2, [l2]),
									LABEL lsal])
			(* hi no es CONST *)
			| _ =>
				let
					val t = newtemp()
				in
					Nx(seq[MOVE(var', unEx lo),
							MOVE(TEMP t, unEx hi),
							CJUMP(LE, var', TEMP t, l2, lsal),
							LABEL l2,
								unNx body,
								CJUMP(EQ, var', TEMP t, lsal, l1),
							LABEL l1,
								MOVE(var', BINOP(PLUS, var', CONST 1)),
								JUMP(NAME l2, [l2]),
							LABEL lsal])
				end
	end

(* La rama del IF computa un valor *)
(* ifThenExp : {test: exp, then': exp} -> exp *)
fun ifThenExp{test, then' = Cx condThen'} =
		let
			val (t, f, t', fl) = (newlabel(), newlabel(), newlabel(), newlabel())
			val test' = unCx test
			val tmp = newtemp()
		in
			(* Forma 1: comparte label f *)
			Ex(ESEQ(seq[test'(t, f),
						LABEL t,
							condThen'(t', f),
							LABEL t',
								MOVE(TEMP tmp, CONST 1),
								JUMP(NAME fl, [fl]),
						LABEL f,
							MOVE(TEMP tmp, CONST 0),
						LABEL fl], TEMP tmp))

			(* Forma 2 *)
			(*
			Ex(ESEQ(seq[test'(t, f),
						LABEL t,
							condThen'(t', f'),
							LABEL t'
								MOVE(TEMP tmp, CONST 1),
								JUMP(NAME fl, [fl])
							LABEL f'
								MOVE(TEMP tmp, CONST 0),
								JUMP(NAME fl, [fl]),
						LABEL f,
							MOVE(TEMP tmp, CONST 0),
						LABEL fl], TEMP tmp))
			*)
		end
	(* then' computa un valor; es decir, then' = Ex _ *)
	| ifThenExp{test, then'} =
		let
			val (t, f, fl) = (newlabel(), newlabel(), newlabel())
			val test' = unCx test
			val tmp = newtemp()
			val then'' = unEx then'
		in
			Ex(ESEQ(seq[test'(t, f),
						LABEL t,
							MOVE(TEMP tmp, then''),
							JUMP(NAME fl, [fl]),
						LABEL f,
							MOVE(TEMP tmp, CONST 0),
							JUMP(NAME fl, [fl]),
						LABEL fl], TEMP tmp))
		end

(* Las ramas del IF computan un valor *)
(* ifThenElseExp : {test: exp, then': exp, else': exp} -> exp *)
fun ifThenElseExp {test, then' = Cx condThen', else' = Ex (CONST 0)} =
		let
			val (t, t', f, fl) = (newlabel(), newlabel(), newlabel(), newlabel())
			val test' = unCx test
			val tmp = newtemp()
		in
			Ex(ESEQ(seq[test'(t, f),
						LABEL t,
							condThen'(t', f),
							LABEL t',
								MOVE(TEMP tmp, CONST 1),
								JUMP(NAME fl, [fl]),
						LABEL f,
							MOVE(TEMP tmp, CONST 0),
						LABEL fl], TEMP tmp))
		end
	| ifThenElseExp {test, then' = Ex (CONST 1), else' = Cx condElse'} =
		let
			val (t, f, f', fl) = (newlabel(), newlabel(), newlabel(), newlabel())
			val test' = unCx test
			val tmp = newtemp()
		in
			Ex(ESEQ(seq[test'(t, f),
						LABEL t,
							MOVE(TEMP tmp, CONST 1),
							JUMP(NAME fl, [fl]),
						LABEL f,
							condElse'(t, f'),
							LABEL f',
								MOVE(TEMP tmp, CONST 0),
						LABEL fl], TEMP tmp))
		end
	| ifThenElseExp {test, then', else'} =
		let
			val (t, f, fl) = (newlabel(), newlabel(), newlabel())
			val test' = unCx test
			val then'' = unEx then'
			val else'' = unEx else'
			val tmp = newtemp()
		in
			Ex(ESEQ(seq[test'(t, f),
						LABEL t,
							MOVE(TEMP tmp, then''),
							JUMP(NAME fl, [fl]),
						LABEL f,
							MOVE(TEMP tmp, else''),
						LABEL fl], TEMP tmp))
		end
	

(* Las ramas del IF no computan un valor; es decir, devuelven TUnit *)
(* ifThenElseExpUnit : {test: exp, then': exp, else': exp} -> exp *)
fun ifThenElseExpUnit {test, then', else'} =
	let
		val (t, f, fl) = (newlabel(), newlabel(), newlabel())
		val test' = unCx test
		val then'' = unNx then'
		val else'' = unNx else'
	in
		Nx(seq[test'(t, f),
				LABEL t,
					then'',
					JUMP(NAME fl, [fl]),
				LABEL f,
					else'',
				LABEL fl])
	end

(* assignExp : {var: exp, exp:exp}-> exp *)
fun assignExp{var, exp} =
	let
		val v = unEx var
		val vl = unEx exp
	in
		Nx(MOVE(v, vl))
	end

(* Integer arithmetic: each tigerabs arithmetic op corresponds to 
a tigertree op *)
(* binOpIntExp : {left:exp, oper:tigerabs.oper, right:exp} -> exp *)
fun binOpIntExp {left, oper, right} = 
	let
		(* La subexpresión left debe ser evaluada antes que la 
		subexpresión right. Ambas computan un valor *)
		val l = unEx left
		val r = unEx right
	in
		case oper of
			PlusOp => Ex(BINOP(PLUS, l, r))
			| MinusOp => Ex(BINOP(MINUS, l, r))
			| TimesOp => Ex(BINOP(MUL, l, r))
			| DivideOp => Ex(BINOP(DIV, l, r))
			| _ => raise Fail "Error: no es un cálculo aritmético"
	end

(* binOpIntRelExp: {left:exp, oper:tigerabs.oper, right:exp} -> exp *)
fun binOpIntRelExp {left,oper,right} =
	let
		(* La subexpresión left debe ser evaluada antes que la 
		subexpresión right. Ambas computan un valor *)
		val l = unEx left
		val r = unEx right

		fun cond oper = fn (t, f) => 
			CJUMP(oper, l, r, t, f)
	in
		case oper of
			EqOp => Cx(cond EQ)
			| NeqOp => Cx(cond NE)
			| LtOp => Cx(cond LT)
			| LeOp => Cx(cond LE)
			| GtOp => Cx(cond GT)
			| GeOp => Cx(cond GE)
			| _ => raise Fail "Error: no es una comparación de enteros"
	end

(* binOpStrExp : {left:exp, oper:tigerabs.oper, right:exp} -> exp *)
fun binOpStrExp {left,oper,right} =
	(* "Because the string equality op is complicated (it must loop 
		through the bytes checking byte-for-byte equality), the 
		compiler should call a runtime-system function stringEqual 
		that implements it. This function returns 0 or 1 value 
		(false or true)" - page 162 *)
	let
		val l = unEx left
		val r = unEx right

		fun cond oper = fn (t, f) => 
			CJUMP(oper,
				tigerframe.externalCall("_stringEqual", [l, r]), 
				CONST 0,
				t, 
				f)
	in
		(* Only string comparison *)
		case oper of
			EqOp => Cx(cond EQ)
			| NeqOp => Cx(cond NE)
			| LtOp => Cx(cond LT)
			| LeOp => Cx(cond LE)
			| GtOp => Cx(cond GT)
			| GeOp => Cx(cond GE)
			| _ => raise Fail "Error: operaciones aritméticas entre strings no admitidas"
	end


end
