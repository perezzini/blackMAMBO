structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero
	
type level = {parent:frame option , frame: frame, level: int}
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)

fun getActualLev() = !actualLevel (* Nivel de anidamiento *)

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

val datosGlobs = ref ([]: frag list)

fun procEntryExit{level: level, body} =
	let	val label = STRING(name(#frame level), "")
		val body' = PROC{frame= #frame level, body=unNx body}
		val final = STRING(";;-------", "")
	in	
		datosGlobs:=(!datosGlobs@[label, body', final]) 
	end

fun getResult() = !datosGlobs

fun stringLen s =
	let	fun aux[] = 0
		| aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
		| aux(_::t) = 1+aux(t)
	in	
		aux(explode s) 
	end

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
		nivel: nivel de la función en la cual la var es declarada. *)
	let
		fun followSL 0 = TEMP(tigerframe.fp)
			| followSL n = MEM(followSL (n-1))
	in
		Ex(tigerframe.exp acc (followSL (getActualLev() - nivel)))
	end

(* varDec : access -> exp *)
fun varDec(acc) = simpleVar(acc, getActualLev())

(* fieldVar : exp * int -> exp *)
fun fieldVar(var, field) = 
	let
		val v = unEx var
		val f = unEx field
	in
		Nx(MOVE(v, f))
	end

(* subscriptVar : exp * exp -> exp *)
fun subscriptVar(arr, ind) =
	let
		val a = unEx arr
		val i = unEx ind
		val ra = newtemp()
		val ri = newtemp()
	in
		Ex( ESEQ(seq[MOVE(TEMP ra, a),
			MOVE(TEMP ri, i),
			EXP(externalCall("_checkindex", [TEMP ra, TEMP ri]))],
			MEM(BINOP(PLUS, TEMP ra,
				BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
	end

(* recordExp : (exp * int) list -> exp *)
fun recordExp l =
	Ex (CONST 0) (*COMPLETAR*)

(* arrayExp : {size: exp, init: exp} -> exp *)
fun arrayExp{size, init} =
	let
		val s = unEx size
		val i = unEx init
	in
		Ex (externalCall("allocArray", [s, i]))
	end

(* callExp : tigertemp.label * bool * bool * level * exp list -> exp *)
fun callExp (name,external,isproc,lev:level,ls) = 
	Ex (CONST 0) (*COMPLETAR*)

(* letExp : tigertree.stm list * exp -> exp *)
fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits,unEx body))

(* breakExp : unit -> exp *)
fun breakExp() = 
	Ex (CONST 0) (*COMPLETAR*)

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
					let val unexps = map unNx exps
					in Nx (seq unexps) end
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
		Nx (seq[LABEL l1,
			cf(l2,l3),
			LABEL l2,
			expb,
			JUMP(NAME l1, [l1]),
			LABEL l3])
	end

(* forExp : {lo: exp, hi: exp, var: exp, body: exp} -> exp *)
fun forExp {lo, hi, var, body} =
	Ex (CONST 0) (*COMPLETAR*)

(* ifThenExp : {test: exp, then': exp} -> exp *)
fun ifThenExp{test, then'} =
	Ex (CONST 0) (*COMPLETAR*)

(* ifThenElseExp : {test: exp, then': exp, else': exp} -> exp *)
fun ifThenElseExp {test,then',else'} =
	Ex (CONST 0) (*COMPLETAR*)

(* ifThenElseExpUnit : {test: exp, then': exp, else': exp} -> exp *)
fun ifThenElseExpUnit {test,then',else'} =
	Ex (CONST 0) (*COMPLETAR*)

(* assignExp : {var: exp, exp:exp}-> exp *)
fun assignExp{var, exp} =
	let
		val v = unEx var
		val vl = unEx exp
	in
		Nx (MOVE(v,vl))
	end

(* binOpIntExp : {left:exp, oper:tigerabs.oper, right:exp} -> exp *)
fun binOpIntExp {left, oper, right} = 
	Ex (CONST 0) (*COMPLETAR*)

(* binOpIntRelExp: {left:exp, oper:tigerabs.oper, right:exp} -> exp *)
fun binOpIntRelExp {left,oper,right} =
	Ex (CONST 0) (*COMPLETAR*)

(* binOpStrExp : {left:exp, oper:tigerabs.oper, right:exp} -> exp *)
fun binOpStrExp {left,oper,right} =
	Ex (CONST 0) (*COMPLETAR*)


end
