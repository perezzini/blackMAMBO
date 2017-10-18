structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertemp
open tigertrans

(* 	exp: Intermediate representation code (IR)
	ty: Expression Tiger type
*)
type expty = {exp: unit, ty: Tipo}

(* Environment de variables y funciones. Para cada identificador de variable debemos saber si es una variable 
	(Var) o una función (Func). Si es una variable,cuál es su tipo; y si es una función qué son sus argumemntos y 
	los tipos de retorno
*)
type venv = (string, EnvEntry) tigertab.Tabla

(* Environment de declaraciones de tipos (por ej: type a = int es un par ("a", TInt) en la tabla tenv) *)
type tenv = (string, Tipo) tigertab.Tabla

(* bindings de tipos de Tiger predefinidos *)
val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString)])

(* 	levelPila: pila de levels

	Mantiene los levels de forma externa, en vez de pasar como argumento getActualLev() 
	a cada caso de la función trexp.

	En la inicialización de un programa Tiger, la pila sólo contiene un elemento: el level 
	tigertrans.outermost (level = 0).
	Cuando se declara una función, tigertrans.newLevel utiliza topLevel().
	Esto se utiliza en transDec. Recordemos que transDec crea un nuevo "nesting level" para 
	cada función llamando a tigertrans.newLevel (esta función, a la vez, llama a 
	tigertrans.newFrame para crear un nuevo frame).
*)
val levelPila: tigertrans.level tigerpila.Pila = tigerpila.nuevaPila1(tigertrans.outermost) 
fun pushLevel l = tigerpila.pushPila levelPila l
fun popLevel() = tigerpila.popPila levelPila 
fun topLevel() = tigerpila.topPila levelPila
	

(* bindings de funciones predefinidas de Tiger
	"All library functions are declared at the outermost
	level, which doesn't contain a frame or formal 
	parameter list" - page 143

	extern=true en todas ya que son external calls
*)
val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=topLevel(), label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=topLevel(), label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=topLevel(), label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=topLevel(), label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=topLevel(), label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=topLevel(), label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=topLevel(), label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=topLevel(), label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=topLevel(), label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=topLevel(), label="exit",
		formals=[TInt], result=TUnit, extern=true})
	])

fun tipoReal (TTipo s, (env : tenv)) : Tipo = 
    (case tabBusca(s , env) of 
         NONE => raise Fail "tipoReal Ttipo"
       | SOME t => t)
  | tipoReal (t, _) = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo _) b =
		(* let *)
		(* 	val a = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (1)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)raise Fail "No debería pasar! (1)"
  | tiposIguales a (TTipo _) =
		(* let *)
		(* 	val b = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (2)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)raise Fail "No debería pasar! (2)"
  | tiposIguales a b = (a=b)

(* 	transExp: 
		* Type-checking de una expresión Tiger, y 
		* traduce expresiones en código intermedio (IR) - tigertrans
*)
fun transExp(venv, tenv) =
	let 
		fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")

		fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}
		| trexp(NilExp _)= {exp=nilExp(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=intExp i, ty=TInt}
		| trexp(StringExp(s, _)) = {exp=stringExp(s), ty=TString}
		| trexp(CallExp({func = f, args}, nl)) =
			let
				(* Buscamos la función en venv, para extraer información: sólo necesitamos formals y result type *)
				val (level, label, formals, result, extern) = case tabBusca(f, venv) of
					SOME (Func f) => (#level f, #label f, #formals f, #result f, #extern f)
					| SOME _ => error(f^": no es una función", nl)
					| NONE => error(f^": no existe tal función", nl)

				(* Ahora, debo comparar formals con args *)
				fun comparar [] [] r = r
					| comparar [] _ _= error(f^": toma demasiados argumentos", nl)
					| comparar _ [] _ = error(f^": toma pocos argumentos", nl)
					| comparar (t :: tt) (a :: aa) r = 
						let
							val {exp=ae', ty=te'} = trexp a
							val _ = if tiposIguales t te' 
	                                  then () 
	                                  else error("El(los) argumento(s) de "^f^" no coinciden con su declaración", nl)
						in
							comparar tt aa r@[{exp=ae', ty=te'}]
						end

				val leargs = comparar formals args []
				val leargs'= List.map #exp leargs

				val isproc = if tiposIguales TUnit result then true else false
			in
				{exp=callExp(f, extern, isproc, level, leargs'), ty=result}
			end
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then 
					{exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=EqOp,right=expr} else binOpIntRelExp {left=expl,oper=EqOp,right=expr}, ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then 
					{exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=NeqOp,right=expr} else binOpIntRelExp {left=expl,oper=NeqOp,right=expr}, ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if tipoReal(tyl, tenv)=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
						| MinusOp => if tipoReal(tyl, tenv)=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
						| TimesOp => if tipoReal(tyl, tenv)=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
						| DivideOp => if tipoReal(tyl, tenv)=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
						| LtOp => if tipoReal(tyl, tenv)=TInt orelse tipoReal(tyl, tenv)=TString then
							{exp=if tipoReal(tyl, tenv)=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} 
							else error("Error de tipos", nl)
						| LeOp => if tipoReal(tyl, tenv)=TInt orelse tipoReal(tyl, tenv)=TString then 
							{exp=if tipoReal(tyl, tenv)=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} 
							else error("Error de tipos", nl)
						| GtOp => if tipoReal(tyl, tenv)=TInt orelse tipoReal(tyl, tenv)=TString then
							{exp=if tipoReal(tyl, tenv)=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} 
							else error("Error de tipos", nl)
						| GeOp => if tipoReal(tyl, tenv)=TInt orelse tipoReal(tyl, tenv)=TString then
							{exp=if tipoReal(tyl, tenv)=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} 
							else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo y verificar que realmente es un record *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal(t,tenv) of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* 	Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde.
					A cada expresión de cada field del record se le asigna la posición correspondiente, 
					donde la expresión se encuentra ya traducida en código intermedio.
				*)
				fun verificar _ [] [] = []
				  | verificar _ (c::cs) [] = error("Faltan campos", nl)
				  | verificar _ [] (c::cs) = error("Sobran campos", nl)
				  | verificar n ((s, t, _)::cs) ((sy, {exp, ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty (!t) then (exp, n)::(verificar (n+1) cs ds)
							 else error("Error de tipo del campo "^s, nl)

				val lf = verificar 0 cs tfields
			in
				{exp=recordExp lf, ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	
				val _ = if List.length s < 2 then error("La secuencia tiene menos de dos expresiones", nl) else ()

				val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti

				(* Se evalúa el tipo y se genera código intermedio de la última expresión 
					de la secuencia
				*)
				val {exp, ty=tipo} = hd(rev lexti)
			in	
				{exp=seqExp(exprs), ty=tipo} 
			end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) =
			let
				(* Buscamos si existe la variable s en el entorno *)
				val _ = case tabBusca(s, venv) of
					SOME (Var{ty, ...}) => () 
					| SOME (VIntro{...}) => error(s^": asignación de variable de sólo lectura", nl)
					| SOME (Func{...}) => error(s^" es una función y no una variable", nl)
					| _ => error(s^": variable inexistente", nl)

				val {ty=tyexp, exp=expe} = trexp exp
				val {ty=tyvar, exp=expv} = trvar (SimpleVar s, nl)
			in
				if tiposIguales tyexp tyvar then {exp=assignExp{var=expv, exp=expe}, ty=TUnit} else error("Error de tipos para la asignación de la variable "^s, nl)
			end
		| trexp(AssignExp({var, exp}, nl)) =
			let
				val {ty=tyexp, exp=expe} = trexp exp
				val {ty=tyvar, exp=expv} = trvar (var, nl)
			in
				if tiposIguales tyexp tyvar then {exp=assignExp{var=expv, exp=expe}, ty=TUnit} else error("Error de tipos para asignación", nl)
			end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let 
				val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tipoReal(tytest, tenv)=TInt andalso tiposIguales tythen tyelse then
				{exp=if tipoReal(tythen, tenv)=TUnit then ifThenElseExpUnit{test=testexp, then'=thenexp, else'=elseexp} else ifThenElseExp {test=testexp, then'=thenexp, else'=elseexp}, ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let 
				val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tipoReal(tytest, tenv)=TInt andalso tythen=TUnit then
				{exp=ifThenExp{test=exptest, then'=expthen}, ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test

				val _ = preWhileForExp()

				val tbody = trexp body

				val exptrans = whileExp {test=(#exp ttest), body=(#exp tbody), lev=topLevel()}

				val _ = postWhileForExp()

			in
				if tipoReal(#ty ttest, tenv) = TInt andalso #ty tbody = TUnit then {exp=exptrans, ty=TUnit}
				else if tipoReal(#ty ttest, tenv) <> TInt then error("Error de tipo en la condición", nl)
				else error("El cuerpo de un while no puede devolver un valor", nl)
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
			let
				(* Veamos que lo y hi tienen tipo TInt *)
				val {ty=tylo, exp=explo} = trexp lo
				val {ty=tyhi, exp=exphi} = trexp hi
				val _ = if tipoReal(tylo, tenv) = TInt andalso tiposIguales tylo tyhi then () else error("Las cotas del for no son de tipo TInt", nl)
			
				(* Acceso de var *)
				val access' = allocLocal(topLevel()) (!escape)

				val level' = getActualLev()

				val _ = preWhileForExp()

				val venv' = tabRInserta(var, 
										VIntro{access=access', level=level'}, 
										venv)

				val {exp=eb', ty=tb'} = transExp (venv', tenv) body

				(* tb' debe ser TUnit *)
				val _ = if tiposIguales tb' TUnit then () else error("El body de una expresión For debe ser TUnit", nl)

				(* Preparamos código intermedio *)
				val ev' = simpleVar(access', 0)
				val ef' = forExp{lo=explo, hi=exphi, var=ev', body=eb'}

				val _ = postWhileForExp()
			in
				{exp=ef', ty=TUnit}
			end
		| trexp(LetExp({decs, body}, _)) =
			let
				fun aux (d, (v, t, exps1)) =
					let
						val (v', t', exps2) = trdec (v, t) d
					in
						(v', t', exps1@exps2)
					end
					
				val (venv', tenv', expdecs) = List.foldl aux (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{exp=seqExp(expdecs@[expbody]), ty=tybody}
			end
		| trexp(BreakExp nl) =
			({exp=breakExp(), ty=TUnit}
				handle Empty => error("break fuera de un while/for", nl))
		| trexp(ArrayExp({typ, size, init}, nl)) =
			let
				(* Veamos si typ está declarado en tenv como tipo TArray *)
				val (typarr, u) = case tabBusca(typ, tenv) of
					SOME a => (case tipoReal(a, tenv) of
						TArray (t, u) => (t, u)
						| _ => error("El tipo "^typ^" no es un TArray", nl))
					| _ => error("El tipo "^typ^" no existe", nl)
					
				val {ty=tysize, exp=expsize} = trexp size
				val {ty=tyinit, exp=expinit} = trexp init

				(* size debe ser int *)
				val _ = if tipoReal(tysize, tenv) = TInt then () else error("El tamaño de un arreglo debe ser de tipo TInt", nl)
				(* init debe ser de tipo typarr *)
				val _ = if tiposIguales tyinit (!typarr) then () else error("El tipo de la expresión de inicialización de arreglo y el tipo del arreglo no coinciden", nl)
			in
				{exp=arrayExp{size=expsize, init=expinit}, ty=TArray (typarr, u)}
			end

		and trvar(SimpleVar s, nl) =
			let
				(* "The identifier must refer to a variable. The result type is 
					the type of the variable"
				*)
				val (sty, access, level) = case tabBusca(s, venv) of
					SOME (Var{ty, access, level}) => (ty, access, level)
					| SOME (VIntro{access, level}) => (TInt, access, level)
					| _ => error(s^": variable indefinida", nl)
			in
				{exp=simpleVar(access, level), ty=sty}
			end
		| trvar(FieldVar(v, s), nl) =
			(* v debe tener tipo TRecord, y s debe ser un ID del record. El tipo resultado es el tipo del 
			campo ID dentro del record *)
			let
				val {ty=tyv, exp=expv} = trvar (v, nl)

				(* tyv debe ser de tipo TRecord. Ademas debemos extraer la lista de fields *)
				val recordFields = case tipoReal(tyv, tenv) of
					TRecord (l, _) => l
					| _ => error("No es un TRecord", nl)

				(* Debemos ver que el identifier, s, es un id de la lista de fields del record. tyId es el tipo del 
				identifier s en el record *)
				val (tyId, pos) = case List.find (fn (str, _, _) => str=s) recordFields of
					SOME (str, tyfield, i) => (tipoReal(!tyfield, tenv), i)
					| NONE => error(s^": no es un field del record", nl)

			in
				{exp=fieldVar(expv, pos), ty=tyId}
			end
		| trvar(SubscriptVar(v, e), nl) =
			(* v debe ser de tipo TArray, y la expresión e debe ser de tipo TInt. El tipo resultado 
			es el tipo del elemento del array *)
			let
				(* La variable v debe pertencer a venv y debe tener tipo Var{TArray(t, _)} *)
				val {ty=tyv, exp=expv} = trvar (v, nl)

				(* Index expression debe ser TInt *)
				val {ty=tye, exp=expe} = trexp e
				val _ = if tiposIguales tye TInt then () else error("La expresión utilizada como índice no es de tipo TInt", nl)
			in
				case tipoReal(tyv, tenv) of
					TArray(t, _) => {exp=subscriptVar(expv, expe), ty=(!t)}
					| _ => error("La variable no es de tipo TArray", nl)
			end

		(* 	DECLARACIONES DE VARIABLES, FUNCIONES, Y TIPOS

			"The call to transDec will not only return a result record (containing a
			new type env, value env, and tigertrans.exp) but also will have side 
			effects: for each variable declaration within the declaration, additional 
			space will be reserved in the current level's frame. Also, for each function 
			declaration, a new fragment of Tree code will be kept for the function's 
			body. ...Therefore, transDec must also return an exp list of assignment 
			expressions that accomplish these initializations.
			If transDec is applied to function and type declarations, the exp list 
			will be empty" - page 167
		*)

		and trdec (venv, tenv) (VarDec ({name, escape, typ=NONE, init}, nl)) = 
			let
				(* "If the declaration explicity specifies a type, it must match 
					the type of the initializer. The type of the variable is the 
					explicitly specified type, or, if missing, the initializer 
					type"
				*)

				val {ty=tyinit, exp=expinit} = transExp(venv, tenv) init

				val _ = case tipoReal(tyinit,tenv) of
					TNil => error("No se puede asignar nil a variable sin saber si es de tipo record", nl)
					| _ => ()

				(* preparo código intermedio *)
				val level' = getActualLev() (* número de actual level *)
				val access' = allocLocal(topLevel()) (!escape)

				(* Aumento venv con nuevo binding *)
				val venv' = tabRInserta (name, Var{ty=tyinit, access=access', level=level'}, venv)

				val transVar = varDec(access')
			in
				(venv', tenv, [assignExp{var=transVar, exp=expinit}])
			end
		| trdec (venv, tenv) (VarDec ({name, escape, typ=SOME s, init}, nl)) =
			let
				(* Debemos ver que el tipo de name y typ son de tipos equivalentes *)
				val {ty=tyinit, exp=expinit} = transExp(venv, tenv) init

				val tys = case tabBusca(s, tenv) of
					SOME t => t
					| NONE => error(s^": no existe tal tipo", nl)

				val _ = case tipoReal(tyinit, tenv) of
					TNil => (case tipoReal(tys, tenv) of
						TRecord _ => ()
						| _ => error("No se puede asignar nil si la variable "^name^" no se inicializa con tipo record", nl))
					| _ => ()

				val _ = if tiposIguales tyinit tys then () else error("El tipo de " ^name^" y el tipo "^s^" no son comparables", nl)

				(* En caso contrario, se aumenta venv *)

				(* preparo código intermedio *)
				val level' = getActualLev() (* número de actual level *)
				val access' = allocLocal(topLevel()) (!escape)

				val venv' = tabRInserta(name, Var{ty=tys, access=access', level=level'}, venv)

				val transVar = varDec(access')
			in
				(venv', tenv, [assignExp{var=transVar, exp=expinit}])
			end
		| trdec (venv, tenv) (FunctionDec fs) =
			let
				(* Busquemos si hay nombres de funciones repetidos en un mismo batch, ya que no se permite *)
				fun reps [] = false
					| reps (({name, ...}, nl) :: t) = if List.exists (fn ({name=x, ...}, nl') => 
						x=name) t then true else reps t

				val _ = if reps fs then raise Fail ("trdec(FunctionDec): nombres de funciones repetidas en batch") else ()

				(* Traduce lista de argumentos de una función en su verdadero tipo (de tipo Tipo), si es que está definido *)
				fun traducirParams [] = []
					| traducirParams ({typ=NameTy s, name, escape} :: pp) = 
						(case tabBusca(s, tenv) of
							SOME t => ({name=name, escape=(!escape), ty=t} :: traducirParams pp)
							| NONE => raise Fail ("tredec(FunctionDec), traducirParams(): el parámetro de una función no está definido"))
					| traducirParams _ = raise Fail ("trdec(FunctionDec), traducirParams(): no debería pasar, Tiger no acepta argumentos de funciones de tipo record o array")


				fun symbolToTipo s = case s of
					SOME s' => (case tabBusca(s', tenv) of
						SOME t => t
						| NONE => raise Fail ("trdec(FunctionDec): "^s'^", tipo de retorno de función no existente"))
					| NONE => TUnit
				

				(* Inserta funciones del batch en el environment de funciones y variables *)
				fun insertarFunciones [] env : (string, EnvEntry) Tabla = env
					| insertarFunciones (({name, params, result, ...}, nl) :: fss) env =
						let
							(* Analizo que el tipo del resultado de una función esté definido *)
							val funResultType = symbolToTipo result

							(* Analizo los tipos de los argumentos de la función, en busca de alguno no definido en tenv *)
							val arguments = traducirParams params

							val funLabel = if name = "_tigermain" then name else name^"_"^newlabel()
							val funLevel = newLevel{parent=topLevel(),
													name=funLabel,
													formals=map (! o #escape) params}
							val funFormals = map #ty arguments

							(* Inserto funciones en environment *)
							val env' = tabRInserta (name, 
													Func{level=funLevel,
															label=funLabel,
															formals=funFormals,
															result=funResultType,
															extern=false}, (* false, ya que las funciones externas se definen en runtime *)
													insertarFunciones fss env)						
						in
							env'
						end

				(* Nuevos environments donde están definidas las nuevas funciones *)
				val venv' = insertarFunciones fs venv (* Este es el que retorno *)
								
				(* agregarParams : field * env -> env *)
				fun agregarParams [] env = env
					| agregarParams ({name, escape, typ=NameTy s} :: pp) env = (case tabBusca(s, tenv) of
						SOME t => tabRInserta(name, 
												Var{ty=t, access=allocLocal(topLevel()) (!escape), level=getActualLev()}, 
												agregarParams pp env)
						| _ => raise Fail ("trdec(FunctionDec), agregarParams(): se quiere agregar argumento de función con tipo indefinido"))
					| agregarParams _ _ = raise Fail ("trdec(FunctionDec), agregarParams(): no debería pasar; Tiger no acepta argumentos de función con tipo array o record")

				(* Analiza body de una función: agrega parámetros y evalúa, con ellos, la expresión body *)
				fun analizarBody name params result body env =
					let
						val venv'' = agregarParams params env

						val isproc = case result of
							SOME _ => false
							| _ => true

						val level = case tabBusca(name, env) of
							SOME (Func{level, ...}) => level
							| _ => raise Fail "Error interno al analizar cuerpo de función"

						(* Aumentamos un nivel *)
						val _ = pushLevel level

						val {ty=tybody, exp=expbody} = transExp (venv'', tenv) body

						val transFunctionDec = functionDec(expbody, level, isproc)

						(* Volvemos al nivel anterior *)
						val _ = popLevel()
					in
						tybody
					end


				(* Analizo todos los bodies de las funciones del batch con venv'' *)
				val functionTypes = List.map (fn ({name, params, result, body}, _) => 
					let
						val _ = preFunctionDec()
						val tybody = analizarBody name params result body venv'
						val _ = postFunctionDec()
					in
						tybody
					end) fs

				(* Los tipos que devuelven, por definición, cada función del batch *)
				val batchFunctionTypes = List.map (fn ({result, ...}, _) =>
					symbolToTipo result) fs

				(* Debugging: entrega1
				val _ = tigermuestratipos.printTipo("FunctionDec, functionTypes header", hd functionTypes, tabAList(tenv))
				val _ = tigermuestratipos.printTipo("FunctionDec, batchFunctionTypes header", hd batchFunctionTypes, tabAList(tenv))
				*)
				
				fun compareListsTipos (x :: xs) (y :: ys) = if tiposIguales x y then true else false
					| compareListsTipos _ _ = false

				(* NOTA: se supone que ambas anteriores tienen la misma longitud *)
				val _ = if compareListsTipos functionTypes batchFunctionTypes then () else raise Fail ("trdec(FunctionDec): tipos de resultados de funciones analizadas no coinciden")

			in
				(* 3er elemento: lista vacía *)
				(venv', tenv, [])
			end	
		| trdec (venv, tenv) (TypeDec ldecs) =
			let
				(* Busquemos si hay nombres de tipos repetidos en un mismo batch, ya que no se permite *)
				fun reps [] = false
					| reps (({name, ...}, nl) :: t) = if List.exists (fn ({name=x, ...}, nl') => 
						x=name) t then true else reps t

				val _ = if reps ldecs then raise Fail ("trdec(TypeDec): nombres de tipos repetidos en batch") else ()

				val ldecs' = map #1 ldecs (* sacamos los nl *)
				val tenv' = (tigertopsort.fijaTipos ldecs' tenv)
				handle tigertopsort.Ciclo => raise Fail ("trdec(TypeDec): ciclo(s) en batch")
			in
				(* 3er elemento: lista vacía *)
				(venv, tenv', [])
			end
	in 
		trexp 
	end

fun transProg ex =

	(* Entrega2: "The result of calling transProg should be a 
		tigertrans.frag list" - page 170
	*)
	let	val main =
				(* ponemos la expresion (AST) en una función de Tiger *)
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=SOME ("int"), body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val {ty=tyt, exp=expt} = transExp(tab_vars, tab_tipos) main (* use 'ex' to get real type of input expression *)
	in	
		(tigermuestratipos.printTipo("\nTipo final del programa", tyt, tabAList(tab_tipos));
		print "\n")
	end
end
