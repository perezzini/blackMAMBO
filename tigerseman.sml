structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertemp

(* Código intermedio *)
open tigertrans

(* type expty = {exp: Translate.exp, ty: Tipo}. Usamos () por el momento, ya que no tenemos el módulo Translate *)
(* expty: expression type *)
type expty = {exp: unit, ty: Tipo}

(* Environment de variables y funciones. Para cada identificador de variable debemos saber si es una variable 
(Var) o una función (Func). Si es una variable,cuál es su tipo; y si es una función qué son sus argumemntos y 
los tipos de retorno *)
type venv = (string, EnvEntry) tigertab.Tabla

(* Environment de declaraciones de tipos (por ej: type a = int es un par ("a", TInt) en la tabla tenv) *)
type tenv = (string, Tipo) tigertab.Tabla

(* bindings de tipos de Tiger predefinidos *)
val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString)])

(* 	levelPila: pila de levels
	Mantiene los levels de forma externa.
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

fun tipoReal (TTipo (s, ref (SOME (t)))) = tipoReal t
  | tipoReal t = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo (_, r)) b =
		let
			val a = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (1)"
		in
			tiposIguales a b
		end
  | tiposIguales a (TTipo (_, r)) =
		let
			val b = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (2)"
		in
			tiposIguales a b
		end
  | tiposIguales a b = (a=b)

(* Chequea tipos en el AST, y traduce expresiones en código intermedio *)
fun transExp(venv, tenv) =
	let fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=(), ty=TUnit}
		| trexp(NilExp _)= {exp=(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=(), ty=TInt}
		| trexp(StringExp(s, _)) = {exp=(), ty=TString}
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
					| comparar (t :: tt) (a ::aa) r = 
						let
							val {exp=ae', ty=te'} = trexp a
							val _ = if tiposIguales t te' 
	                                  then () 
	                                  else error("El(los) argumento(s) de "^f^" no coinciden con su declaración", nl)
						in
							comparar tt aa r@[{exp=ae', ty=te'}]
						end

				val leargs = comparar formals args []
				val leargs'= List.map #exp leargs (* sacamos código intermedio *)
			in
				{exp=(), ty=result}
			end
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if tipoReal(tyl, tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| MinusOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| TimesOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| DivideOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| LtOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| LeOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| GtOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| GeOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Verifico que typ sea de tipo TRecord *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal(t,tenv) of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = ()
				  | verificar (c :: cs) [] = error("Faltan campos", nl)
				  | verificar [] (c :: cs) = error("Sobran campos", nl)
				  | verificar ((s, t, _) :: cs) ((sy, {exp, ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty (!t) then verificar cs ds
							 else error("Error de tipo del campo "^s, nl)
				val _ = verificar cs tfields
			in
				{exp=(), ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	
				val _ = if List.length s < 2 then error("La secuencia tiene menos de dos expresiones", nl) else ()
				val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	
				{ exp=(), ty=tipo } 
			end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) =
			let
				(* Buscamos si existe la variable s en el entorno *)
				val _ = case tabBusca(s, venv) of
					SOME (Var{ty}) => ty
					| SOME (VIntro) => error(s^": asignación de variable de sólo lectura", nl)
					| SOME (Func{...}) => error(s^" es una función y no una variable", nl)
					| _ => error(s^": variable inexistente", nl)

				val {ty=tyexp, ...} = trexp exp
				val {ty=tyvar, ...} = trvar (SimpleVar s, nl)
			in
				if tiposIguales tyexp tyvar then {exp=(), ty=TUnit} else error("Error de tipos para la asignación de la variable "^s, nl)
			end
		| trexp(AssignExp({var, exp}, nl)) =
			let
				val {ty=tyexp, ...} = trexp exp
				val {ty=tyvar, ...} = trvar (var, nl)
			in
				if tiposIguales tyexp tyvar then {exp=(), ty=TUnit} else error("Error de tipos para asignación", nl)
			end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tipoReal(tytest,tenv)=TInt andalso tiposIguales tythen tyelse then {exp=(), ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tipoReal(tytest,tenv)=TInt andalso tythen=TUnit then {exp=(), ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
				val tbody = trexp body
			in
				if tipoReal(#ty ttest, tenv) = TInt andalso #ty tbody = TUnit then {exp=(), ty=TUnit}
				else if tipoReal(#ty ttest, tenv) <> TInt then error("Error de tipo en la condición", nl)
				else error("El cuerpo de un while no puede devolver un valor", nl)
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
			let
				(* Veamos que lo y hi tienen tipo TInt *)
				val {ty=tylo, ...} = trexp lo
				val {ty=tyhi, ...} = trexp hi
				val _ = if tipoReal(tylo, tenv)=TInt andalso tiposIguales tylo tyhi then () else error("Las cotas del for no son de tipo TInt", nl)
			
				val venv' = tabRInserta(var, VIntro, venv)

				val {ty=tybody, ...} = transExp (venv', tenv) body

				(* El tipo del body debe ser TUnit *)
				val _ = if tiposIguales TUnit tybody then () else error("El tipo del body del for debe ser TUnit", nl)
			in
				{exp=(), ty=TUnit}
			end
		| trexp(LetExp({decs, body}, _)) =
			let
				val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{exp=(), ty=tybody}
			end
		| trexp(BreakExp nl) =
			{exp=(), ty=TUnit}
		| trexp(ArrayExp({typ, size, init}, nl)) =
			let
				(* Veamos si typ está declarado en tenv como tipo TArray *)
				val (typarr, u) = case tabBusca(typ, tenv) of
					SOME a => (case tipoReal(a, tenv) of
						TArray (t, u) => (t, u)
						| _ => error("El tipo "^typ^" no es un TArray", nl))
					| _ => error("El tipo "^typ^" no existe", nl)
					
				val {ty=tysize, ...} = trexp size
				val {ty=tyinit, ...} = trexp init

				(* size debe ser int *)
				val _ = if tipoReal (tysize, tenv) = TInt then () else error("El tamaño de un arreglo debe ser de tipo TInt", nl)
				(* init debe ser de tipo typarr *)
				val _ = if tiposIguales tyinit (!typarr) then () else error("El tipo de la expresión de inicialización de arreglo y el tipo del arreglo no coinciden", nl)
			in
				{exp=(), ty=TArray (typarr, u)}
			end

		and trvar(SimpleVar s, nl) =
			let
				val sty = case tabBusca(s, venv) of
					SOME (Var{ty}) => ty
					| SOME (VIntro) => TInt
					| _ => error(s^": variable indefinida", nl)
			in
				{exp=(), ty=sty}
			end
		| trvar(FieldVar(v, s), nl) =
			(* v debe tener tipo TRecord, y s debe ser un ID del record. El tipo resultado es el tipo del 
			campo ID dentro del record *)
			let
				val {ty=tyv, ...} = trvar (v, nl)

				(* tyv debe ser de tipo TRecord. Ademas debemos extraer la lista de fields *)
				val recordFields = case tipoReal(tyv, tenv) of
					TRecord (l, _) => l
					| _ => error("No es un TRecord", nl)

				(* Debemos ver que el identifier, s, es un id de la lista de fields del record. tyId es el tipo del 
				identifier s en el record *)
				val tyId = case List.find (fn (str, tyref, i) => str=s) recordFields of
					SOME (str, tyref, i) => tipoReal(!tyref, tenv)
					| NONE => error(s^": no es un field del record", nl)

			in
				{exp=(), ty=tyId}
			end
		| trvar(SubscriptVar(v, e), nl) =
			(* v debe ser de tipo TArray, y la expresión e debe ser de tipo TInt. El tipo resultado 
			es el tipo del elemento del array *)
			let
				(* La variable v debe pertencer a venv y debe tener tipo Var{TArray(t, _)} *)
				val {ty=tyv, ...} = trvar (v, nl)

				(* Index expression debe ser TInt *)
				val {ty=tye, ...} = trexp e
				val _ = if tipoReal (tye, tenv) = TInt then () else error("La expresión utilizada como índice no es de tipo TInt", nl)
			in
				case tipoReal(tyv, tenv) of
					TArray(t, _) => {exp=(), ty=(!t)}
					| _ => error("La variable no es de tipo TArray", nl)
			end

		(* lo más difícil. Por lo tanto, para ir probando lo demás, colocar "ex" en transExp *)
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},nl)) = 
			let
				val {ty=tyinit, ...} = transExp(venv, tenv) init

				val _ = case tipoReal(tyinit, tenv) of
					TNil => error("No se puede asignar nil a variable sin saber si es de tipo record", nl)
					| _ => ()

				(* Aumento venv con nuevo binding *)
				val venv' = tabRInserta (name, Var{ty=tyinit}, venv)
			in
				(venv', tenv, [])
			end
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},nl)) =
			let
				(* Debemos ver que el tipo de name y typ son de tipos equivalentes *)
				val {ty=tyinit, ...} = transExp(venv, tenv) init

				val tys = case tabBusca(s, tenv) of
					SOME t => t
					| NONE => error(s^": no existe tal tipo", nl)

				val _ = case tipoReal(tyinit, tenv) of
					TNil => (case tipoReal(tys, tenv) of
						TRecord _ => ()
						| _ => error("No se puede asignar nil si la variable "^name^" no se inicializa con tipo record", nl))
					| _ => ()

				val _ = if tiposIguales tyinit tys then () else error("El tipo de " ^name^" y el tipo "^s^" no son comparables", nl)

				(* En caso contrario, aumento venv *)
				val venv' = tabRInserta(name, Var{ty=tys}, venv)
			in
				(venv', tenv, [])
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
							val resultType = symbolToTipo result

							(* Analizo los tipos de los argumentos de la función, en busca de alguno no definido en tenv *)
							val arguments = traducirParams params

							(* Inserto funciones en environment *)
							val env' = tabRInserta (name, 
													Func{level=(),
															label=name ^ newlabel(),
															formals=map #ty arguments,
															result=resultType,
															extern=false}, (* false, ya que las funciones externas se definen en runtime *)
													insertarFunciones fss env)						
						in
							env'
						end

				(* Nuevo environment donde están definidas las nuevas funciones *)
				val venv' = insertarFunciones fs venv (* Este es el que debo retornar *)
				val venv'' = insertarFunciones fs venv (* Este es el que debo usar para analizar los bodies de las funciones del batch *)
						

				fun agregarParams [] env = env
					| agregarParams ({typ=NameTy s, name, ...} :: pp) env = (case tabBusca(s, tenv) of
						SOME t => tabRInserta(name, Var{ty=t}, agregarParams pp env)
						| _ => raise Fail ("trdec(FunctionDec), agregarParams(): se quiere agregar argumento de función con tipo indefinido"))
					| agregarParams _ _ = raise Fail ("trdec(FunctionDec), agregarParams(): no debería pasar; Tiger no acepta argumentos de función con tipo array o record")

				(* Analiza body de una función: agrega parámetros y evalúa, con ellos, la expresión body *)
				fun analizarBody params body env = 
					let
						val {ty=tybody, ...} = transExp ((agregarParams params env), tenv) body 
					in
						tybody
					end

				(* Analizo todos los bodies de las funciones del batch con venv'' *)
				val functionTypes = List.map (fn ({params, body, ...}, _) => 
					analizarBody params body venv'') fs

				(* Los tipos que devuelven, por def, cada función del batch *)
				val batchFunctionTypes = List.map (fn ({result, ...}, _) =>
					symbolToTipo result) fs

				(* Debugging... *)
				val _ = tigermuestratipos.printTipo("FunctionDec, functionTypes header", hd functionTypes, tabAList(tenv))
				val _ = tigermuestratipos.printTipo("FunctionDec, batchFunctionTypes header", hd batchFunctionTypes, tabAList(tenv))

				fun compareListsTipos (x :: xs) (y :: ys) = if tiposIguales x y then true else false
					| compareListsTipos _ _ = false

				(* NOTA: se supone que ambas anteriores tienen la misma longitud *)
				val _ = if compareListsTipos functionTypes batchFunctionTypes then () else raise Fail ("trdec(FunctionDec): tipos de resultados de funciones analizadas no coinciden")

			in
				(venv', tenv, [])
			end	
		| trdec (venv,tenv) (TypeDec ldecs) =
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
				(venv, tenv', [])
			end
	in 
		trexp 
	end

fun transProg ex =
	(* ponemos la expresion (AST) en una función de Tiger *)
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=SOME ("int"), body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val {ty=tyt, ...} = transExp(tab_vars, tab_tipos) ex (* main *)
	in	
		(tigermuestratipos.printTipo("\nTipo final del programa", tyt, tabAList(tab_tipos));
		print "\n")
	end
end
