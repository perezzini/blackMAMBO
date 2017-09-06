structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres

open tigertopsort

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

(* bindings de funciones predefinidas de Tiger *)
val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=mainLevel, label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=mainLevel, label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=mainLevel, label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=mainLevel, label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=mainLevel, label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=mainLevel, label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=mainLevel, label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=mainLevel, label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=mainLevel, label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=mainLevel, label="exit",
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
					SOME(Func{lvl, lbl, fmls, rslt, extrn}) => (lvl, lbl, fmls, rstl, extrn)
					| SOME _ => error(f^": no es una función", nl)
					| NONE => error(f^": no existe tal función", nl)

				(* Ahora, debo comparar formals con args *)
				fun comparar [] [] r = r
					| comparar [] _ _= error(f^": toma demasiados argumentos", nl)
					| comparar _ [] _ = error(f^": toma pocos argumentos", nl)
					| comparar (t:tt) (a:aa) r = let
						val {exp=ae', ty=te'} = trexp a
						val _ = tiposIguales(t, te')
						handle _ => error(f^": argumento de tipo incorrecto")
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

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal(t,tenv) of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = ()
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty (!t) then verificar cs ds
							 else error("Error de tipo del campo "^s, nl)
				val _ = verificar cs tfields
			in
				{exp=(), ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=(), ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) =
			let
				(* Buscamos si existe la variable s en el entorno *)
				val _ = case tabBusca(s, venv) of
					SOME (Var{ty}) => ty
					| _ => error(s^": variable inexistente", nl)

				val {ty=tyexp, ...} = trexp exp
				val {ty=tyvar, ...} = trvar (SimpleVar s)
			in
				if tiposIguales tyexp tyvar then {exp=(), ty=TUnit} else error("Asignación errónea entre "^s^" y "^exp, nl)
			end
		| trexp(AssignExp({var, exp}, nl)) =
			let
				val {ty=tyexp} = trexp exp
				val {ty=tyval} = trvar var
			in
				if tiposIguales tyexp tyvar then {exp=(), ty=TUnit} else error("Asignación errónea entre "^s^" y "^exp, nl)
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
				let {ty=tylo, ...} = trexp lo
				let {ty=tyhi, ...} = trexp hi
				(* Chequeamos si lo y hi tienen tipo int *)
				val _ = if (tipoReal tylo <> TInt orelse (tipoReal tyhi) <> TInt then error(lo^" o "^hi^" no son de tipo int", nl)
				
				(* Debemos chequear que lo < hi? Cómo lo hago? *)

				let {ty=tyvar, ...} = trvar var
				let _ = if tipoReal tyvar = TInt () else error(var^" debe ser de tipo int", nl)

				(* Insertamos var en venv para que body pueda usarla *)
				val _ = tabInserta(var, lo, venv)
				val {ty=tybody} = trexp body
				(* Chequeamos que la expresión body produce ningun valor *)
				val _ = if tybody <> TUnit then error("El cuerpo de un for no puede devolver un valor", nl)

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
				(* Veamos si type está declarado en tenv como tipo TArray *)
				val typarr = case tabEsta(type, tenv) of
					SOME (TArray(t, _)) => t
					| SOME _ => error(typ^" no es de tipo Array", nl)
					| _ => error("El tipo "^typ^" no existe", nl)

				val {ty=tysize, ...} = trexp size
				val {ty=tyinit, ...} = trexp init

				(* size debe ser int *)
				val _ = if tipoReal tysize = TInt then () else error(size^" debe ser de tipo int", nl)
				(* init debe ser de tipo typ *)
				val _ = if tiposIguales tysize typ then () else error(init^" debe ser de tipo "^typ^, nl)
			in
				{exp=(), ty=typ}
			end

		and trvar(SimpleVar s, nl) =
			let
				val sty = case tabBusca(s, tenv) of
					SOME (Var{ty}) => ty
					| _ => error(s^:" variable indefinida", nl)
			in
				{exp=(), ty=sty}
			end
		| trvar(FieldVar(v, s), nl) =
			let
				val {ty=tyv} = trvar v

				(* tyv debe ser de tipo TRecord. Ademas debemos extraer la lista de fields *)
				val rFields = case tyv of
					TRecord (l, _) => l
					| _ => error(v^": debe ser de tipo record", nl)

				(* Debemos ver que el identifier, s, es un id de la lista de fields del record *)
				val idTrFields = List.map (fn (s, tr, i) => 
					s, tf) rFields
				val tyId = case List.find (fn (str, tr) => 
					if str=s then tr) of
					SOME (str, tr) => !tr
					| NONE => error(s^": no es un field del record "^v, nl)

			in
				{exp=(), ty=tyId}
			end
		| trvar(SubscriptVar(v, e), nl) =
			let
				{ty=tyv, ...} = trvar v
				{ty=tye, ...} = trexp e
			in
				case tye of
					TArray(t, _) => if tipoReal !t = TInt then {exp=(), ty=tyv} else error("El índice "^!t^" no es un entero", nl)
					| _ => error(e^" no es un array", nl)
			end

		(* lo más difícil. Por lo tanto, para ir probando lo demás, colocar "ex" en transExp *)
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
			let
				{ty=tyinit, ...} = trexp init

				(* Aumento venv con nuevo binding *)
				val venv' = tabInserta name (Var{ty=tyinit}) venv
			in
				(venv', tenv, [])
			end
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
			let
				(* Debemos ver que el tipo de name y typ son de tipos equivalentes *)
				val {ty=tyinit} = trexp init
				val tys = case tabBusca s tenv of
					SOME t => t
					| NONE => error(s^": no existe tal tipo", nl)

				val _ = if tiposIguales tyinit tys then () else error(name^" y "^s^" no son tipos comparables", nl)

				(* Aumento venv *)
				val venv' = tabInserta name (Var{ty=tys}) venv
			in
				(venv', tenv, [])
			end
		| trdec (venv,tenv) (FunctionDec []) =
			(venv, tenv, [])
		| trdec (venv,tenv) (FunctionDec fs) =
			let
				(* Busquemos si hay nombres de funciones repetidos en un mismo batch, ya que no se permite *)
				fun reps [] = false
					| reps (({name, ...}, nl) :: t) = if List.exist (fn ({name=x, ...}, nl') => 
						x=name) t then true else reps t

				val _ = if reps ldecs then error("trdec(FunctionDec): nombres de funciones repetidas en batch", nl) else ()

				(* Insertar funciones en del batch como Func's, juntos con sus argumentos en forma de Var en venv, y 
				analizar body *)
				fun insertar (({name, params, result, body}, nl)::fss) venv =
					let
						val venv' = tabInserta name Func{formals=params, result=result} venv

						fun insertarVar [] env = env
							insertarVar ({name, ...} :: ff) env = case tabBusca s venv of
								SOME tyname => tabInserta name Var{ty=tyname} (insertarVar ff env)
								| NONE => error("trdec(FunctionDec): tipo de argumento no definido", nl)

						let venv'' = insertarVar params venv'
							
					in
						venv'
					end

			in
				(insertar fs venv, tenv, [])
			end
		| trdec (venv,tenv) (TypeDec []) =
			(venv, tenv, [])
		| trdec (venv,tenv) (TypeDec ldecs) =
			let
				(* Busquemos si hay nombres de tipos repetidos en un mismo batch, ya que no se permite *)
				fun reps [] = false
					| reps (({name, ...}, nl) :: t) = if List.exist (fn ({name=x, ...}, nl') => 
						x=name) t then true else reps t

				val _ = if reps ldecs then error("trdec(TypeDec): nombres de tipos repetidos en batch", nl) else ()

				val ldecs' = map #1 ldecs (* sacamos los nl *)
				val tenv' = (tigertopsort.fijaTipos ldecs' tenv)
				handle tigertopsort.Ciclo => error("trdec(TypeDec): ciclo(s) en batch", nl)
			in
				(venv, tenv', [])
			end
	in trexp end
fun transProg ex =
	(* ponemos la expresion (AST) en una función de Tiger*)
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=NONE, body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) main (* ex *)
	in	print "bien!\n" end
end
