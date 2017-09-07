structure tigersres =
struct

open tigerabs
open tigertab
open tigertips

datatype EnvEntry =
	VIntro	(* int readonly; se utiliza sólo para la variable de iteración de un FOR *)
	| Var of {ty: Tipo}
	| Func of {level: unit, label: tigertemp.label,
		formals: Tipo list, result: Tipo, extern: bool}

val mainLevel = ()
end
