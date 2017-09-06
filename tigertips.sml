structure tigertips =
struct

type unique = unit ref
(*datatype R = RO | RW*)
datatype Tipo = TUnit (* Cuando produce ningún valor *)
	| TNil
	| TInt (*TInt of R*) (* primitive *)
	| TString (* primitive *)
	| TArray of Tipo ref  * unique
	| TRecord of (string * Tipo ref * int) list * unique
	(*| TFunc of Tipo list * Tipo*) (* lo usamos? *)
	| TTipo of string (* TName? *)

end
