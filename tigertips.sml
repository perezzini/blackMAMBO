structure tigertips =
struct

type unique = unit ref
(*datatype R = RO | RW*)
datatype Tipo = TUnit
	| TNil
	| TInt (*TInt of R*) (* primitive *)
	| TString (* primitive *)
	| TArray of Tipo ref  * unique
	| TRecord of (string * Tipo ref * int) list * unique
	| TTipo of string 

end
