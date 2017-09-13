structure tigertips =
struct

type unique = unit ref
datatype Tipo = TUnit
	| TNil
	| TInt (* primitive, RW *)
	| TString (* primitive *)
	| TArray of Tipo ref  * unique
	| TRecord of (string * Tipo ref * int) list * unique (* int es la pos del id dentro del record *)
	| TTipo of string
end
