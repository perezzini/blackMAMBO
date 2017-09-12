structure tigertips =
struct

type unique = unit ref
(*datatype R = RO | RW*)
datatype Tipo = TUnit (* Cuando produce ningún valor *)
	| TNil
	| TInt (* primitive, RW *)
	| TString (* primitive *)
	| TArray of Tipo ref  * unique
	| TRecord of (string * Tipo ref) list * unique (* No uso la posición del field dentro del record. Uso estrictamente la espec de Tiger *)
	| TTipo of string

end
