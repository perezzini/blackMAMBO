signature tigertrans = sig

exception breakexc
exception divCero

type level (* Nesting level of a function *)
type access
type frag = tigerframe.frag (* page 169, 262 *)

val outermost : level (* "Not the level of the Tiger main program, it's the level within which 
						that program is nested. All "library" functions are declared at this 
						outermost level, which doesn't contain a frame or formal parameter 
						list" - page 143 *)
val newLevel : {parent: level, name: tigertemp.label,
				formals: bool list} -> level (*	"In the seman analysis phase of the Tiger compiler, 
												transDec() creates a new nesting level for each 
												function by calling tigertrans.newLevel. This 
												function in turn calls tigerframe.newFrame to make 
												a new frame for the particular function. Semant 
												keeps this level in its FunEntry data structure for 
												the function. The function tigerseman.transDec will 
												make a new level for each Tiger function declaration. 
												tigertrans.newLevel must be told the enclosing 
												function's level (parent arg). This means that 
												tigerseman.transDec must know, while processing each 
												declaration. the current static nesting level" - page 141, 142.
											*)
val formals : level -> access list
val getActualLev : unit -> int
val allocArg : level -> bool -> access
val allocLocal : level -> bool -> access (* "allocLocal(lev)(esc): creates a local var in level lev; 
											the argument esc specifies whether the var escapes. The 
											result is a tigertrans.access (same as tigerframe.access)" 
											- page 141 *)

type exp

val procEntryExit : {level: level, body: exp} -> unit
val getResult : unit -> frag list
val unitExp : unit -> exp
val nilExp : unit -> exp
val intExp : int -> exp
val stringExp : string -> exp
val simpleVar : access * int -> exp
val varDec : access -> exp
val fieldVar : exp * int -> exp
val subscriptVar : exp * exp -> exp
val recordExp : (exp * int) list -> exp
val callExp : tigertemp.label * bool * bool * level * exp list -> exp
val letExp : tigertree.stm list * exp -> exp
val breakExp : unit -> exp
val seqExp : exp list -> exp
val preWhileForExp : unit -> unit
val postWhileForExp : unit -> unit
val whileExp : {test: exp, body: exp, lev:level} -> exp
val forExp : {lo: exp, hi: exp, var: exp, body: exp} -> exp
val ifThenExp : {test: exp, then': exp} -> exp
val ifThenElseExp : {test: exp, then': exp, else': exp} -> exp
val ifThenElseExpUnit : {test: exp, then': exp, else': exp} -> exp
val assignExp : {var: exp, exp:exp}-> exp
val preFunctionDec : unit -> unit
val functionDec : exp * level * bool -> exp
val postFunctionDec : unit -> unit
val binOpIntExp : {left:exp, oper:tigerabs.oper, right:exp} -> exp
val binOpIntRelExp: {left:exp, oper:tigerabs.oper, right:exp} -> exp
val binOpStrExp : {left:exp, oper:tigerabs.oper, right:exp} -> exp
val arrayExp : {size: exp, init: exp} -> exp

val Ir : frag list -> string

end