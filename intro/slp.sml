type id = string

datatype binop = 
	Plus 
	| Minus
	| Times
	| Div

datatype stm = 
	CompoundStm of stm * stm 
	| AssignStm of id * exp
	| PrintStm of exp list
and exp = IdExp of id
	| NumExp of int
	| OpExp of exp * binop * exp
	| EseqExp of stm * exp

val prog = CompoundStm (AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)), 
		CompoundStm (AssignStm ("b", EseqExp (PrintStm [IdExp "a", OpExp (IdExp "a", Minus, NumExp 1)], OpExp(NumExp 10, Plus, IdExp "a"))), PrintStm [IdExp "b"]))

(* prog : stm *)

(* Some exercises *)
fun maxargs (s:stm) : int =
    case s of 
	CompoundStm (s1,s2) => Int.max (maxargs s1, maxargs s2)
      | AssignStm (_,e) => (maxargs_expr e)
      | PrintStm l => (List.length l)
and maxargs_expr (e:exp) : int = 
    case e of 
	IdExp _ => 0
      | NumExp _ => 0
      | OpExp (e1,_,e2) => Int.max (maxargs_expr e1, maxargs_expr e2)
      | EseqExp (s1,e1) => Int.max (maxargs s1, maxargs_expr e1)