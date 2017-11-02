(*	"Every target machine architecture will have a different standard stack frame layout.
	If we want Tiger functions to be able to call C functions, we should use the 
	standard layout" - page 134 
*)
signature tigerframe =
sig

type frame (* Holds info about formal parameters and local variables allocated in this frame *)

type register = string

val rv : tigertemp.temp
val fp : tigertemp.temp
val sp : tigertemp.temp
val rax : tigertemp.temp
val rdx : tigertemp.temp

val wSz : int
val fpPrev : int
val fpPrevLev : int

val calldefs : tigertemp.temp list
val callersaves : tigertemp.temp list
val argregs : tigertemp.temp list


datatype access = InFrame of int 
				| InReg of tigertemp.label (* Describes formals and locals that may be in the frame 
											or in registers. InFrame(X) indicates a memory location 
											at offset X from the frame pointer. InReg(t84) indicates 
											that it will be held in "register" t84 *)

val newFrame : {name: tigertemp.label, formals: bool list} -> frame (* Creates a new frame for a function f with k 
																	formal parameters. Call newFrame{name=f, formals=l}, 
																	where l is a list of k booleans: true for each 
																	parameter that escapes (needs to be kept in memory), 
																	and false for each parameter that does not. 
																	The result will be a frame object *)
val name : frame -> tigertemp.label
val string : tigertemp.label * string -> string
val formals : frame -> access list (* Extracts a list of k "accesses" denoting the locations where the formal 
									parameters will be kept at run-time, as seen from inside the callee. 
									Parameters may be seen differently by the caller and the callee *)
val allocArg : frame -> bool -> access
val allocLocal : frame -> bool -> access
val exp : access -> tigertree.exp -> tigertree.exp
val externalCall : string * tigertree.exp list -> tigertree.exp (* Referes to an external function which is 
																	written in C or assembly lang - it cannot 
																	be written in Tiger because Tiger has no 
																	mechanism for manipulating raw memory. The 
																	calling conventions for C may differ from 
																	those of Tiger functions; and C functions 
																	don't expect to receive a static link, and 
																	so on. externalCall takes the name of the 
																	external procedure and the arguments to be 
																	passed. The implementation may have to be 
																	adjusted for static links, or "_" in labels, 
																	and so on. Page 156 *)
val printAL : access list -> string

val procEntryExit1 : frame * tigertree.stm -> tigertree.stm


(* Handle only procedure entry/exit sequences *)
(* val procEntryExit3 : frame -> string -> string *)

(*
	FRAGMENTS

	"Given a Tiger function definition comprising a level and an already-translated 
	body expression, the Translate phase should produce a descriptor for the funciton 
	containing this necessary info:
		- frame: the frame descriptor containing machine-specific info about local 
				variables and parameters.
		- body: the result returned from procEntryExit1
	Call this pair a fragment to be translated to assembly lang. It's the second kind 
	of fragment we've seen; the other was the assembly-lang pseudo-instruction sequence 
	for a string literal. Thus, it is useful to define (in the Translate interface) a 
	frag datatype.

	The semantic analysis phase calls upon tigertrans.newLevel in processing a function 
	header. Later it calls other interface fields of tigertrans to translate the body of 
	the Tiger function; this has the side effect of remembering STRING fragments for any 
	string literals encountered (pages 163 and 262). Finally the semantic analyzer calls 
	procEntryExit, which has the side effect of remembering a PROC fragment.
	All the remembered fragments go into a frag list ref local to tigertrans; then 
	getResult can be used to extract the fragment list" - page 169

	If PROC returns a value, do: MOVE rv, res
*)
datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string

end
