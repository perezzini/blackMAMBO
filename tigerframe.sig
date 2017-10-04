signature tigerframe =
sig

type frame (* Holds info about formal parameters and local variables allocated in this frame *)
type register = string
val rv : tigertemp.temp
val ov : tigertemp.temp
val fp : tigertemp.temp
datatype access = InFrame of int 
				| InReg of tigertemp.label (* Describes formals and locals that may be in the frame 
											or in registers. InFrame(X) indicates a memory location 
											at offset X from the frame pointer. InReg(t84) indicates 
											that it will be held in "register" t84 *)
val fpPrev : int
val fpPrevLev : int
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
val sp : tigertemp.temp
val maxRegFrame : frame -> int
val wSz : int
val log2WSz : int
val calldefs : tigertemp.temp list
val callersaves : tigertemp.temp list
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
val procEntryExit1 : frame * tigertree.stm -> tigertree.stm
(*val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list*)
datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string

end
