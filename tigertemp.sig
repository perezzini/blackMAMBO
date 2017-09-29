signature tigertemp = sig

	(* TEMPORARIES & LABELS: 
		Sometimes is too early to determine exactly which registers 
		are available, or exactly where a procedure body will be 
		located. So, we use the word "temporary" to mean a value 
		that is temporarily held in a register, and the word 
		"label" to mean some machine-language location whose exact 
		address is yet to be determined - just like a label in 
		assembly language. *)

	type label = string (* Abstract names for static memory addresses *)
	type temp = string (* Abstract names for local variables *)
	
	val makeString: string -> string
	val newtemp: unit -> temp (* Returns a new temporary from an infinite set of temps *)
	val newlabel: unit -> label (* Returns a new label from an infinite set of labels *)

end
