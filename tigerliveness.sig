signature tigerliveness = 
sig
	type node

	val getInstrFromNode : node -> tigerassem.instr

	val getNumFromNode : node -> int

	val compareNodes : node * node -> order

	(* [calculateUseSet instr] returns a set of temporaries the instruction uses *)
	val calculateUseSet : tigerassem.instr -> tigertemp.temp Splayset.set

	(* [calculateDefSet instr] returns a set of temporaries the instruction defines *)
	val calculateDefSet : tigerassem.instr -> tigertemp.temp Splayset.set

	(* [calculateLiveOut instrList] returns a live-out set of registers for each instruction in the list 
	along with the instruction graph correspoding to the input *)
	val calculateLiveOut : tigerassem.instr list -> tigertemp.temp Splayset.set list * node Splayset.set

	(* [liveOutInfoToString instrList] returns a list of several strings of info, corresponding to each element 
	in the input list: node's assembly, node's number, node's successors, and node's live-out set of registers *)
	val liveOutInfoToString : tigerassem.instr list -> (string * string * string * string) list	
end