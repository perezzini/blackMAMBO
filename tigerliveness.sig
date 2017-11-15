signature tigerliveness = 
sig
	type node

	(* [calculateLiveOut instrList] returns a live-out set of registers for each instruction in the list 
	along with the instruction graph correspoding to the input *)
	val calculateLiveOut : tigerassem.instr list -> tigertemp.temp Splayset.set list * node Splayset.set

	(* [liveOutInfoToString instrList] returns a list of several strings of info, corresponding to each element 
	in the input list: assembly, node number, node successors, node live-out set of registers *)
	val liveOutInfoToString : tigerassem.instr list -> (string * string * string * string) list	
end