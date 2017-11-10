signature tigerliveness = 
sig
	val calculateLiveOut : tigerassem.instr list -> tigertemp.temp Splayset.set list
	val liveOutInfoToString : tigerassem.instr list -> (string * string) list 				(* Returns a list of pairs where the 1st component is an instr an 
																							assembly-lang (in string format), and the 2nd component is the set 
																							of live-out register (in string format) corresponding to the instr *)
end