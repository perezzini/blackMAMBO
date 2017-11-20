signature tigerregalloc = 
sig
	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict
	type move

	val regAlloc : (tigerassem.instr list * tigerframe.frame) -> allocation

	val allocPairToString : (tigertemp.temp * tigerframe.register) -> string
end