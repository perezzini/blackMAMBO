signature tigerregalloc = 
sig
	type allocation
	type move

	val regAlloc : (tigerassem.instr list * tigerframe.frame) -> allocation
end