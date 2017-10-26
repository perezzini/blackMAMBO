structure tigerassem = 
struct
	type reg = string
	type temp = tigertemp.temp
	type label = tigertemp.label

	(* Independent of the chosen target-machine assembly language *)
	datatype instr = 
		OPER of {assem: string, 			(* Assembly-lang instruction *)
				dst: temp list, 			(* A list of operands registers (may be empty) *)
				src: temp list, 			(* A list of result registers (may be empty) *)
				jump: label list option} 	(* Operations that always fall through to the next 
											instruction have jump=NONE; other operations have 
											a list of "target" labels to which they may jump *)
		(* A LABEL is a point in a program to which jumps may go *)
		| LABEL of {assem: string, 			(* How the label will look in the assembly-lang program *)
					lab: tigertemp.label} 	(* Which label symbol was used *)
		(* A MOVE is like an OPER, but must perform only data transfer. Then, if the dst and 
			src temporaries are assigned to the same register, the MOVE can later be deleted *)
		| MOVE of {assem: string,
					dst: temp,
					src: temp}

	fun format saytemp =
	    let 
			fun speak(assem,dst,src,jump) =
			    let 
			    	fun saylab s = s  
					fun f(#"`":: #"s":: i::rest) = 
					    (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
					  | f( #"`":: #"d":: i:: rest) = 
					    (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
					  | f( #"`":: #"j":: i:: rest) = 
					    (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
					  | f( #"`":: #"`":: rest) = #"`" :: f rest
					  | f( #"`":: _ :: rest) = raise Fail "Error - format(): bad assem format"
					  | f(c :: rest) = (c :: f rest)
					  | f nil = nil
			    in 
			    	implode(f(explode assem))
			    end
	      in 
	      	fn OPER{assem,dst,src,jump=NONE} => speak(assem,dst,src,nil)
	          | OPER{assem,dst,src,jump=SOME j} => speak(assem,dst,src,j)
			  | LABEL{assem,...} => assem
			  | MOVE{assem,dst,src} => speak(assem,[dst],[src],nil)
	     end
end