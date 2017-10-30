structure tigerflowgraph :> tigerflowgraph 
struct
	
	type flowgraph = {
		control: tigergraph.graph,							(* A directed graph wherein each node represents an instruction (or perhaps, a basic block) *)
		def: tigertemp.temp list tigergraph.tableNodes,		(* A table of temporaries defined at each node (destination registers of the instruction) *)
		use: tigertemp.temp list tigergraph.tableNodes,		(* A table of temporaries used at each node (source registers of the instruction) *)
		ismove: bool tigergraph.tableNodes					(* Tells whether each instruction is a MOVE instruction, which could be deleted if the def and use are identical *)
	}

	fun newGraph() = {
			control: tigergraph.newGraph(),
			def: tigertab.crearTabla (tigergraph.eq),
			use: tigertab.crearTabla (tigergraph.eq),
			ismove tigertab.crearTabla (tigergraph.eq)
		}

	fun newNode flowgraph srcs dests move =
		let
			val node = tigergraph.newNode (#control flowgraph)
			
		in
			body
		end

end