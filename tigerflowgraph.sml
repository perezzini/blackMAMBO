structure tigerflowgraph :> tigerflowgraph =
struct
	
	type node = tigergraph.node

	datatype flowgraph = 
		FGRAPH of {
			control: tigergraph.graph,							(* A directed graph wherein each node represents an instruction (or perhaps, a basic block) *)
			def: tigertemp.temp list tigergraph.Table,			(* A table of temporaries defined at each node (destination registers of the instruction) *)
			use: tigertemp.temp list tigergraph.Table,			(* A table of temporaries used at each node (source registers of the instruction) *)
			ismove: bool tigergraph.Table						(* Tells whether each instruction is a MOVE instruction, which could be deleted if the def and use are identical *)
		}

	fun newGraph() = FGRAPH {
			control = tigergraph.newGraph(),
			def = tigergraph.newNodeTable(),
			use = tigergraph.newNodeTable(),
			ismove = tigergraph.newNodeTable()
		}

	fun newNode(FGRAPH f, srcs, dsts, move) =
		let
			val node = tigergraph.newNode(#control f)
			val def = tigertab.tabInserta(node, srcs, #def f)
			val use = tigertab.tabInserta(node, dsts, #use f)
			val ismove = tigertab.tabInserta(node, move, #ismove f)

			val f' = FGRAPH {
				control = #control f,
				def = def,
				use = use,
				ismove = ismove
			}
		in
			(f', node)
		end

end