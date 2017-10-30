signature tigerflowgraph = 
sig
	type flowgraph

	val newGraph : unit -> flowgraph																				(* Returns new, empty, flow graph *)

	val newNode : flowgraph -> tigertemp.temp list -> tigertemp.temp list -> bool -> tigergraph.node * flowgraph	(* Creates new node in flow graph taking information about src, dst, moves from an instruction *)
end