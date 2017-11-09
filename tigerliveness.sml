structure tigerliveness :> tigerliveness =
struct

	open Splayset tigerassem

	type node = tigerassem.instr * int

	fun setMap f set compare =
		let
			val setToList = Splayset.listItems set
			val mappedSetToList = List.map f setToList

			val emptySet = Splayset.empty(compare)
		in
			Splayset.addList(emptySet, mappedSetToList)
		end
	
	(* calculateLiveOut : tigerassem.instr list -> tigertemp.temp list list *)
	fun calculateLiveOut instrList =
		let
			(* instrListTabs : node list *)
			val instrListTabs = List.tabulate(instrList, fn x => x)				(* Instructions numbered from 0 *)

			fun compareNodes ((_, n1), (_, n2)) =
				Int.compare(n1, n2)

			val emptyGraph = Splayset.empty(compareNodes)

			(* instrGraph : node Splayset.set *)
			val instrGraph = Splayset.addList(emptyGraph, instrListTabs)		(* Instructions graph. Every elem has type 
																				node, where the integer 
																				corresponds to the number of the node in the 
																				graph. Every elem is a "node" for now on. *) 

			(* getLabelNodeNum : tigertemp.label -> int *)
			fun getLabelNodeNum labelTemp = 
				let
					val num = case Splayset.find (fn (instr, _) => 
								case instr of
									LABEL {lab=labelTemp, ...} => true
									| _ => false) instrGraph of
								SOME (_, n) => n
								| NONE => raise Fail "Error - liveness: LABEL no encontrado en el grafo de instrucciones"
				in
					num
				end

			(* successors : node -> int Splayset.set *)
			fun successors node =
				let
					val resultSet = Splayset.empty(Int.compare)
				in
					case node of
						(OPER {jump = NONE, ...}, n) => Splayset.add(resultSet, n+1)
						| (OPER {jump = SOME [l], ...}, _) => Splayset.add(resultSet, getLabelNodeNum l)
						| (OPER {jump = SOME [t, f], ...}, _) => let
																	val resultSet' = Splayset.add(resultSet, getLabelNodeNum t)
																	val resultSet'' = Splayset.add(resultSet', getLabelNodeNum f)
																in
																	Splayset.union(resultSet', resultSet'')
																end
						| (OPER {jump = SOME _, ...}, _) => raise Fail "Error - liveness: LABEL con más de dos etiquetas"
						| (MOVE _, n) => Splayset.add(resultSet, n+1)
						| _ => raise Fail "Error - liveness: no debería pasar"
				end

			(* fromNumToNode : int Splayset.set -> node Splayset.set *)
			fun fromNumToNode intSet = 
				let
					fun toNode num = case Splayset.find (fn (_, n) => 
						if n=num then true else false) instrGraph of
						SOME node => node
						| NONE => raise Fail "Error - liveness: número de nodo no encontrado en el grafo de instrucciones"
				in
					setMap toNode intSet compareNodes
				end


			(* calculateUseSet : node -> tigertemp.temp Splayset.set *)
			fun calculateUseSet node =
				let
					val emptySet = Splayset.empty(String.compare)
				in
					case node of
						(OPER {src, ...}, _) => Splayset.addList(src, emptySet)
						| _ => emptySet
				end

			(* calculateDefSet : node -> tigertemp.temp Splayset.set *)
			fun calculateDefSet node =
				let
					val emptySet = Splayset.empty(String.compare)
				in
					case node of
						(OPER {dst, ...}, _) => Splayset.addList(dst, emptySet)
						| _ => emptySet
				end

			(* calculateInSet : node -> tigertemp.temp Splayset.set *)
			fun calculateInSet node =
				let
					val useSet = calculateUseSet node
					val defSet = calculateDefSet node
					val outSet = calculateOutSet node

					val diffSet = Splayset.difference(outSet, defSet)
				in
					Splayset.union(useSet, diffSet)
				end

			(* calculateOutSet : node -> tigertemp.temp Splayset.set *)
			fun calculateOutSet node =
				let
					(* succNodes : node Splayset.set *)
					val succNodes = fromNumToNode (successors node)
					(* appIn : tigertemp.temp Splayset.set Splayset.set *)
					val appIn = setMap calculateInSet succNodes String.compare

					val emptySet = Splayset.empty(String.compare)
				in
					(* Returns generalized union. Has type: tigertemp.temp Splayset.set *)
					Splayset.foldl union emptySet appIn
				end

		in
			(* Return type: tigertemp.temp Splayset.set Splayset.set *)
			setMap calculateOutSet instrGraph String.compare
		end
end