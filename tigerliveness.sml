structure tigerliveness :> tigerliveness =
struct

	open tigerassem

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
			val tabs = List.tabulate(List.length instrList, fn x => x)

			(* instrListTabs : node list *)
			val instrListTabs : node list = ListPair.zip(instrList, tabs)		(* Instructions numbered from 0 *)

			fun compareNodes ((_, n1), (_, n2)) =
				Int.compare(n1, n2)

			val emptyGraph = Splayset.empty(compareNodes)

			(* instrGraph : node Splayset.set *)
			val instrGraph = Splayset.addList(emptyGraph, instrListTabs)		(* Instructions graph. Every elem has type 
																				node, where the integer 
																				corresponds to the number of the node in the 
																				graph. Every elem is a "node" for now on. *) 

			val emptyStringSet = Splayset.empty(String.compare)
			val (liveInArray, liveOutArray) = (Array.array (List.length instrList, emptyStringSet),
												Array.array (List.length instrList, emptyStringSet))	(* Init live-in and live-out arrays with empty temp-sets *)

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
						(OPER {src, ...}, _) => Splayset.addList(emptySet, src)
						| (MOVE {src ,...}, _) => Splayset.add(emptySet, src)
						| _ => emptySet
				end

			(* calculateDefSet : node -> tigertemp.temp Splayset.set *)
			fun calculateDefSet node =
				let
					val emptySet = Splayset.empty(String.compare)
				in
					case node of
						(OPER {dst, ...}, _) => Splayset.addList(emptySet, dst)
						| (MOVE {dst ,...}, _) => Splayset.add(emptySet, dst)
						| _ => emptySet
				end

			(* computationByIteration : node list -> bool -> unit *)
			fun computationByIteration (((node as (_, n)) :: nodes) : node list) flag =
					let
						fun f (_, n) = Array.sub (liveInArray, n)

						val (liveInArrayAux, liveOutArrayAux) = (Array.sub (liveInArray, n),
																Array.sub (liveOutArray, n))

						(* succNodes : node Splayset.set. Set of successors nodes of input node *)
						val succNodes = fromNumToNode (successors node)

						(* succNodes' : tigertemp.temp Splayset.set list *)
						val succNodes' = setMap f succNodes String.compare 


						val succNodesToList = Splayset.listItems(succNodes)
						val mappedSuccNodesToList = List.map f succNodesToList


						val _ = Array.update (liveOutArray, n, foldl Splayset.union emptyStringSet succNodes')

						(* val _ = Array.update (liveOutArray, n, foldl Splayset.union emptyStringSet succNodes') *)

						val diff = Splayset.difference(Array.sub (liveOutArray, n), calculateDefSet node)
						val union = Splayset.union(calculateUseSet node, diff)

						val _ = Array.update(liveInArray, n, union)

						val flag' = (Splayset.equal (Array.sub (liveInArray, n), liveInArrayAux))
									andalso
									(Splayset.equal (Array.sub (liveOutArray, n), liveOutArrayAux))
									andalso
									flag
					in
						computationByIteration nodes flag'
					end
				| computationByIteration [] false = computationByIteration instrListTabs true
				| computationByIteration [] true = ()

			(* Compute liveness *)
			val _ = computationByIteration instrListTabs true

		in
			List.map (fn (_, n) => 
				Array.sub(liveOutArray, n)) instrListTabs
		end
end