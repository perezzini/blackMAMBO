structure tigerliveness :> tigerliveness =
struct

	open tigerassem

	type node = tigerassem.instr * int

	(* compareNodes : node * node -> order *)
	fun compareNodes ((_, n1), (_, n2)) = Int.compare(n1, n2)

	(* getLabelNodeNum : tigertemp.label -> node Splayset.set -> int
	Where's the error? Couldn't find it...
	fun getLabelNodeNum (labelTemp : string) (iGraph : node Splayset.set) = 
		let
			val num = case Splayset.find (fn (instr, _) => 
						case instr of
							LABEL {lab = labelTemp, ...} => true
							| _ => false) iGraph of
						SOME (_, n) => n
						| NONE => raise Fail "Error - liveness: LABEL no encontrado en el grafo de instrucciones"
		in
			num
		end
	*)

	fun getInstrFromNode (instr, _) = instr

	fun getNumFromNode (_, n) = n

	(* createLabelsTableFromGraph : node Splayset.set -> (string * int) Splaymap.dict *)
	fun createLabelsTableFromGraph iGraph =
		let
			val nodesList = Splayset.listItems iGraph

			val labelNodes = List.filter (fn (instr, _) => 
				case instr of
					LABEL _ => true
					| _ => false) nodesList

			val labelNodes' : (string * int) list = List.map (fn (instr, n) => 
				case instr of
					LABEL {lab, ...} => (lab, n)
					| _ => raise Fail "Error - liveness (createLabelsTableFromGraph()): no debería pasar") labelNodes

			val table = Splaymap.mkDict String.compare
		in
			List.foldl (fn ((lab, n), table) => 
				Splaymap.insert(table, lab, n)) table labelNodes'		(* Supposing "lab's" are unique *)
		end

	(* getLabelNodeNum : tigertemp.label -> (string * int) Splaymap.dict -> int *)
	fun getLabelNodeNum (labelTemp : string) labelsTable = 
		let
			val num = case Splaymap.peek(labelsTable, labelTemp) of
				SOME n => n
				| NONE => raise Fail "Error - liveness: LABEL no encontrado en el grafo de instrucciones"
		in
			num
		end

	(* successors : node -> node Splayset.set -> int Splayset.set *)
	fun successors node iGraph =
		let
			val resultSet = Splayset.empty(Int.compare)
			val labelsTable = createLabelsTableFromGraph iGraph 		(* Hash table only containing labels "instructions" as keys, 
																		mapping number of node *)
		in
			case node of
				(OPER {jump = NONE, ...}, n) => Splayset.add(resultSet, n+1)
				| (OPER {jump = SOME [], ...}, _) => raise Fail "Error - liveness: jump vacío"
				| (OPER {jump = SOME [l], ...}, _) => Splayset.add(resultSet, getLabelNodeNum l labelsTable)
				| (OPER {jump = SOME [t, f], ...}, _) => let
															val resultSet' = Splayset.add(resultSet, getLabelNodeNum t labelsTable)
															val resultSet'' = Splayset.add(resultSet', getLabelNodeNum f labelsTable)
														in
															Splayset.union(resultSet', resultSet'')
														end
				| (OPER {jump = SOME _, ...}, _) => raise Fail "Error - liveness: LABEL con más de dos etiquetas"
				| (MOVE _, n) => Splayset.add(resultSet, n+1)
				| (LABEL {lab, ...}, n) => case String.isPrefix "EXIT_LAB_" lab of
					true => resultSet
					| false => Splayset.add(resultSet, n+1)
		end

	(* fromNumToNode : int Splayset.set -> node Splayset.set -> node Splayset.set *)
	fun fromNumToNode intSet iGraph = 
		let
			fun toNode num = case Splayset.find (fn (_, n) => 
				if n=num then true else false) iGraph of
				SOME node => node
				| NONE => raise Fail "Error - liveness: número de nodo no encontrado en el grafo de instrucciones"
		in
			utils.setMap toNode intSet compareNodes
		end

	(* calculateUseSet : tigerassem.instr -> tigertemp.temp Splayset.set *)
	fun calculateUseSet instr =
		let
			val emptySet = Splayset.empty(String.compare)
		in
			case instr of
				OPER {src, ...} => Splayset.addList(emptySet, src)
				| MOVE {src ,...} => Splayset.add(emptySet, src)
				| _ => emptySet
		end

	(* calculateDefSet : tigerassem.instr -> tigertemp.temp Splayset.set *)
	fun calculateDefSet instr =
		let
			val emptySet = Splayset.empty(String.compare)
		in
			case instr of
				OPER {dst, ...} => Splayset.addList(emptySet, dst)
				| MOVE {dst ,...} => Splayset.add(emptySet, dst)
				| _ => emptySet
		end

	(* calculateLiveOut : tigerassem.instr list -> tigertemp.temp Splayset.set list *)
	fun calculateLiveOut instrList =
		let
			val tabs = List.tabulate(List.length instrList, fn x => x)

			(* nodesList : node list *)
			val nodesList : node list = ListPair.zip(instrList, tabs)			(* Instructions numbered from 0 *)

			val emptyGraph = Splayset.empty(compareNodes)

			(* iGraph : node Splayset.set *)
			val iGraph = Splayset.addList(emptyGraph, nodesList)				(* Instructions graph. Every elem has type 
																				node, where the integer 
																				corresponds to the number of the node in the 
																				graph. Every elem is a "node" for now on. *)

			val emptyStringSet = Splayset.empty(String.compare)
			val (liveInArray, liveOutArray) = (Array.array (List.length instrList, emptyStringSet),
												Array.array (List.length instrList, emptyStringSet))	(* Init live-in and live-out arrays with empty temp-sets *)


			(* computationByIteration : node list -> bool -> unit *)
			fun computationByIteration [] false = computationByIteration nodesList true
				| computationByIteration [] true = ()
				| computationByIteration (((node as (instr, n)) :: nodes) : node list) flag =
					let
						(* insertSetFromArray : node -> tigertemp.temp Splayset.set *)
						fun insertLiveInSetFromArray (_, n) = Array.sub (liveInArray, n)

						val emptySet = Splayset.empty(String.compare)

						val (liveInArrayAux, liveOutArrayAux) = (Array.sub (liveInArray, n),
																Array.sub (liveOutArray, n))

						(* succNodes : node Splayset.set. Set of successors nodes of input node *)
						val succNodes : node Splayset.set = fromNumToNode (successors node iGraph) iGraph

						val succNodesToList : node list = Splayset.listItems(succNodes)

						(* succNodes' : tigertemp.temp Splayset.set list *)
						val succNodes' : tigertemp.temp Splayset.set list  = List.map insertLiveInSetFromArray succNodesToList


						(* Begin iteration... 
						Update first out[n] and then in[n] *)

						val genUnionSet : tigertemp.temp Splayset.set = List.foldl Splayset.union emptySet succNodes'
						val _ = Array.update (liveOutArray, n, genUnionSet)

						val diff = Splayset.difference(Array.sub (liveOutArray, n), calculateDefSet instr)
						val union = Splayset.union(calculateUseSet instr, diff)
						val _ = Array.update(liveInArray, n, union)

						val flag' = (Splayset.equal (Array.sub (liveInArray, n), liveInArrayAux))
									andalso
									(Splayset.equal (Array.sub (liveOutArray, n), liveOutArrayAux))
					in
						computationByIteration nodes (flag andalso flag')
					end

			(* Compute liveness *)
			val _ = computationByIteration nodesList true
		in
			(List.map (fn (_, n) => 
				Array.sub(liveOutArray, n)) nodesList,
			iGraph)
		end

	fun liveOutInfoToString instrList =
		let
			(* liveOutList : tigertemp.temp Splatset.set list
			iGraph : node Splayset.set
			*)
			val (liveOutList, iGraph) = calculateLiveOut instrList

			val iGraphList : node list = Splayset.listItems iGraph

			val iGraphList' = List.map (fn (node as (instr, n)) => 
				let
					val instrStr : string = tigerassem.format tigertemp.makeString instr
					val nStr : string = Int.toString n
					val succSet : int Splayset.set = successors node iGraph
					val succSetStr : string = utils.setToString succSet Int.toString
					val liveOutSet : tigertemp.temp Splayset.set = List.nth(liveOutList, n)
					val liveOutStr : string = utils.setToString liveOutSet (fn x => x)

					(* Just deletes last \n char *)
					val instrStr' : string = utils.deleteEnterFromString instrStr
				in
					(instrStr',
					nStr,
					succSetStr,
					liveOutStr)
				end) iGraphList
		in
			iGraphList'
		end
end