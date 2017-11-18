structure tigerregalloc :> tigerregalloc = 
struct

	open tigerassem

	(* type node = tigertemp.temp *)

	(* Each temp maps a machine register *)
	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

	type move = tigertemp.temp * tigertemp.temp

	type edge = tigertemp.temp * tigertemp.temp

	val emptyNodeSet : tigertemp.temp Splayset.set = Splayset.empty String.compare

	fun cmpPairs ((s1, d1), (s2, d2)) = if s1 = s2 then String.compare(d1, d2) else String.compare(s1, s2)

	val emptyMoveSet : (tigertemp.temp * tigertemp.temp) Splayset.set = Splayset.empty cmpPairs

	val machineRegsSet : tigertemp.temp Splayset.set = Splayset.addList(emptyNodeSet, tigerframe.registers)
	val machineColoredRegsSet : tigertemp.temp Splayset.set = Splayset.addList(emptyNodeSet, [tigerframe.fp, tigerframe.sp])

	val precolored : tigertemp.temp Splayset.set = Splayset.difference(machineRegsSet, machineColoredRegsSet)
	val k : int = Splayset.numItems precolored


	fun regAlloc instrList =
		let

			(* DATA STRUCTURES *)

			(* Node work-lists, sets, and stacks. The following lists and sets are always mutually disjoint 
			and every node is always in exactly one of the sets or lists *)

			(* temporary registers, not precolored and not yet processed *)
			val initial : tigertemp.temp Splayset.set ref = ref emptyNodeSet

			(* list of low-degree non-move-related nodes *)
			val simplifyWorklist : tigertemp.temp Splayset.set ref = ref emptyNodeSet

			(* low-degree move-related nodes *)
			val freezeWorklist : tigertemp.temp Splayset.set ref = ref emptyNodeSet

			(* high-degree nodes *)
			val spillWorklist : tigertemp.temp Splayset.set ref = ref emptyNodeSet

			(* nodes marked for spilling during this round; initially empty *)
			val spilledNodes : tigertemp.temp Splayset.set ref = ref emptyNodeSet

			(* registers that have been coalesced *)
			val coalescedNodes : tigertemp.temp Splayset.set ref = ref emptyNodeSet

			(* nodes successfully colored *)
			val coloredNodes : tigertemp.temp Splayset.set ref = ref emptyNodeSet

			(* stack containing temps removed from the interference graph *)
			val selectStack = ref []

			(* Stack methods *)
			fun pushStack n = (selectStack := n :: !selectStack)
	        fun popStack() = let val h = List.hd (!selectStack)
	                             val _ = (selectStack := List.tl (!selectStack))
	                         in h
	                         end
	        (* End Stack methods *)


			(* Move sets. There are 5 sets of move instructions, and every move is in exactly one of these 
				sets (after Build() through the end of Main()) *)

			(* moves that have been coalesced *)
			val coalescedMoves : move Splayset.set ref = ref emptyMoveSet

			(* moves whose source and target interfiere *)
			val constrainedMoves : move Splayset.set ref = ref emptyMoveSet

			(* moves that will no longer be considered for coalescing *)
			val frozenMoves : move Splayset.set ref = ref emptyMoveSet

			(* moves enabled for possible coalescing *)
			val worklistMoves : move Splayset.set ref = ref emptyMoveSet

			(* moves not yet ready for coalescing *)
			val activeMoves : move Splayset.set ref = ref emptyMoveSet


			(* Other data structures *)

			(* the set of interference edges (u,v) in the, undirected, interference graph *)
			val adjSet : edge Splayset.set ref = ref (Splayset.empty cmpPairs)

			(* adjacency list representation of the graph *)
			val adjList : (tigertemp.temp, tigertemp.temp Splayset.set) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

			(* an array containing the current degree of each node *)
			val degree : (tigertemp.temp, int) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

			(* a mapping from a node to the list of moves it's associated with *)
			val moveList : (tigertemp.temp, move Splayset.set) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

			val alias = ref (Splaymap.mkDict String.compare)

			val color = ref (Splaymap.mkDict String.compare)


			(* Init liveness info: instruction graph, and live-out set of tmp registers for each node in 
				the instruction graph *)
			val iGraph : tigerliveness.node Splayset.set ref = ref (Splayset.empty tigerliveness.compareNodes)
			val liveOutTable : tigertemp.temp Splayset.set list ref = ref []


			fun livenessAnalysis() =
				let
					val (liveOutTableList : tigertemp.temp Splayset.set list, 
						graph : tigerliveness.node Splayset.set) = tigerliveness.calculateLiveOut instrList

					val _ = iGraph := graph
					val _ = liveOutTable := liveOutTableList

					val graphList : tigerliveness.node list = Splayset.listItems graph
					val graphList' : tigertemp.temp Splayset.set list = List.map (fn node => 
						let
							val instr = tigerliveness.getInstrFromNode node
							val useSet = tigerliveness.calculateUseSet instr
							val defSet = tigerliveness.calculateDefSet instr
						in
							Splayset.union(useSet, defSet)
						end) graphList

					(* Gen union *)
					val allTmpsSet : tigertemp.temp Splayset.set = List.foldl (fn (tmpSet, s) => 
						Splayset.union(tmpSet, s)) (Splayset.empty String.compare) graphList'
				in
					(* Update "initial" set *)
					initial := Splayset.difference(allTmpsSet, precolored)	
				end

			(* isMoveInstruction : tigerassem.instr -> bool *)	
			fun isMoveInstruction (MOVE _) = true
				| isMoveInstruction _ = false

			(* getDstSrcFromMoveInstruction : tigerassem.instr -> {src : tigertemp.temp, dst : tigertemp.temp} *)
			fun getDstSrcFromMoveInstruction (MOVE {dst, src, ...}) = {src=src, dst=dst}
				| getDstSrcFromMoveInstruction _ = raise Fail "Error - regAlloc. Tratando de obtener src y dst de una instrucción que no es MOVE"


			(* addEdge : (tigertemp.temp * tigertemp.temp) -> unit *)
			fun addEdge(u, v) =
				if not(Splayset.member(!adjSet, (u, v))) andalso not(u = v)
				then 
					(adjSet := Splayset.add(Splayset.add(!adjSet, (u, v)), (v, u));
					if not(Splayset.member(precolored, u)) 
					then 
						(let
							val peeked = case Splaymap.peek(!adjList, u) of
								SOME set => set
								| NONE => raise Fail "Error - regAlloc. addEdge() peek error"

							val singleton = Splayset.singleton String.compare u
						in
							adjList := Splaymap.insert(!adjList, u, Splayset.union(peeked, singleton))
						end;
						let
							val peeked = case Splaymap.peek(!degree, u) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. addEdge() peek error"
						in
							degree := Splaymap.insert(!degree, u, peeked + 1)
						end) 
					else 
						();
					if not(Splayset.member(precolored, v))  
					then 
						(let
							val peeked = case Splaymap.peek(!adjList, u) of
								SOME set => set
								| NONE => raise Fail "Error - regAlloc. addEdge() peek error"

							val singleton = Splayset.singleton String.compare u
						in
							adjList := Splaymap.insert(!adjList, u, Splayset.union(peeked, singleton))
						end;
						let
							val peeked = case Splaymap.peek(!degree, u) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. addEdge() peek error"
						in
							degree := Splaymap.insert(!degree, u, peeked + 1)
						end)  
					else 
						())
				else 
					()


			fun build() =
				let
					val forall = Splayset.app (fn node => 
						let
							val instr = tigerliveness.getInstrFromNode node
							val nodeNum = tigerliveness.getNumFromNode node
						in
							if isMoveInstruction instr 
							then 
								let
									val moveInstr = getDstSrcFromMoveInstruction instr
									val singletonMoveInstr = Splayset.singleton cmpPairs (#src moveInstr, #dst moveInstr)

									val useSet = tigerliveness.calculateUseSet instr
									val defSet = tigerliveness.calculateDefSet instr
									val union = Splayset.union(useSet, defSet)
								in
									(Splayset.app (fn n => 
										let
											val peekedMove = case Splaymap.peek(!moveList, n) of
												SOME s => s
												| NONE => raise Fail "Error - regAlloc. build() peek error"
										in
											moveList := Splaymap.insert(!moveList, n, Splayset.union(peekedMove, singletonMoveInstr))
										end) union;
									worklistMoves := Splayset.union(!worklistMoves, singletonMoveInstr))
								end
							else
								let
									val defSet = tigerliveness.calculateDefSet instr
								in
									Splayset.app (fn d => 
										let
											(* Search for instr index number from the iGraph *)
											val instrLiveOutSet : tigertemp.temp Splayset.set = List.nth(!liveOutTable, nodeNum)
											
										in
											Splayset.app (fn l => 
												addEdge(l, d)) instrLiveOutSet
										end) defSet
								end
							end) (!iGraph)
				in
					()
				end

			(* nodeMoves : tigertemp.temp -> move Splayset.set *)
			fun nodeMoves n =
				let
					val union = Splayset.union(!activeMoves, !worklistMoves)
					val peeked = case Splaymap.peek(!moveList, n) of
						SOME moveSet => moveSet
						| NONE => raise Fail "Error - regAlloc. nodeMoves() peek error"
				in
					Splayset.intersection(peeked, union)
				end

			(* moveRelated : tigertemp.temp -> bool *)
			fun moveRelated n = 
				not(Splayset.isEmpty (nodeMoves n))

			(* makeWorklist : tigertemp.temp Splayset.set -> unit *)
			fun makeWorklist() =
				Splayset.app (fn n => 
					let
						val degree = case Splaymap.peek(!degree, n) of
							SOME i => i
							| NONE => raise Fail "Error - regAlloc. makeWorklist() peek error"
					in
						if degree >= k 
						then 
							spillWorklist := Splayset.add(!spillWorklist, n) 
						else 
							if moveRelated n 
							then 
								freezeWorklist := Splayset.add(!freezeWorklist, n) 
							else 
								simplifyWorklist := Splayset.add(!simplifyWorklist, n)
					end) (!initial)

			(* adjacent : tigertemp.temp -> tigertemp.temp Splayset.set *)
			fun adjacent n =
				let
					val peeked = case Splaymap.peek(!adjList, n) of
						SOME tmpSet => tmpSet
						| NONE => raise Fail "Error - regAlloc. adjacent() peek error"

					val union = Splayset.union(Splayset.addList(Splayset.empty String.compare, !selectStack), !coalescedNodes)
				in
					Splayset.difference(peeked, union)
				end

			(* enableMoves : tigertemp.temp Splayset.set -> unit *)
			fun enableMoves nodes =
				Splayset.app (fn n => 
					Splayset.app (fn m => 
						if Splayset.member(!activeMoves, m) 
						then 
							(activeMoves := Splayset.difference(!activeMoves, Splayset.singleton cmpPairs m);
							worklistMoves := Splayset.union(!worklistMoves, Splayset.singleton cmpPairs m)) 
						else 
							()) (nodeMoves n)) nodes

			(* decrementDegree : tigertemp.temp -> unit *)
			fun decrementDegree n =
				let
					val singleton = Splayset.singleton String.compare n

					val d = case Splaymap.peek(!degree, n) of
						SOME i => i
						| NONE => raise Fail "Error - regAlloc. decrementDegree() peek error"

					val _ = Splaymap.insert(!degree, n, d - 1)
				in
					if d = k 
					then 
						(enableMoves (Splayset.union(singleton, adjacent n));
						spillWorklist := Splayset.difference(!spillWorklist, singleton);
						if moveRelated n 
						then 
							freezeWorklist := Splayset.union(!freezeWorklist, singleton) 
						else 
							simplifyWorklist := Splayset.union(!simplifyWorklist, singleton)) 
					else 
						()
				end


			(* simplify : unit -> unit *)
			fun simplify() =
				case Splayset.find (fn _ => 
					true) (!simplifyWorklist) of
					SOME tmp => (simplifyWorklist := Splayset.difference(!simplifyWorklist, Splayset.singleton String.compare tmp);
								pushStack tmp;
								Splayset.app decrementDegree (adjacent tmp))
					| NONE => ()

			(* ok : (tigertemp.temp * tigertemp.temp) -> bool *)
			fun ok(t,r) =
				let
					val degree = case Splaymap.peek(!degree, t) of
						SOME i => i
						| NONE => raise Fail "Error - regAlloc. ok() peek error"
				in
					degree < k orelse Splayset.member(precolored, t) orelse Splayset.member(!adjSet, (t, r))
				end

			fun condCoalesce(u, v) = 
				Splayset.foldl (fn (t, b) => b andalso ok(t, u)) true (adjacent v)

			(* addWorkList : tigertemp.temp -> unit *)
			fun addWorkList u = 
				let
					val degree = case Splaymap.peek(!degree, u) of
						SOME i => i
						| NONE => raise Fail "Error - regAlloc. addWorkList() peek error"
				in
					if not(Splayset.member(precolored, u)) andalso not(moveRelated u) andalso degree < k  
					then 
						let
							val singleton = Splayset.singleton String.compare u
						in
							(freezeWorklist := Splayset.difference(!freezeWorklist, singleton);
							simplifyWorklist := Splayset.union(!simplifyWorklist, singleton))
						end
					else 
						()
				end

			(* conservative : tigertemp.temp Splayset.set -> bool *)
			fun conservative nodes =
				let
					val k' = ref 0
					val _ = Splayset.app (fn n => 
						let
							val degree = case Splaymap.peek(!degree, n) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. conservative() peek error"
						in
							if degree >= k 
							then 
								k' := !k' + 1 
							else 
								()
						end) nodes
				in
					!k' < k
				end

			(* getAlias : tigertemp.temp -> tigertemp.temp *)
			fun getAlias n =
				if Splayset.member(!coalescedNodes, n) 
				then 
					let
					 	val peek = case Splaymap.peek(!alias, n) of
					 		SOME a => a
					 		| NONE => raise Fail "Error - regAlloc. getAlias() peek error"
					 in
					 	getAlias peek
					 end 
				else 
					n

			(* combine : (tigertemp.temp * tigertemp.temp) -> unit *)
			fun combine(u, v) =
				(if Splayset.member(!freezeWorklist, v) 
					then 
						freezeWorklist := Splayset.difference(!freezeWorklist, Splayset.singleton String.compare v) 
					else 
						spillWorklist := Splayset.difference(!spillWorklist, Splayset.singleton String.compare v);
				coalescedNodes := Splayset.union(!coalescedNodes, Splayset.singleton String.compare v);
				Splaymap.insert(!alias, v, u);
				let
					val moveListU = case Splaymap.peek(!moveList, u) of
						SOME pair => pair
						| NONE => raise Fail "Error - regAlloc. combine() peek error"

					val moveListV = case Splaymap.peek(!moveList, v) of
						SOME pair => pair
						| NONE => raise Fail "Error - regAlloc. combine() peek error"
				in
					Splaymap.insert(!moveList, u, Splayset.union(moveListU, moveListV))
				end;
				enableMoves(Splayset.singleton String.compare v);
				Splayset.app (fn t => 
					(addEdge(t, u); decrementDegree t)) (adjacent v);
				let
					val degree = case Splaymap.peek(!degree, u) of
						SOME i => i
						| NONE => raise Fail "Error - regAlloc. combine() peek error"
				in
					if degree >= k andalso Splayset.member(!freezeWorklist, u) 
					then 
						(freezeWorklist := Splayset.difference(!freezeWorklist, Splayset.singleton String.compare u);
						spillWorklist := Splayset.union(!spillWorklist, Splayset.singleton String.compare u)) 
					else 
						()
				end)

			(* coalesce : unit -> unit *)
			fun coalesce() = 
				case Splayset.find (fn _ => true) (!worklistMoves) of
					NONE => ()
					| SOME (m as (x', y')) => let
												val x = getAlias x'
												val y = getAlias y'

												val (u, v) = if Splayset.member(precolored, y) 
												then 
													(y, x) 
												else 
													(x, y)

												val singleton = Splayset.singleton cmpPairs m

												val _ = worklistMoves := Splayset.difference(!worklistMoves, singleton)
											in
												if u = v 
												then 
													(coalescedMoves := Splayset.union(!coalescedMoves, singleton);
													addWorkList u) 
												else 
													if Splayset.member(precolored, v) orelse Splayset.member(!adjSet, (u, v)) 
													then 
														(constrainedMoves := Splayset.union(!constrainedMoves, singleton);
														addWorkList u;
														addWorkList v) 
													else 
														if (Splayset.member(precolored, u) andalso condCoalesce(u, v))
															orelse not(Splayset.member(precolored, u)
																andalso conservative(Splayset.union(adjacent u, adjacent v))) 
														then 
															(coalescedMoves := Splayset.union(!coalescedMoves, singleton);
															combine(u, v);
															addWorkList u) 
														else 
															activeMoves := Splayset.union(!activeMoves, singleton)
											end

			(* freezeMoves : tigertemp.temp -> unit *)
			fun freezeMoves u =
				Splayset.app (fn (m as (x, y)) => 
					let
						val v = if (getAlias y) = (getAlias u) 
						then 
							getAlias x 
						else 
							getAlias y
					in
						(activeMoves := Splayset.difference(!activeMoves, Splayset.singleton cmpPairs m);
						frozenMoves := Splayset.union(!frozenMoves, Splayset.singleton cmpPairs m);
						let
							val degree = case Splaymap.peek(!degree, v) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. freezeMoves() peek error"
						in
							if Splayset.isEmpty(nodeMoves v) andalso degree < k 
							then 
								(freezeWorklist := Splayset.difference(!freezeWorklist, Splayset.singleton String.compare v);
								simplifyWorklist := Splayset.union(!simplifyWorklist, Splayset.singleton String.compare v)) 
							else 
								()
						end)
					end) (nodeMoves u)


			(* freeze : unit -> unit *)
			fun freeze() =
				case Splayset.find (fn _ => true) (!freezeWorklist) of
					NONE => ()
					| SOME u => let
						val singleton = Splayset.singleton String.compare u
					in
						(freezeWorklist := Splayset.difference(!freezeWorklist, singleton);
						simplifyWorklist := Splayset.union(!simplifyWorklist, singleton);
						freezeMoves u)
					end

			(* selectSpill : unit -> unit *)
			fun selectSpill() = 
				let
					(* Select a temporary from spillWorkList using the following heuristic *)

					fun chooseMinIndex (tmp, currentMinIndex) =
						let
							val tmpIndex = case Int.fromString (String.extract(tmp, 1, NONE)) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. selectSpill(): imposible sustraer índice de temporario"
						in
							if tmpIndex < currentMinIndex 
							then 
								tmpIndex 
							else 
								currentMinIndex
						end

					val minIndex = Splayset.foldl chooseMinIndex (tigertemp.lastTempIndex()) (!spillWorklist)
					val m = "T"^Int.toString minIndex

					val singleton = Splayset.singleton String.compare m
				in
					(spillWorklist := Splayset.difference(!spillWorklist, singleton);
					simplifyWorklist := Splayset.union(!simplifyWorklist, singleton);
					freezeMoves m)
				end

			(* repeat : unit -> unit *)
			fun repeat() =
	            let val _ = if not(Splayset.isEmpty (!simplifyWorklist)) then simplify()
	                        else if not(Splayset.isEmpty (!worklistMoves)) then coalesce()
	                        else if not(Splayset.isEmpty (!freezeWorklist)) then freeze()
	                        else if not(Splayset.isEmpty (!spillWorklist)) then selectSpill()
	                        else ()
	                val _ = if Splayset.isEmpty (!simplifyWorklist)
	                            andalso Splayset.isEmpty (!worklistMoves)
	                            andalso Splayset.isEmpty (!freezeWorklist)
	                            andalso Splayset.isEmpty (!spillWorklist)
	                        then ()
	                        else repeat()
	            in 
	            	()
	            end


		in
			()
		end

end