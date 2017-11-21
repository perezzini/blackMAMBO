structure tigerregalloc :> tigerregalloc = 
struct

	(* Each temp maps a machine register *)
	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

	(* move: (a, b), where:
		- a: source
		- b: destination
	*)
	type move = tigertemp.temp * tigertemp.temp

	type edge = tigertemp.temp * tigertemp.temp

	val emptyNodeSet : tigertemp.temp Splayset.set = Splayset.empty String.compare

	(* cmpPairs : (tigertemp.temp * tigertemp.temp) * (tigertemp.temp * tigertemp.temp) -> order *)
	fun cmpPairs ((s1, d1), (s2, d2)) = 
		if s1 = s2 then String.compare(d1, d2) else String.compare(s1, s2)

	(* allocPairToString : (tigertemp.temp * tigerframe.register) -> string *)
	fun allocPairToString pair = 
		utils.pairToString pair utils.id utils.id

	val emptyMoveSet : (tigertemp.temp * tigertemp.temp) Splayset.set = Splayset.empty cmpPairs

	(* All general-purpose target-machine registers *)
	val precolored : tigertemp.temp Splayset.set = Splayset.addList(emptyNodeSet, tigerframe.registers)

	val specialRegsSet : tigertemp.temp Splayset.set = Splayset.addList(emptyNodeSet, tigerframe.specialregs)

	(* Number of "colors": available target-machine registers to identify temporaries *)
	val kColors : tigertemp.temp Splayset.set = Splayset.difference(precolored, specialRegsSet)
	val k : int = Splayset.numItems kColors

	(* precolored = {%r10,%r11,%r12,%r13,%r14,%r15,%r8,%r9,%rax,%rbp,%rbx,%rcx,%rdi,%rdx,%rsi,%rsp} 
	All machine registers. In total = 16 registers *)


	fun regAlloc (instrList, frame) =
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
	        fun popStack() = 
        		let 
					val h = List.hd (!selectStack)
	             	val _ = (selectStack := List.tl (!selectStack))
	            in
	            	h
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

			(* adjacency list representation of the interference graph. For each non-precolored tmp u, 
			adjList[u] is the set of nodes that interfere with u *)
			val adjList : (tigertemp.temp, tigertemp.temp Splayset.set) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

			(* an array containing the current degree of each node *)
			val degree : (tigertemp.temp, int) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

			(* a mapping from a node to the list of moves it's associated with *)
			val moveList : (tigertemp.temp, move Splayset.set) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

			(* when a move (u, v) has been coalesced, and v put in coalescedNodes, then 
				alias(v) = u *)
			val alias : (tigertemp.temp, tigertemp.temp) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

			(* the color chosen by the algorithm for a node. For precolored nodes this is initialized to the 
			given color *)
			val color : (tigertemp.temp, tigerframe.register) Splaymap.dict ref = ref (List.foldl (fn (reg, dict) => 
				Splaymap.insert(dict, reg, reg)) (Splaymap.mkDict String.compare) tigerframe.registers)


			(* Init liveness info: instruction graph, and live-out set of tmp registers for each node in 
				the instruction graph *)
			val iGraph : tigerliveness.node Splayset.set ref = ref (Splayset.empty tigerliveness.compareNodes)
			val liveOutTable : tigertemp.temp Splayset.set list ref = ref []

			
			(* Prints current iGraph and liveOutTable information *)
			(* printIGraphAndLiveOutInfo : unit -> unit *)
			fun printIGraphAndLiveOutInfo() =
				let
					val iGraphList = Splayset.listItems (!iGraph)

					val _ = print("\niGraph = \n"^(utils.listToString (iGraphList) (fn node => 
						let
							val instr = tigerliveness.getInstrFromNode node
							val num = tigerliveness.getNumFromNode node
							val pair = (instr, num)
						in
							utils.pairToString pair (tigerassem.format tigertemp.makeString) Int.toString
						end))^"\n")

					val _ = print("\nliveOutTable = \n"^(utils.listToString (!liveOutTable) (fn tmpSet => 
						utils.setToString tmpSet utils.id))^"\n")
				in
					()
				end

			(* livenessAnalysis : unit -> unit *)
			fun livenessAnalysis() =
				let
					val (liveOutTableList : tigertemp.temp Splayset.set list, 
						graph : tigerliveness.node Splayset.set) = tigerliveness.calculateLiveOut instrList

					(* Update iGraph and live-out info *)
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
					(initial := Splayset.difference(allTmpsSet, precolored);
					adjList := Splayset.foldl (fn (tmp, dict) => 
						Splaymap.insert(dict, tmp, emptyNodeSet)) (!adjList) allTmpsSet;
					degree := Splayset.foldl (fn (tmp, dict) => 
						Splaymap.insert(dict, tmp, 0)) (!degree) allTmpsSet;
					moveList := Splayset.foldl (fn (tmp, dict) => 
						Splaymap.insert(dict, tmp, emptyMoveSet)) (!moveList) allTmpsSet)	
				end

			(* addEdge : (tigertemp.temp * tigertemp.temp) -> unit *)
			fun addEdge(u, v) =
				if not(Splayset.member(!adjSet, (u, v))) andalso not(u = v)
				then 
					(adjSet := Splayset.add(Splayset.add(!adjSet, (u, v)), (v, u));
					if not(Splayset.member(precolored, u)) 
					then 
						(let
							val adjList_u = case Splaymap.peek(!adjList, u) of
								SOME set => set
								| NONE => raise Fail "Error - regAlloc. addEdge() peek error"

							val singleton = Splayset.singleton String.compare v
						in
							adjList := Splaymap.insert(!adjList, u, Splayset.union(adjList_u, singleton))
						end;
						let
							val degree_u = case Splaymap.peek(!degree, u) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. addEdge() peek error"
						in
							degree := Splaymap.insert(!degree, u, degree_u + 1)
						end) 
					else 
						();
					if not(Splayset.member(precolored, v))  
					then 
						(let
							val adjList_v = case Splaymap.peek(!adjList, v) of
								SOME set => set
								| NONE => raise Fail "Error - regAlloc. addEdge() peek error 1"

							val singleton = Splayset.singleton String.compare u
						in
							adjList := Splaymap.insert(!adjList, v, Splayset.union(adjList_v, singleton))
						end;
						let
							val degree_v = case Splaymap.peek(!degree, v) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. addEdge() peek error 2"
						in
							degree := Splaymap.insert(!degree, v, degree_v + 1)
						end)  
					else 
						())
				else 
					()

			(* build : unit -> unit *)
			fun build() =
				Splayset.app (fn node => 
					let
						val instr = tigerliveness.getInstrFromNode node
						val instrNum = tigerliveness.getNumFromNode node

						(* val _ = print("\nbuild(): instrNum = "^Int.toString instrNum^"\n") *)
					in
						(if tigerassem.isMoveInstruction instr 
						then 
							(let
								val moveInstr = tigerassem.getSrcDstFromMoveInstruction instr
								val moveInstrNode = (#src moveInstr, #dst moveInstr)

								val defSet = tigerliveness.calculateDefSet instr
								val useSet = tigerliveness.calculateUseSet instr
								val defUseUnion = Splayset.union(defSet, useSet)
							in
								Splayset.app (fn n => 
									let
										val moveListN = case Splaymap.peek(!moveList, n) of
											SOME set => set
											| NONE => raise Fail "Error - regAlloc. build() peek error"

										val singletonInstr = Splayset.singleton cmpPairs moveInstrNode
									in
										moveList := Splaymap.insert(!moveList, n, Splayset.union(moveListN, singletonInstr))
									end) defUseUnion
							end;
							let
								val moveInstr = tigerassem.getSrcDstFromMoveInstruction instr
								val moveInstrNode = (#src moveInstr, #dst moveInstr)

								val singletonInstr = Splayset.singleton cmpPairs moveInstrNode
							in
								worklistMoves := Splayset.union(!worklistMoves, singletonInstr)
							end)
						else 
							();
						Splayset.app (fn d => 
							let
								val liveOutSetInstr = List.nth(!liveOutTable, instrNum)
							in
								Splayset.app (fn l => 
									addEdge(l, d)) liveOutSetInstr
							end) (tigerliveness.calculateDefSet instr))
					end) (!iGraph)

			(* nodeMoves : tigertemp.temp -> move Splayset.set *)
			fun nodeMoves n =
				let
					val union = Splayset.union(!activeMoves, !worklistMoves)
					val moveList_n = case Splaymap.peek(!moveList, n) of
						SOME moveSet => moveSet
						| NONE => raise Fail "Error - regAlloc. nodeMoves() peek error"
				in
					Splayset.intersection(moveList_n, union)
				end

			(* moveRelated : tigertemp.temp -> bool *)
			fun moveRelated n = 
				not(Splayset.isEmpty (nodeMoves n))

			(* makeWorklist : tigertemp.temp Splayset.set -> unit *)
			fun makeWorklist() =
				Splayset.app (fn n => 
					(initial := Splayset.difference(!initial, Splayset.singleton String.compare n);
					let
						val degree_n = case Splaymap.peek(!degree, n) of
							SOME i => i
							| NONE => raise Fail "Error - regAlloc. makeWorklist() peek error"
					in
						if degree_n >= k 
						then 
							spillWorklist := Splayset.add(!spillWorklist, n) 
						else 
							if moveRelated n 
							then 
								freezeWorklist := Splayset.add(!freezeWorklist, n) 
							else 
								simplifyWorklist := Splayset.add(!simplifyWorklist, n)
					end)) (!initial)

			(* adjacent : tigertemp.temp -> tigertemp.temp Splayset.set *)
			fun adjacent n =
				let
					val adjList_n = case Splaymap.peek(!adjList, n) of
						SOME tmpSet => tmpSet
						| NONE => raise Fail "Error - regAlloc. adjacent() peek error"

					val union = Splayset.union(Splayset.addList(Splayset.empty String.compare, !selectStack), !coalescedNodes)
				in
					Splayset.difference(adjList_n, union)
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
			fun decrementDegree m =
				let
					val singleton_m = Splayset.singleton String.compare m

					val d = case Splaymap.peek(!degree, m) of
						SOME i => i
						| NONE => raise Fail "Error - regAlloc. decrementDegree() peek error"

					val _ = degree := Splaymap.insert(!degree, m, d - 1)
				in
					if d = k 
					then 
						(enableMoves (Splayset.union(singleton_m, adjacent m));
						spillWorklist := Splayset.difference(!spillWorklist, singleton_m);
						if moveRelated m
						then 
							freezeWorklist := Splayset.union(!freezeWorklist, singleton_m) 
						else 
							simplifyWorklist := Splayset.union(!simplifyWorklist, singleton_m)) 
					else 
						()
				end

			(* simplify : unit -> unit *)
			fun simplify() =
				case Splayset.find (fn _ => 
					true) (!simplifyWorklist) of
					SOME n => (simplifyWorklist := Splayset.difference(!simplifyWorklist, Splayset.singleton String.compare n);
								pushStack n;
								Splayset.app decrementDegree (adjacent n))
					| NONE => ()

			(* ok : (tigertemp.temp * tigertemp.temp) -> bool *)
			fun ok(t,r) =
				let
					val degree_t = case Splaymap.peek(!degree, t) of
						SOME i => i
						| NONE => raise Fail "Error - regAlloc. ok() peek error"
				in
					degree_t < k orelse Splayset.member(precolored, t) orelse Splayset.member(!adjSet, (t, r))
				end

			fun condCoalesce(u, v) = 
				Splayset.foldl (fn (t, b) => b andalso ok(t, u)) true (adjacent v)

			(* addWorkList : tigertemp.temp -> unit *)
			fun addWorkList u = 
				let
					val degree_u = case Splaymap.peek(!degree, u) of
						SOME i => i
						| NONE => raise Fail "Error - regAlloc. addWorkList() peek error"
				in
					if not(Splayset.member(precolored, u)) andalso not(moveRelated u) andalso degree_u < k  
					then 
						let
							val singleton_u = Splayset.singleton String.compare u
						in
							(freezeWorklist := Splayset.difference(!freezeWorklist, singleton_u);
							simplifyWorklist := Splayset.union(!simplifyWorklist, singleton_u))
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
							val degree_n = case Splaymap.peek(!degree, n) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. conservative() peek error"
						in
							if degree_n >= k 
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
					 	val alias_n = case Splaymap.peek(!alias, n) of
					 		SOME a => a
					 		| NONE => raise Fail "Error - regAlloc. getAlias() peek error"
					 in
					 	getAlias alias_n	
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
				alias := Splaymap.insert(!alias, v, u);
				let
					val moveList_u = case Splaymap.peek(!moveList, u) of
						SOME pair => pair
						| NONE => raise Fail "Error - regAlloc. combine() peek error"

					val moveList_v = case Splaymap.peek(!moveList, v) of
						SOME pair => pair
						| NONE => raise Fail "Error - regAlloc. combine() peek error"
				in
					moveList := Splaymap.insert(!moveList, u, Splayset.union(moveList_u, moveList_v))
				end;
				enableMoves(Splayset.singleton String.compare v);
				Splayset.app (fn t => 
					(addEdge(t, u); decrementDegree t)) (adjacent v);
				let
					val degree_u = case Splaymap.peek(!degree, u) of
						SOME i => i
						| NONE => raise Fail "Error - regAlloc. combine() peek error"
				in
					if degree_u >= k andalso Splayset.member(!freezeWorklist, u) 
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

												val singleton_m = Splayset.singleton cmpPairs m

												val _ = worklistMoves := Splayset.difference(!worklistMoves, singleton_m)
											in
												if u = v 
												then 
													(coalescedMoves := Splayset.union(!coalescedMoves, singleton_m);
													addWorkList u) 
												else 
													if Splayset.member(precolored, v) orelse Splayset.member(!adjSet, (u, v)) 
													then 
														(constrainedMoves := Splayset.union(!constrainedMoves, singleton_m);
														addWorkList u;
														addWorkList v) 
													else 
														if (Splayset.member(precolored, u) andalso condCoalesce(u, v))
															orelse not(Splayset.member(precolored, u)
																andalso conservative(Splayset.union(adjacent u, adjacent v))) 
														then 
															(coalescedMoves := Splayset.union(!coalescedMoves, singleton_m);
															combine(u, v);
															addWorkList u) 
														else 
															activeMoves := Splayset.union(!activeMoves, singleton_m)
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
							val degree_v = case Splaymap.peek(!degree, v) of
								SOME i => i
								| NONE => raise Fail "Error - regAlloc. freezeMoves() peek error"
						in
							if Splayset.isEmpty(nodeMoves v) andalso degree_v < k 
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
									val singleton_u = Splayset.singleton String.compare u
								in
									(freezeWorklist := Splayset.difference(!freezeWorklist, singleton_u);
									simplifyWorklist := Splayset.union(!simplifyWorklist, singleton_u);
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

					val singleton_m = Splayset.singleton String.compare m
				in
					(spillWorklist := Splayset.difference(!spillWorklist, singleton_m);
					simplifyWorklist := Splayset.union(!simplifyWorklist, singleton_m);
					freezeMoves m)
				end

			(* repeat : unit -> unit *)
			fun repeat() =
	            let 
	            	val _ = if not(Splayset.isEmpty (!simplifyWorklist)) then simplify()
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

	        (* assignColors : unit -> unit *)
	       	fun assignColors() =
	       		let
	       			fun whileSelectStackNotEmpty() =
	       				if (!selectStack) <> [] 
	       				then 
	       					let
	       						val n = popStack()
	       					 	val okColors = ref kColors

	       					 	val adjList_n = case Splaymap.peek(!adjList, n) of
	       					 		SOME tmpSet => tmpSet
	       					 		| NONE => raise Fail "Error - regAlloc. assignColors() peek error"

	       					 	val _ = Splayset.app (fn w => 
	       					 		if Splayset.member(Splayset.union(!coloredNodes, precolored), getAlias w) 
	       					 		then 
	       					 			let
	       					 				val colorAlias_w = case Splaymap.peek(!color, getAlias w) of
	       					 					SOME reg => reg
	       					 					| NONE => raise Fail "Error - regAlloc. assignColors() peek error"
	       					 			in
	       					 				okColors := Splayset.difference(!okColors, Splayset.singleton String.compare colorAlias_w)
	       					 			end
	       					 		else 
	       					 			()) adjList_n

	       					 	val _ = if Splayset.isEmpty (!okColors) 
			       					 	then 
			       					 		spilledNodes := Splayset.union(!spilledNodes, Splayset.singleton String.compare n) 
			       					 	else 
			       					 		let
			       					 			val _ = coloredNodes := Splayset.union(!coloredNodes, Splayset.singleton String.compare n)
			       					 			val c = case Splayset.find (fn _ => true) (!okColors) of
			       					 				SOME reg => reg
			       					 				| NONE => raise Fail "Error - regAlloc. assignColors() peek error"
			       					 			val _ = color := Splaymap.insert(!color, n, c)
			       					 		in
			       					 			()
			       					 		end
	       					in
	       						whileSelectStackNotEmpty()
	       					end
	       				else 
	       					()
	       		in
	       			(whileSelectStackNotEmpty();
	       			Splayset.app (fn n => 
	       				let
	       					val colorAlias_n = case Splaymap.peek(!color, getAlias n) of
	       						SOME reg => reg
	       						| NONE => raise Fail "Error - regAlloc. assignColors() peek error"
	       					val _ = color := Splaymap.insert(!color, n, colorAlias_n)
	       				in
	       					()
	       				end) (!coalescedNodes))
	       		end

	       	(* rewriteProgram() allocates memory locations for the spilled nodes temporaries and 
	       	inserts store and fetch instructions to access them. Those stores and fetches are 
	       		newly created temporaries (with tiny live ranges). *)
	       	(* rewriteProgram : unit -> tigerassem.instr list *)
	       	fun rewriteProgram() =
	       		let
	       			(* Table for mapping each v in spilledNodes to a memory location *)
	       			val memLocTable : (tigertemp.temp, int) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

	       			val spilledNodesList : tigertemp.temp list = Splayset.listItems (!spilledNodes)

	       			(* Allocate memory locations for each v in spilledNodes *)
	       			val spilledNodesList' : (tigertemp.temp * int) list = List.map (fn tmp => 
	       				(tmp, tigerframe.getOffsetFromAccess(tigerframe.allocLocal frame true))) spilledNodesList

	       			(* Update memLocTable *)
	       			val _ = List.foldl (fn ((tmp, offset), dict) => 
	       				Splaymap.insert(dict, tmp, offset)) (!memLocTable) spilledNodesList'

	       			val newTemps : tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)

	       			(* newStore : tigertemp.temp -> tigertemp.temp -> tigerassem.instr *)
	       			fun newStore oldTmp newTmp =
	       				let
	       					val oldTmpOffset = case Splaymap.peek(!memLocTable, oldTmp) of
	       						SOME i => i
	       						| NONE => raise Fail "Error - regAlloc. rewriteProgram(). newStore() peek error"
	       				in
	       					tigerassem.OPER {
	       						assem = "movq `s0, "^utils.intToString oldTmpOffset^"(`s1) # SPILLED - STORE\n",
	       						src = [newTmp,
	       							tigerframe.fp],
	       						dst = [],
	       						jump = NONE
	       					}
	       				end

	       			(* newLoad : tigertemp.temp -> tigertemp.temp -> tigerassem.instr *)
	       			fun newLoad newTmp oldTmp =
	       				let
	       					val oldTmpOffset = case Splaymap.peek(!memLocTable, oldTmp) of
	       						SOME i => i
	       						| NONE => raise Fail "Error - regAlloc. rewriteProgram(). newLoad() peek error"
	       				in
	       					tigerassem.OPER {
	       						assem = "movq "^utils.intToString oldTmpOffset^"(`s0), `d0 # SPILLED - LOAD\n",
	       						src = [tigerframe.fp],
	       						dst = [newTmp],
	       						jump = NONE
	       					}
	       				end

	       			(* rewriteInstruction : tigerassem.instr -> tigerassem.instr list *)
	       			fun rewriteInstruction (instr as (tigerassem.LABEL l)) = [instr]
	       				| rewriteInstruction instr =
	       					let
	       						val instrUsesSet : tigertemp.temp Splayset.set = Splayset.addList(Splayset.empty String.compare, tigerassem.instrSrcsToList instr)
	       						val instrDefsSet : tigertemp.temp Splayset.set = Splayset.addList(Splayset.empty String.compare, tigerassem.instrDstsToList instr)
	       					
	       						(* Just consider spilled nodes *)
	       						val instrUsesSpilledSet : tigertemp.temp Splayset.set = Splayset.intersection(instrUsesSet, !spilledNodes)
	       						val instrDefsSpilledSet : tigertemp.temp Splayset.set = Splayset.intersection(instrDefsSet, !spilledNodes)

	       						(* Convert sets to lists *)
	       						val instrUsesSpilledList : tigertemp.temp list = Splayset.listItems instrUsesSpilledSet
	       						val instrDefsSpilledList : tigertemp.temp list = Splayset.listItems instrDefsSpilledSet

	       						(* Create a new temporary v_i for each definition and each use; and put them all into newTemps set *)
	       						val instrUsesPairList : (tigertemp.temp * tigertemp.temp) list = List.map (fn tmp => 
	       							let
	       								val newTmp = tigertemp.newtemp()
	       								val _ = newTemps := Splayset.add(!newTemps, newTmp)
	       							in
	       								(tmp, newTmp)
	       							end) instrUsesSpilledList
	       						val instrDefsPairList : (tigertemp.temp * tigertemp.temp) list = List.map (fn tmp => 
	       							let
	       								val newTmp = tigertemp.newtemp()
	       								val _ = newTemps := Splayset.add(!newTemps, newTmp)
	       							in
	       								(tmp, newTmp)
	       							end) instrUsesSpilledList

	       						(* Perform store's and load's *)
	       						val storesList : tigerassem.instr list = List.map (fn (oldTmp, newTmp) => 
	       							newStore oldTmp newTmp) instrDefsPairList
	       						val loadsList : tigerassem.instr list = List.map (fn (oldTmp, newTmp) => 
	       							newLoad oldTmp newTmp) instrUsesPairList

	       						val _ = print("\ninstrDefsPairList = "^utils.listToString instrDefsPairList allocPairToString^"\n")
	       						val _ = print("\ninstrUsesPairList = "^utils.listToString instrUsesPairList allocPairToString^"\n")

	       						(* getReplace : tigertemp.temp -> (tigertemp.temp * tigertemp.temp) list -> tigertemp.temp *)
	       						fun getReplace (tmp : tigertemp.temp) (listOfPairs : (tigertemp.temp * tigertemp.temp) list) =
	       							let
	       								val newTmp = case List.find (fn (old, new) => 
	       									tmp = old) listOfPairs of
	       									SOME (old, new) => new
	       									| NONE => raise Fail "Error - regAlloc. rewriteProgram(). rewriteProgram(). getRepĺace(): temporario no encontrado en lista de pares"
	       							in
	       								newTmp
	       							end

	       						(* Now, rewrite instruction placing new recently created temporaries *)
	       						val newInstr : tigerassem.instr = case instr of
	       							tigerassem.OPER {assem, src, dst, jump} => tigerassem.OPER {
	       								assem = assem,
	       								src = List.map (fn tmp => 
	       									getReplace tmp instrUsesPairList) src,
	       								dst = List.map (fn tmp => 
	       									getReplace tmp instrDefsPairList) dst,
	       								jump = jump 		
	       							}
	       							| tigerassem.MOVE {assem, src, dst} => tigerassem.MOVE {
	       								assem = assem,
	       								src = getReplace src instrUsesPairList,
	       								dst = getReplace dst instrDefsPairList
	       							}
	       					in
	       						(* this has to be a list of tigerassem.instr *)
	       						storesList
	       						@ [newInstr]
	       						@ loadsList
	       					end

	       			(* Get only instructions from iGraph *)
	       			val iGraphList : tigerliveness.node list = Splayset.listItems (!iGraph)
	       			val instrList : tigerassem.instr list = List.map (fn node => 
	       				tigerliveness.getInstrFromNode node) iGraphList


	       			val _ = (spilledNodes := Splayset.empty String.compare;
	       					initial := Splayset.union(Splayset.union(!coloredNodes, !coalescedNodes), !newTemps);
	       					coloredNodes := Splayset.empty String.compare;
	       					coalescedNodes := Splayset.empty String.compare)
	       		in
	       			List.concat (List.map rewriteInstruction instrList) : tigerassem.instr list
	       		end

	       	(* checkInvariants : unit -> unit *)
	       	fun checkInvariants() =
	       		let
	       			(* INVARIANTS. After makeWorklist(), the following invariants always hold *)

	       			fun degreeInv() =
	       				let
	       					val unionSet = Splayset.union(Splayset.union(!simplifyWorklist, !freezeWorklist), !spillWorklist)
	       					val unionSet' = Splayset.union(unionSet, precolored)

	       					val boolRes = Splayset.foldl (fn (tmp, b) => 
	       						(Splaymap.find(!degree, tmp) = Splayset.numItems(Splayset.intersection(unionSet', Splaymap.find(!adjList, tmp)))) andalso b) true unionSet
	       							handle NotFound => raise Fail "Error - regAlloc. degreeInv(): exception NotFound"
	       				in
	       					if not(boolRes) 
	       					then 
	       						raise Fail "Error - regAlloc. degreeInv does not hold!" 
	       					else 
	       						()
	       				end

	       			fun simplifyWorklistInv() =
	       				let
	       					val unionSet = Splayset.union(!activeMoves, !worklistMoves)

	       					val boolRes = Splayset.foldl (fn (tmp, b) => 
	       						Splaymap.find(!degree, tmp) < k andalso Splayset.isEmpty(Splayset.intersection(Splaymap.find(!moveList, tmp), unionSet)) andalso b) true (!simplifyWorklist)
	       							handle NotFound => raise Fail "Error - regAlloc. simplifyWorklistInv(): exception NotFound"
	       				in
	       					if not(boolRes) 
	       					then 
	       						raise Fail "Error - regAlloc. simplifyWorklistInv does not hold!"  
	       					else 
	       						()
	       				end

	       			fun freezeWorklistInv() =
	       				let
	       					val unionSet = Splayset.union(!activeMoves, !worklistMoves)

	       					val boolRes = Splayset.foldl (fn (tmp, b) => 
	       						Splaymap.find(!degree, tmp) < k andalso not(Splayset.isEmpty(Splayset.intersection(Splaymap.find(!moveList, tmp), unionSet))) andalso b) true (!freezeWorklist)
	       							handle NotFound => raise Fail "Error - regAlloc. freezeWorklistInv(): exception NotFound"
	       				in
	       					if not(boolRes) 
	       					then 
	       						raise Fail "Error - regAlloc. freezeWorklistInv does not hold!"  
	       					else 
	       						()
	       				end

	       			fun spillWorklistInv() =
	       				let
	       					val boolRes = Splayset.foldl (fn (tmp, b) => 
	       						Splaymap.find(!degree, tmp) >= k andalso b) true (!spillWorklist)
	       							handle NotFound => raise Fail "Error - regAlloc. spillWorklistInv(): exception NotFound"
	       				in
	       					if not(boolRes) 
	       					then 
	       						raise Fail "Error - regAlloc. spillWorklistInv does not hold!"  
	       					else 
	       						()
	       				end
	       		in
	       			(degreeInv();
	       			simplifyWorklistInv();
	       			freezeWorklistInv();
	       			spillWorklistInv())
	       		end

	       	(* printDataStructure : string -> unit *)
	       	fun printDataStructure ds =
	       		case ds of
	       			"precolored" => print("\n"^"precolored = "^(utils.setToString precolored utils.id)^"\n")
	       			| "initial" => print("\n"^"initial = "^(utils.setToString (!initial) utils.id)^"\n")
	       			| "simplifyWorklist" => print("\n"^"simplifyWorklist = "^(utils.setToString (!simplifyWorklist) utils.id)^"\n")
	       			| "freezeWorklist" => print("\n"^"freezeWorklist = "^(utils.setToString (!freezeWorklist) utils.id)^"\n")
	       			| "spillWorklist" => print("\n"^"spillWorklist = "^(utils.setToString (!spillWorklist) utils.id)^"\n")
	       			| "spilledNodes" => print("\n"^"spilledNodes = "^(utils.setToString (!spilledNodes) utils.id)^"\n")
	       			| "coalescedNodes" => print("\n"^"coalescedNodes = "^(utils.setToString (!coalescedNodes) utils.id)^"\n")
	       			| "coloredNodes" => print("\n"^"coloredNodes = "^(utils.setToString (!coloredNodes) utils.id)^"\n")
	       			| "selectStack" => print("\n"^"selectStack = "^(utils.listToString (!selectStack) utils.id)^"\n")
	       			| "coalescedMoves" => print("\n"^"coalescedMoves = "^(utils.setToString (!coalescedMoves) (fn pair => 
	       				utils.pairToString pair utils.id utils.id))^"\n")
	       			| "constrainedMoves" => print("\n"^"constrainedMoves = "^(utils.setToString (!constrainedMoves) (fn pair => 
	       				utils.pairToString pair utils.id utils.id))^"\n")
	       			| "frozenMoves" => print("\n"^"frozenMoves = "^(utils.setToString (!frozenMoves) (fn pair => 
	       				utils.pairToString pair utils.id utils.id))^"\n")
	       			| "worklistMoves" => print("\n"^"worklistMoves = "^(utils.setToString (!worklistMoves) (fn pair => 
	       				utils.pairToString pair utils.id utils.id))^"\n")
	       			| "activeMoves" => print("\n"^"activeMoves = "^(utils.setToString (!activeMoves) (fn pair => 
	       				utils.pairToString pair utils.id utils.id))^"\n")
	       			| "adjSet" => print("\n"^"adjSet = "^(utils.setToString (!adjSet) (fn pair => 
	       				utils.pairToString pair utils.id utils.id))^"\n")
	       			| "adjList" => let
					       				val adjList' = Splaymap.listItems (!adjList)
					       			in
					       				print("\n"^"adjList = "^(utils.listToString adjList' (fn pair => 
	       									utils.pairToString pair utils.id (fn tmpSet => utils.setToString tmpSet utils.id)))^"\n")
					       			end
					| "degree" => let
									val degree' = Splaymap.listItems (!degree)
								in
									print("\n"^"degree = "^(utils.listToString degree' (fn pair => 
	       									utils.pairToString pair utils.id Int.toString))^"\n")
								end
					| "moveList" => let
										val moveList' : (tigertemp.temp * move Splayset.set) list = Splaymap.listItems (!moveList)
										val moveList'' : (tigertemp.temp * string) list = List.map (fn (tmp, moveSet) => 
											(tmp, utils.setToString moveSet allocPairToString)) moveList'
									in
										print("\n"^"moveList = "^(utils.listToString moveList'' (fn pair => 
	       									utils.pairToString pair utils.id utils.id))^"\n")
									end
					| "color" => let
									val color' = Splaymap.listItems (!color)
								in
									print("\n"^"color = "^(utils.listToString color' (fn pair => 
	       									allocPairToString pair))^"\n")
								end
					| "kColors" => print("\n"^"kColors = "^(utils.setToString kColors utils.id)^"\n")
	       			| _ => raise Fail "Error - regAlloc. printDataStructure(): data structured not considered"
	       	
	       	(* printAllDataStructures : unit -> unit *)
	       	fun printAllDataStructures() =
	       		(print("\n====================\t Begin all data structures \t====================\n");
	       		printDataStructure "precolored";
	       		printDataStructure "initial";
	       		printDataStructure "simplifyWorklist";
	       		printDataStructure "freezeWorklist";
	       		printDataStructure "spillWorklist";
	       		printDataStructure "spilledNodes";
	       		printDataStructure "coalescedNodes";
	       		printDataStructure "coloredNodes";
	       		printDataStructure "selectStack";
	       		printDataStructure "coalescedMoves";
	       		printDataStructure "constrainedMoves";
	       		printDataStructure "frozenMoves";
	       		printDataStructure "worklistMoves";
	       		printDataStructure "activeMoves";
	       		printDataStructure "adjSet";
	       		printDataStructure "adjList";
	       		printDataStructure "degree";
	       		printDataStructure "moveList";
	       		printDataStructure "color";
	       		printDataStructure "kColors";
	       		print("\n====================\t End all data structures \t====================\n"))

		in
			(livenessAnalysis();
			print("\n**liveAnalysis() DONE\n");
			build();
			print("\n**build() DONE\n");
			makeWorklist();
			print("\n**makeWorklist() DONE\n");
			checkInvariants();
			repeat();
			print("\n**repeat() DONE\n");
			assignColors();
			print("\n**assignColors() DONE\n");
			if not(Splayset.isEmpty (!spilledNodes))
			then 
				let
					val newInstrList = rewriteProgram();

					(* perform algorithm all over again with new 
						altered graph *)
					val _ = regAlloc(newInstrList, frame)
				in
					()
				end
			else 
				();
			(!color))	
		end

end