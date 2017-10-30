structure tigergraph :> tigergraph =
struct
  open Dynarray                                                                   (* Array implementation *)

  type node' = int
  type temp = tigertemp.temp

  datatype noderep = NODE of {succ: node' list, pred: node' list}

  val emptyNode = NODE{succ=[], pred=[]}

  val bogusNode = NODE{succ=[~1], pred=[]}

  fun isBogus(NODE{succ= ~1::_, ...}) = true
    | isBogus _ = false

  type graph = noderep array                                                      (* array of noderep's *)

  type node = graph * node'                                                       (* Contains a noderep allong with its assigned number within a graph *)

  type 'a tableNodes = (node, 'a) tigertab.Tabla

  fun eq((_,a),(_,b)) = a=b                                                       (* Two nodes are equal if their assigned number are equal *)

  fun augment (g: graph) (n: node') : node = (g,n)                                (* Augments a graph with a new node *)

  fun newGraph() = array(0, bogusNode)                                            (* [array(n, d)] returns a dynamic array, all of whose elements are initialized to 
                                                                                  the default d.  The parameter n is used as a hint of the upper bound on non-default elements *)

  fun nodes g = 
    let 
        fun f i = 
            if isBogus( sub(g,i)) 
            then nil
            else (g,i)::f(i+1)
    in 
        f 0          
    end

  fun succ(g,i) = let val NODE{succ=s,...} = sub(g,i) 
		   in map (augment g) s 
		  end

  fun pred(g,i) = let val NODE{pred=p,...} = sub(g,i)
                     in map (augment g) p 
		  end

  fun adj gi = pred gi @ succ gi

  fun newNode g = (* binary search for unused node *)
    let fun look(lo,hi) =
               (* i < lo indicates i in use
                  i >= hi indicates i not in use *)
            if lo=hi then (update(g,lo,emptyNode); (g,lo))
            else let val m = (lo+hi) div 2
                  in if isBogus(sub(g,m)) then look(lo,m) else look(m+1,hi)
                 end
     in look(0, 1 + bound g)
    end

  exception GraphEdge

  fun check(g,g') = (* if g=g' then () else raise GraphEdge *) ()

  fun delete(i,j::rest) = if i=j then rest else j::delete(i,rest)
    | delete(_,nil) = raise GraphEdge

  fun diddle_edge change {from=(g:graph, i),to=(g':graph, j)} = 
      let val _ = check(g,g')
          val NODE{succ=si,pred=pi} = sub(g,i)
          val _ = update(g,i,NODE{succ=change(j,si),pred=pi})
          val NODE{succ=sj,pred=pj} = sub(g,j)
          val _ = update(g,j,NODE{succ=sj,pred=change(i,pj)})
       in ()
      end

  val mk_edge = diddle_edge (op ::)
  val rm_edge = diddle_edge delete

  fun nodename(g,i:int) = "n" ^ Int.toString(i)

end

