signature tigergraph =
sig
    type graph

    type node

    type 'a Table = (node, 'a) tigertab.Tabla

    val nodes: graph -> node list                               (* Returns nodes within a graph *)
    val succ: node -> node list                                 (* Returns a list of successors nodes of a given node *)
    val pred: node -> node list                                 (* Returns a list of predecessors nodes of a given node *)
    val adj: node -> node list                                  (* When working with undirected graphs, adj(m) = succ(m) + pred(m) *)
    val eq: node * node -> bool                                 (* Test whether two given nodes are equal *)                

    val newGraph: unit -> graph                                 (* Creates an empty directed graph *)
    val newNode : graph -> node                                 (* newNode(g): Makes a new node within a graph *)
    
    exception GraphEdge
    
    val mk_edge: {from: node, to: node} -> unit                 (* mk_edge(n, m): Creates a new directed edge from node n to node m; after that, m will be found in the list succ(n) and n will be in pred(m) *)
    val rm_edge: {from: node, to: node} -> unit                 (* Delete an edge *)

    val nodename: node -> string                                (* for debugging only *)

    val newNodeTable : unit -> (node, 'a) tigertab.Tabla        (* Creates new node table where every node has assigned particular info *)

end