signature GRAPH = sig
  structure S : ORD_SET

  type node = S.Key.ord_key
  
  type graph
  val succ: graph -> node -> S.set (* gets the set of successors of a node *)
  val pred: graph -> node -> S.set (* gets the set of predessors of a node *)
  val adj: graph -> node -> S.set  (*  union(succ, pred)  *)
    
  val newGraph: unit -> graph  (* makes a new, empty graph *)
  
  val mk_edge: graph -> {from: node, to: node} -> unit
    (* mk_edge adds a directed edige (from,to) to the graph.
       If the edge is already there, then the graph doesn't change.
    *)

  val rm_edge: graph -> {from: node, to: node} -> unit
    (* rm_edge removes the directed edige (from,to) to the graph.
       If the edge wasn't there, then the graph doesn't change.
    *)

  val nodes: graph -> S.set  (* gets all the nodes that have ever been
                                mentioned in any mk_edge, rm_edge, succ, 
                                pred, or adj operation on this graph*)
end