functor Graph (S: ORD_SET) : GRAPH where S=S =
struct
  structure S = S

  type node = S.Key.ord_key
  datatype noderep = NODE of {succ: S.set, pred: S.set}

  val emptyNode = NODE{succ=S.empty,pred=S.empty}

  structure H = RedBlackMapFn(S.Key)
  type graph = noderep H.map ref

  fun newGraph() = ref H.empty

  fun put(g,i,x) = g := H.insert(!g, i, x)
  fun get(g,i) = case H.find(!g,i) 
                 of SOME x => x 
                 | NONE => (put(g,i,emptyNode); emptyNode)

  fun succ g i = let val NODE{succ=x,...} = get(g,i) in x end
  fun pred g i = let val NODE{pred=x,...} = get(g,i) in x end
  fun adj g i =  let val NODE{pred=p,succ=s} = get(g,i) in S.union(p,s) end

  fun mk_edge g {from,to} =
    let val NODE{pred=p1,succ=s1} = get (g,from)
        val NODE{pred=p2,succ=s2} = get (g,to)
     in put (g,from, NODE{pred=p1, succ=S.union(s1, S.singleton to)});
        put (g,to,   NODE{pred=S.union(p2, S.singleton from), succ=s2})
    end

  fun delete'(s,x) = S.delete(s,x) handle NotFound => s
  fun rm_edge g {from,to} =
    let val NODE{pred=p1,succ=s1} = get (g,from)
        val NODE{pred=p2,succ=s2} = get (g,to)
     in put(g, from, NODE{pred=p1, succ=delete'(s1, to)});
        put(g, to,   NODE{pred=delete'(p2, from), succ=s2})
    end

  fun nodes g = H.foldli (fn(i,_,s) => S.add(s,i)) S.empty (!g)

end

