signature LIVENESS = sig
  structure IG: GRAPH where S=X86.RegSet
  val analyze: {mention: X86.reg -> unit, 
	        interfere: X86.reg -> X86.reg -> unit} ->
               X86.funcode -> (X86.RegSet.set Symbol.table)
  val interference_graph: X86.funcode -> IG.graph
  val printgraph: (string->unit) -> IG.graph -> unit
end

   (* ErrorMsg.impossible "Liveness.analyze unimplemented" *)
structure Liveness : LIVENESS = struct
  structure IG = Graph(X86.RegSet)
  structure M = X86
  structure RS = X86.RegSet
  structure FS = RedBlackSetFn(type ord_key = M.lab val compare = Symbol.compare)
  structure FG = Graph(FS)

 fun print_set set : unit = 
   (RS.app (fn reg => (print " "; print (M.reg2name reg))) set; print "\n")
 
 fun make_mention (mention : M.reg -> unit) (regSet : RS.set) : unit = 
  RS.app mention regSet
 
 fun make_interfere (interfere : M.reg -> M.reg -> unit) (defSet : RS.set) (live_out : RS.set) : unit =
  (RS.app (fn reg1 => RS.app (fn reg2 => interfere reg1 reg2) live_out) defSet;
  RS.app (fn reg1 => RS.app (fn reg2 => interfere reg1 reg2) defSet) live_out)

 fun compute_live_in(M.Li(r, i)::M.Syscall::rest, live_at_end) {mention : M.reg -> unit, interfere : M.reg -> M.reg -> unit} (live_at : RS.set Symbol.table) (flow_graph : FG.graph) : RS.set =
 let val live_out = compute_live_in(rest, live_at_end) {mention=mention, interfere=interfere} live_at flow_graph; val def_use_Li = M.instr_def_use(M.Li(r,i)) in
   if (r = M.reg "%eax") then
     (case M.syscall_def_use (M.immed2int i) of
          SOME def_use => (make_mention mention (#use(def_use)); make_mention mention (#def(def_use)); make_mention mention (#def(def_use_Li)); make_interfere interfere (#def(def_use_Li)) (RS.union(#use(def_use), RS.difference(live_out, #def(def_use)))); RS.union(#use(def_use), RS.difference(live_out, RS.union(#def(def_use_Li),#def(def_use)))))
        | NONE => ErrorMsg.impossible "Unknown Syscall")
    else ErrorMsg.impossible "Syscall not preceded by li %eax" 
 end

 | compute_live_in(i::rest, live_at_end) {mention : M.reg -> unit, interfere : M.reg -> M.reg -> unit} (live_at : RS.set Symbol.table) (flow_graph : FG.graph) : RS.set = (* ErrorMsg.impossible "Liveness.analyze unimplemented" *)
  let 
    val live_out = compute_live_in(rest, live_at_end) {mention=mention, interfere=interfere} live_at flow_graph; 
    val def_use = M.instr_def_use(i) in 
   make_mention mention (#use(def_use)); make_mention mention (#def(def_use)); make_interfere interfere (#def(def_use)) live_out;
    (case i of
      M.Branchz(_,_,lab) => (case Symbol.look(live_at, lab) of 
                                  SOME set => RS.union(#use(def_use), RS.difference(RS.union(set,live_out), #def(def_use)))
                                | NONE => RS.union(#use(def_use), RS.difference(RS.union(RS.empty,live_out), #def(def_use)))) 


    | M.Branch(_,_,_,lab) => (case Symbol.look(live_at, lab) of 
                                   SOME set => RS.union(#use(def_use), RS.difference(RS.union(set,live_out), #def(def_use)))
                                 | NONE => RS.union(#use(def_use), RS.difference(RS.union(RS.empty,live_out), #def(def_use)))) 
    | M.J(lab) => (case Symbol.look(live_at, lab) of
                        SOME set => RS.union(#use(def_use), RS.difference(set, #def(def_use)))
                      | NONE => RS.union(#use(def_use), RS.difference(RS.empty, #def(def_use)))) 
    | M.Jal(lab) =>  ( let val def_use = {use= RS.union(M.list2set([M.reg "%eax"]), #use(def_use)), def= RS.union(M.list2set(M.callerSaved), #def(def_use)) } in ( make_mention mention (#def(def_use)); make_mention mention (#use(def_use)); make_interfere interfere (#def(def_use)) live_out;
        (case Symbol.look(live_at, lab) of
                          SOME set => ((RS.union(#use(def_use), RS.difference(RS.union(set,live_out), #def(def_use)))))
                        | NONE => (RS.union(#use(def_use), RS.difference(live_out, #def(def_use)))))) end ) 

    | _ => RS.union(#use(def_use), RS.difference(live_out, #def(def_use)))) end
 | compute_live_in(nil, live_at_end) {mention : M.reg -> unit, interfere : M.reg -> M.reg -> unit} (live_at : RS.set Symbol.table) (flow_graph : FG.graph) : RS.set = live_at_end

(*  let live_out = compute_live_in*)


 fun analyze_func {mention : M.reg -> unit, interfere: M.reg -> M.reg -> unit} (blocks :M.codeblock list) (live_at : RS.set Symbol.table) (flow_graph : FG.graph) (changed : bool) : (RS.set Symbol.table * bool) =
   (
   case blocks of
        (lab,block)::(next_lab, next_block)::t => (FG.mk_edge flow_graph {from=lab, to=next_lab}; 
        let val next_live_at = Symbol.look(live_at, next_lab) in
          (case next_live_at of
            SOME set =>
              let val new = compute_live_in(block, set) {mention=mention, interfere=interfere} live_at flow_graph; val cur_live_at = Symbol.look(live_at, lab) in 
                (case cur_live_at of
                  SOME cur_set => (analyze_func {mention=mention, interfere=interfere} ((next_lab, next_block)::t) (Symbol.enter(live_at, lab, new)) flow_graph (changed orelse (not(RS.equal(cur_set, new)))))
                | NONE => (analyze_func {mention=mention, interfere=interfere} ((next_lab, next_block)::t) (Symbol.enter(live_at, lab, new)) flow_graph true)) end
          | NONE => 
              let val new = compute_live_in(block, RS.empty) {mention=mention, interfere=interfere} live_at flow_graph; val cur_live_at = Symbol.look(live_at, lab) in
                (case cur_live_at of
                  SOME cur_set => (analyze_func {mention=mention, interfere=interfere} ((next_lab, next_block)::t) (Symbol.enter(live_at, lab, new)) flow_graph (changed orelse (not(RS.equal(cur_set, new)))))
                | NONE => ( analyze_func {mention=mention, interfere=interfere} ((next_lab, next_block)::t) (Symbol.enter(live_at, lab, new)) flow_graph true)) end
          ) end)
      | (lab,block)::[] => 
          let val new = compute_live_in(block, RS.empty) {mention=mention, interfere=interfere} live_at flow_graph; val cur_live_at = Symbol.look(live_at, lab) in 
            (case cur_live_at of
              SOME cur_set => ( analyze_func {mention=mention, interfere=interfere} [] (Symbol.enter(live_at, lab, new)) flow_graph (changed orelse (not(RS.equal(cur_set, new)))))
            | NONE => ( analyze_func {mention=mention, interfere=interfere} [] (Symbol.enter(live_at, lab, new)) flow_graph true)) end
      | [] => (live_at, changed )
      )

 fun printli say l (i:(Symbol.symbol * M.RegSet.set)) =
   (say (Symbol.name(#1(i))); say ":";
    IG.S.app (fn j => (say " "; say (M.reg2name j))) (#2(i));
    say "\n")

 fun print_live_at say live_at lab_list = 
  let val live_at_list = map (fn lab => 
  (case Symbol.look(live_at, lab) of
        SOME set => (lab, set)
      | NONE => (lab, RS.empty)
  )) lab_list in
   app (printli say live_at) live_at_list end
 

 fun analyze {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
             (blocks: M.codeblock list) =
    let val live_at = ref Symbol.empty; val flow_graph = FG.newGraph() 
      fun loop result =
        if(#2(result)) 
          then (live_at := #1(result); loop (analyze_func {mention=mention, interfere=interfere} blocks (!live_at) flow_graph false))
        else 
          !live_at
    in 
      loop (analyze_func {mention=mention, interfere=interfere} blocks (!live_at) flow_graph false)
      (*
      while not(!changed) do (
      let val result = analyze_func {mention=mention,interfere=interfere} blocks (!live_at) flow_graph false in
        live_at := #1(result);
        changed := not(#2(result)) end
      ) *)
(*    ;app (fn (lab, _ ) =>  valOf(Symbol.look(!live_at, lab)))  *)
    end

 fun printadj say g i = 
     (say (M.reg2name i); say ":";
      IG.S.app (fn j => (say " "; say (M.reg2name j))) (IG.adj g i);
      say "\n")

 fun printgraph say g = IG.S.app (printadj say g) (IG.nodes g);

 fun interference_graph(func: M.funcode) =
  let (*val _ = (print "################## LIVENESS: "; 
               print (Symbol.name(#1(List.nth(func,0)))); print "\n")*)
      val g = IG.newGraph()
      fun mention (r: M.reg) = (IG.succ g r; ())
      fun interfere r1 r2 = IG.mk_edge g {from=r1,to=r2}
   in analyze {mention=mention,interfere=interfere} func;
      (*print "################## INTERFERENCE GRAPH \n";
      printgraph print g;*)
      g
  end

end
