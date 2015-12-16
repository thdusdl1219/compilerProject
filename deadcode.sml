signature DEADCODE = sig
  val deadcode_elimination: X86.funcode -> X86.funcode
end
structure Deadcode : DEADCODE = struct
  structure IG = Graph(X86.RegSet)
  structure M = X86
  structure RS = X86.RegSet
  structure FS = RedBlackSetFn(type ord_key = M.lab val compare = Symbol.compare)
  structure FG = Graph(FS)

 val live_at : RS.set Symbol.table ref = ref Symbol.empty 
 fun print_set set : unit = 
   (RS.app (fn reg => (print " "; print (M.reg2name reg))) set; print "\n")

 fun compute_live_in(M.Li(r, i)::M.Syscall::rest, live_at_end) (live_at : RS.set Symbol.table) : (RS.set * M.instruction list) =
 let 
   val (live_out, real_rest) = compute_live_in(rest, live_at_end) live_at
   val def_use_Li = M.instr_def_use(M.Li(r,i)) 
 in
   if (r = M.reg "%eax") then
     (case M.syscall_def_use (M.immed2int i) of
          SOME def_use =>  (RS.union(#use(def_use), RS.difference(live_out, RS.union(#def(def_use_Li),#def(def_use)))), M.Li(r, i) :: M.Syscall :: real_rest)
        | NONE => ErrorMsg.impossible "Unknown Syscall")
   else ErrorMsg.impossible "Syscall not preceded by li %eax" 
 end

 | compute_live_in(i::rest, live_at_end) (live_at : RS.set Symbol.table) : (RS.set * M.instruction list) = (* ErrorMsg.impossible "Liveness.analyze unimplemented" *)
  let 
    val (live_out, real_rest) = compute_live_in(rest, live_at_end) live_at; 
    val def_use = M.instr_def_use(i)
  in 
      (case i of
      M.Branchz(_,_,lab) => 
        (case Symbol.look(live_at, lab) of 
          SOME set => (RS.union(#use(def_use), RS.difference(RS.union(set,live_out), #def(def_use))), i::real_rest)
        | NONE => (RS.union(#use(def_use), RS.difference(RS.union(RS.empty,live_out), #def(def_use))), i::real_rest))

    | M.Branch(_,_,_,lab) => 
        (case Symbol.look(live_at, lab) of 
          SOME set => (RS.union(#use(def_use), RS.difference(RS.union(set,live_out), #def(def_use))), i::real_rest)
        | NONE => (RS.union(#use(def_use), RS.difference(RS.union(RS.empty,live_out), #def(def_use))), i::real_rest))

    | M.J(lab) => 
        (case Symbol.look(live_at, lab) of
          SOME set => (RS.union(#use(def_use), RS.difference(set, #def(def_use))), i::real_rest)
        | NONE => (RS.union(#use(def_use), RS.difference(RS.empty, #def(def_use))), i::real_rest)) 

    | M.Jal(lab) => 
        (case Symbol.look(live_at, lab) of
          SOME set => ((RS.union(#use(def_use), RS.difference(RS.union(set,live_out), #def(def_use)))), i::real_rest)
        | NONE => 
          let
            val def_use = {use= RS.union(M.list2set([M.reg "%eax"]), #use(def_use)), def= RS.union(M.list2set(M.callerSaved), #def(def_use))}
          in
            (RS.union(#use(def_use), RS.difference(live_out, #def(def_use))), i::real_rest) 
          end)
    | M.Push(reg) =>
            (RS.union(#use(def_use), RS.difference(live_out, #def(def_use))), i :: real_rest)
    | M.Pop(reg) =>
            (RS.union(#use(def_use), RS.difference(live_out, #def(def_use))), i :: real_rest)
    | M.Jalr(r1, r2, rr1, rr2) =>
            (RS.union(#use(def_use), RS.difference(live_out, #def(def_use))), i :: real_rest)
    | M.Leave =>
            (RS.union(#use(def_use), RS.difference(live_out, #def(def_use))), i :: real_rest)
    | M.Ret =>
            (RS.union(#use(def_use), RS.difference(live_out, #def(def_use))), i :: real_rest)
    | _ =>
        if (RS.isEmpty(#def(def_use)))
        then
            (RS.union(#use(def_use), RS.difference(live_out, #def(def_use))), i :: real_rest)
        else
          if (RS.isEmpty(RS.intersection(live_out, #def(def_use))))
          then
            (live_out, real_rest )
          else
            (RS.union(#use(def_use), RS.difference(live_out, #def(def_use))), i :: real_rest))
  end

 | compute_live_in(nil, live_at_end) (live_at : RS.set Symbol.table) : (RS.set * M.instruction list) = (live_at_end, nil)


 fun analyze_func (blocks :M.codeblock list) (live_at : RS.set Symbol.table) (codelist : M.codeblock list) (changed : bool) : (RS.set Symbol.table * bool * M.codeblock list) =
  case blocks of
    (lab,block)::(next_lab, next_block)::t =>         
      let 
        val next_live_at = Symbol.look(live_at, next_lab) 
      in
        case next_live_at of
          SOME set =>
            let 
              val (new, real_block) = compute_live_in(block, set) live_at; val cur_live_at = Symbol.look(live_at, lab) 
            in 
              (case cur_live_at of
                SOME cur_set => (analyze_func ((next_lab, next_block)::t) (Symbol.enter(live_at, lab, new)) (codelist @ [(lab, real_block)]) (changed orelse (not(RS.equal(cur_set, new)))))
              | NONE => (analyze_func ((next_lab, next_block)::t) (Symbol.enter(live_at, lab, new)) (codelist @ [(lab, real_block)]) true)) 
            end
        | NONE => 
            let 
              val (new, real_block) = compute_live_in(block, RS.empty) live_at; val cur_live_at = Symbol.look(live_at, lab) 
            in
              (case cur_live_at of
                SOME cur_set => (analyze_func ((next_lab, next_block)::t) (Symbol.enter(live_at, lab, new)) (codelist @ [(lab, real_block)]) (changed orelse (not(RS.equal(cur_set, new)))))
              | NONE => ( analyze_func ((next_lab, next_block)::t) (Symbol.enter(live_at, lab, new)) (codelist @ [(lab, real_block)]) true)) 
            end
      end
    | (lab,block)::[] => 
          let 
            val (new, real_block) = compute_live_in(block, RS.empty) live_at; val cur_live_at = Symbol.look(live_at, lab) 
          in 
            (case cur_live_at of
              SOME cur_set => ( analyze_func [] (Symbol.enter(live_at, lab, new)) (codelist @ [(lab, real_block)]) (changed orelse (not(RS.equal(cur_set, new)))))
            | NONE => ( analyze_func [] (Symbol.enter(live_at, lab, new)) (codelist @ [(lab, real_block)]) true)) 
          end
    | [] => (live_at, changed, codelist)

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
 

 fun analyze (blocks: M.codeblock list) =
    let val _ = () 
      fun loop result =
        if (#2(result)) 
        then 
          (live_at := #1(result); loop (analyze_func blocks (!live_at) [] false))
        else 
          #3(result)
    in 
      loop (analyze_func blocks (!live_at) [] false)
    end

 fun printadj say g i = 
     (say (M.reg2name i); say ":";
      IG.S.app (fn j => (say " "; say (M.reg2name j))) (IG.adj g i);
      say "\n")

 fun printgraph say g = IG.S.app (printadj say g) (IG.nodes g);

 fun deadcode_elimination(func: M.funcode) =
  let (*val _ = (print "################## LIVENESS: "; 
               print (Symbol.name(#1(List.nth(func,0)))); print "\n")*)
    val _ = ()
  in 
    analyze func
      (*print "################## INTERFERENCE GRAPH \n";
      printgraph print g;*)
  end
end
