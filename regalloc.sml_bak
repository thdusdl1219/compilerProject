signature REGALLOC = sig
(* alloc(f) returns mips code for function f that has all the temporaries
    replaced by mips regs and the load/store of spilled temps taken care of
*)
 val alloc : Mips.funcode ->  Mips.funcode 
end

structure RegAlloc :> REGALLOC =
struct
   structure M = Mips
   structure RS = Mips.RegSet
   structure IG = Liveness.IG
   structure RT = M.RegTb
   

   fun getmove g (M.Move(r1,r2)) = IG.mk_edge g {from=r2,to=r1} 
     | getmove g _ = ()

   fun print_set set : unit =
      (RS.app (fn reg => (print " "; print (M.reg2name reg))) set; print "\n")
   fun flat xs = List.foldr op@ [] xs

  (* after spill renaming, make lw *)
   fun after_spills (spills, spillL, index) = 
     fn M.Move(rd, rs) =>
       if (RS.member(spills, rd) andalso RS.member(spills, rs)) then
         [M.Move(rd, rs)]
       else
       (
          if RS.member(spills, rs) then
            (case List.find (fn (ind, reg) => reg = rs) (!spillL) of
                  SOME((ind, reg)) =>  (
                  [M.Lw(rd, (M.immed (ind * 4), M.reg "$sp"))])
                | NONE => (* ErrorMsg.impossible ( "register isn't in spillL : " ^
                   M.reg2name rs ^ " : " ^ M.reg2name rd) *) [M.Move(rd,rs)]
              )  else [M.Move(rd, rs)])
      | others => [others]


   fun rename_spills (spills, spillL, index, nonSpillL) = 
    let fun f r = if RS.member(spills, r)
              (* then (ErrorMsg.impossible ("rename_spills in non-move : " ^
              M.reg2name r); r) *)
                  then r
                  else r
            
    in
      fn M.Move(rd, rs) => 
        if (RS.member(spills, rd) andalso RS.member(spills, rs)) then
          let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
            nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)); 
          [M.Move(tmpReg1, rs), M.Move(tmpReg2, tmpReg1), M.Move(rd, tmpReg2)]
          end
        else(
          if(RS.member(spills, rd)) then
          (case List.find (fn (ind, reg) => reg = rd) (!spillL) of
                SOME((ind, reg)) => 
          [M.Sw(rs, (M.immed (ind * 4), M.reg "$sp"))]
              | NONE => 
          (index := !index + 1; spillL := ((!index, rd) :: !spillL); 
          [M.Sw(rs, (M.immed (!index * 4), M.reg "$sp"))])) else (
          if RS.member(spills, rs) then
            (case List.find (fn (ind, reg) => reg = rs) (!spillL) of
                  SOME((ind, reg)) =>  (
                  [M.Lw(rd, (M.immed (ind * 4), M.reg "$sp"))])
                | NONE => (* ErrorMsg.impossible ( "register isn't in spillL : " ^
                   M.reg2name rs ^ " : " ^ M.reg2name rd) *) [M.Move(rd, rs)]
              )  else [M.Move(rd, rs)])
              )
       | M.Arith2(i,rd,rs) => 
           if (RS.member(spills, rs) andalso RS.member(spills, rd))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rs), M.Arith2(i, tmpReg2 ,tmpReg1), M.Move(rd,
             tmpReg2)]
           end
           else if RS.member(spills, rs)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rs), M.Arith2(i, f rd, tmpReg1)]
           end
           else if RS.member(spills, rd)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Arith2(i, tmpReg1, f rs), M.Move(rd, tmpReg1)]
           end
           else [M.Arith2(i, f rd, f rs)]

       | M.Arith3(i,rd,rs,rt) => 
           if (RS.member(spills, rs) andalso RS.member(spills, rt) andalso
           RS.member(spills, rd))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg2, rs), M.Move(tmpReg1, rt), M.Arith3(i, tmpReg2,
             tmpReg2, tmpReg1), M.Move(rd, tmpReg2)]
                end
           else if (RS.member(spills, rs) andalso RS.member(spills, rd))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rs), M.Arith3(i, tmpReg1, tmpReg1, f rt),
             M.Move(rd, tmpReg1)]
                end
           else if (RS.member(spills, rt) andalso RS.member(spills, rd))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rt), M.Arith3(i, tmpReg1, f rs, tmpReg1),
             M.Move(rd, tmpReg1)]
                end
           else if (RS.member(spills, rs) andalso RS.member(spills, rt))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg2, rs), M.Move(tmpReg1, rt), M.Arith3(i, f rd, tmpReg2,
           tmpReg1)]
                end
           else if RS.member(spills, rs)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rs), M.Arith3(i, f rd, tmpReg1, f rt)]
                end
           else if RS.member(spills, rt)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rt), M.Arith3(i, f rd, f rs, tmpReg1)]
                end
           else if RS.member(spills, rd)
           then let val tmpReg1 = M.newReg() in 
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Arith3(i, tmpReg1, f rs, f rt), M.Move(rd, tmpReg1)]
                end
           else [M.Arith3(i, f rd, f rs, f rt)]
           
       | M.Arithi(i,rt,rs,n) => 
           if (RS.member(spills, rs) andalso RS.member(spills, rt))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rs), M.Arithi(i, tmpReg2, tmpReg1, n),
             M.Move(rt, tmpReg2)]
                end
           else if RS.member(spills, rs)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rs), M.Arithi(i, f rt, tmpReg1, n)]
                end
           else if RS.member(spills, rt)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Arithi(i, tmpReg1, f rs, n), M.Move(rt, tmpReg1)]
                end
           else [M.Arithi(i, f rt, f rs, n)]

       | M.Li(r,n) => 
           if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Li(tmpReg1, n), M.Move(r, tmpReg1)]
                end
           else [M.Li(f r, n)]
           
       | M.La(r,lab) => 
           if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.La(tmpReg1, lab), M.Move(r, tmpReg1)]
                end
           else [M.La(f r, lab)]

       | M.Lw(r,(lab,ra)) => 
           if (RS.member(spills, r) andalso RS.member(spills, ra))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, ra), M.Lw(tmpReg2, (lab, tmpReg1)), M.Move(r,
             tmpReg2)]
                end
           else if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Lw(tmpReg1, (lab, f ra)), M.Move(r, tmpReg1)]
                end
           else if (RS.member(spills, ra))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, ra), M.Lw(f r, (lab, tmpReg1))]
                end
           else [M.Lw(f r, (lab, f ra))]

       | M.Sw(r,(lab,ra)) =>
           if (RS.member(spills, r) andalso RS.member(spills, ra))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Move(tmpReg2, ra), M.Sw(tmpReg1, (lab, tmpReg2))]
                end
           else if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Sw(tmpReg1, (lab, f ra))]
                end
           else if (RS.member(spills, ra))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, ra), M.Sw(f r, (lab, tmpReg1))]
                end
           else [M.Sw(f r, (lab, f ra))]

       | M.Branchz(i,r,lab) => 
           if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Branchz(i, tmpReg1, lab)]
                end
           else [M.Branchz(i, f r, lab)]

       | M.Branchu(i,r1,r2,lab) => 
           if (RS.member(spills, r1) andalso RS.member(spills, r2))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r1), M.Move(tmpReg2, r2), M.Branchu(i, tmpReg1,
             tmpReg2, lab)]
                end
           else if (RS.member(spills, r1))
           then let val tmpReg1 = M.newReg() in 
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r1), M.Branchu(i, tmpReg1, f r2, lab)]
                end
           else if (RS.member(spills, r2))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r2), M.Branchu(i, f r1, tmpReg1, lab)]
                end
           else [M.Branchu(i, f r1, f r2, lab)]

       | M.Branch(i,r1,r2,lab) => 
           if (RS.member(spills, r1) andalso RS.member(spills, r2))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r1), M.Move(tmpReg2, r2), M.Branch(i, tmpReg1,
             tmpReg2, lab)]
                end
           else if (RS.member(spills, r1))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r1), M.Branch(i, tmpReg1, f r2, lab)]
                end
           else if (RS.member(spills, r2))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r2), M.Branch(i, f r1, tmpReg1, lab)]
                end
           else [M.Branch(i, f r1, f r2, lab)]

       | M.J(lab) => [M.J lab]
       | M.Jal(lab) => [M.Jal lab]
       | M.Jr(r,also) => 
           if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Jr(tmpReg1, map f also)]
                end
           else [M.Jr(f r, map f also)]
       | M.Jalr(r1,r2,use,def) => 
           if (RS.member(spills, r1) andalso RS.member(spills, r2))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r2), M.Jalr(tmpReg2, tmpReg1, map f use, map f
             def), M.Move(tmpReg2, r1)]
                end
           else if RS.member(spills, r2)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r2), M.Jalr(f r1, tmpReg1, map f use, map f def)]
                end
           else if RS.member(spills, r1)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Jalr(tmpReg1, f r2, map f use, map f def), M.Move(r1, tmpReg1)]
                end
           else [M.Jalr(f r1, f r2, map f use, map f def)]

       | M.Syscall => [M.Syscall]
       | M.Nop => [M.Nop]
   end
      

   fun make_end (instrL : M.funcode, index) =
     case instrL of
       (l, instrs)::[] => if (!index) = ~1 then [(l, instrs)] else [(l,
       (M.Arithi(M.Addi, M.reg "$sp", M.reg "$sp", M.immed ((!index + 1)*4)))::instrs)]
     | h::t => h :: make_end (t,index)

   fun alloc(instrL as ((funlab,block)::rest) : M.funcode) = 
   let 
       val nonSpillL : M.reg list ref = ref []
       fun spillCost x = if(List.exists (fn reg => reg = x) (!nonSpillL)) then
           Color.spillCostInfinity else 1
       
       val spillL : (int * M.reg) list ref = ref []
       val index = ref (~1)
       val ig = Liveness.interference_graph instrL
       val movegraph = IG.newGraph()
       val _ = app (fn (_,l) => app (getmove movegraph) l) instrL
       val _ = print "###### Move graph\n"
       val _ = Liveness.printgraph print movegraph
       val palette = M.list2set (M.reg"$ra"::M.callerSaved @
       M.calleeSaved)
       val coloring = Color.color {interference = ig, moves=movegraph, 
	                  spillCost = spillCost, palette=palette}
       val _ = Color.verify{complain=ErrorMsg.impossible, func=instrL, 
                            spillCost=spillCost, palette=palette, 
                            coloring=coloring}
       val _ = print "Register Allocation verified.\n"
       val {alloc,spills} = coloring
       val _ = (print "Spills: "; 
                RS.app (fn r => (print (M.reg2name r); print " ")) spills;
	        print "\n")
       
       val instrL = List.map (fn (l,instrs) => (l,List.map (M.rename_regs alloc) instrs)) instrL
       val instrL = List.map (fn (l,instrs) => (l, flat(List.map (rename_spills
       (spills, spillL, index, nonSpillL)) instrs))) instrL
       
       val instrL = (List.map (fn (l,instrs) => (l, flat(List.map (after_spills
       (spills, spillL, index)) instrs))) instrL); 

     val spillset = ref spills
     val finalInstrL = ref instrL
   in
     while not (RS.isEmpty (!spillset)) do
     (
     
        let 
          val ig = Liveness.interference_graph (!finalInstrL)
          val movegraph = IG.newGraph()
          val _ = app (fn (_, l) => app (getmove movegraph) l) (!finalInstrL)
          val _ = print "###### Move graph\n"
          val _ = Liveness.printgraph print movegraph
          val coloring = Color.color {interference = ig, moves=movegraph, 
	                  spillCost = spillCost, palette=palette}
          val _ = Color.verify{complain=ErrorMsg.impossible, func=(!finalInstrL), 
                               spillCost=spillCost, palette=palette, 
                               coloring=coloring}
          val {alloc, spills} = coloring
          val _ = (print "Spills: "; 
                RS.app (fn r => (print (M.reg2name r); print " ")) spills;
	        print "\n")
          val instrL = List.map (fn (l,instrs) => (l,List.map (M.rename_regs
          alloc) instrs)) (!finalInstrL)
          val instrL = (List.map (fn (l,instrs) => (l, flat(List.map (rename_spills
              (spills, spillL, index, nonSpillL)) instrs))) instrL) 
          val instrL = (List.map (fn (l,instrs) => (l, flat(List.map (after_spills
              (spills, spillL, index)) instrs))) instrL);

          val _ = print("hi\n")          

        in
          finalInstrL := instrL  ;
          spillset := spills
        end 
     );
     let val ((funlab, block)::rest) = !finalInstrL; val st_instrL = if (!index)
     = ~1 then !finalInstrL 
          else (funlab, (M.Arithi(M.Addi, M.reg "$sp", M.reg "$sp", M.immed (~((!index + 1)*4)))) :: block)::rest
         val ed_instrL = make_end (st_instrL, index)
     in
        ed_instrL
     end
  end
end
