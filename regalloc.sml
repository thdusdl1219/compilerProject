signature REGALLOC = sig
(* alloc(f) returns mips code for function f that has all the temporaries
    replaced by mips regs and the load/store of spilled temps taken care of
*)
 val alloc : X86.funcode ->  X86.funcode 
end

structure RegAlloc :> REGALLOC =
struct
   structure M = X86
   structure RS = X86.RegSet
   structure IG = Liveness.IG
   structure RT = M.RegTb
   

   fun getmove g (M.Move(r1,r2)) = IG.mk_edge g {from=r2,to=r1} 
     | getmove g _ = ()

 fun print_list list : unit =
   (app (fn (reg, reg2) => (print " "; print (M.reg2name reg))) list; print "\n")
   fun print_set set : unit =
      (RS.app (fn reg => (print " "; print (M.reg2name reg))) set; print "\n")
   fun flat xs = List.foldr op@ [] xs

  (* after spill renaming, make lw and sw instruction in move instruction (== simple spill algorithm) *)
   fun after_spills (spills, spillL, index) = 
     fn M.Move(rd, rs) =>
       if (RS.member(spills, rd) andalso RS.member(spills, rs)) then
         [M.Move(rd, rs)]
       else
       if(RS.member(spills, rd)) then
          (case List.find (fn (ind, reg) => reg = rd) (!spillL) of
                SOME((ind, reg)) => 
          [M.Sw(rs, (M.immed (ind * 4), M.reg "%esp"))]
              | NONE => 
          (index := !index + 1; spillL := ((!index, rd) :: !spillL); 
          [M.Sw(rs, (M.immed (!index * 4), M.reg "%esp"))])) 
       else (
          if RS.member(spills, rs) then
            (case List.find (fn (ind, reg) => reg = rs) (!spillL) of
                  SOME((ind, reg)) =>  (
                  [M.Lw(rd, (M.immed (ind * 4), M.reg "%esp"))])
                | NONE => ErrorMsg.impossible ( "register isn't in spillL : " ^
                   M.reg2name rs ^ " : " ^ M.reg2name rd)
              )  
          else [M.Move(rd, rs)])
      | others => [others]

  (* make c = a op b to ta := a, tb := c, tc = ta op tb , c := tc *)
   fun rename_spills (spills, spillL, index, nonSpillL) = 
      fn M.Move(rd, rs) => 
        if (RS.member(spills, rd) andalso RS.member(spills, rs)) then
          let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
            nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)); 
          [M.Move(tmpReg1, rs), M.Move(tmpReg2, tmpReg1), M.Move(rd, tmpReg2)]
          end
        else
          [M.Move(rd, rs)]
                   
       | M.Arith1(i, r) =>
           if RS.member(spills, r)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Arith1(i, tmpReg1), M.Move(r, tmpReg1)]
                end
           else
             [M.Arith1(i, r)]
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
             [M.Move(tmpReg1, rs), M.Arith2(i,  rd, tmpReg1)]
           end
           else if RS.member(spills, rd)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Arith2(i, tmpReg1,  rs), M.Move(rd, tmpReg1)]
           end
           else [M.Arith2(i,  rd,  rs)]

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
             [M.Move(tmpReg1, rs), M.Arith3(i, tmpReg1, tmpReg1,  rt),
             M.Move(rd, tmpReg1)]
                end
           else if (RS.member(spills, rt) andalso RS.member(spills, rd))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rt), M.Arith3(i, tmpReg1,  rs, tmpReg1),
             M.Move(rd, tmpReg1)]
                end
           else if (RS.member(spills, rs) andalso RS.member(spills, rt))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg2, rs), M.Move(tmpReg1, rt), M.Arith3(i,  rd, tmpReg2,
           tmpReg1)]
                end
           else if RS.member(spills, rs)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rs), M.Arith3(i,  rd, tmpReg1,  rt)]
                end
           else if RS.member(spills, rt)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, rt), M.Arith3(i,  rd,  rs, tmpReg1)]
                end
           else if RS.member(spills, rd)
           then let val tmpReg1 = M.newReg() in 
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Arith3(i, tmpReg1,  rs,  rt), M.Move(rd, tmpReg1)]
                end
           else [M.Arith3(i,  rd,  rs,  rt)]
           
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
             [M.Move(tmpReg1, rs), M.Arithi(i,  rt, tmpReg1, n)]
                end
           else if RS.member(spills, rt)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Arithi(i, tmpReg1,  rs, n), M.Move(rt, tmpReg1)]
                end
           else [M.Arithi(i,  rt,  rs, n)]

       | M.Li(r,n) => 
           if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Li(tmpReg1, n), M.Move(r, tmpReg1)]
                end
           else [M.Li( r, n)]
           
       | M.La(r,lab) => 
           if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.La(tmpReg1, lab), M.Move(r, tmpReg1)]
                end
           else [M.La( r, lab)]

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
             [M.Lw(tmpReg1, (lab,  ra)), M.Move(r, tmpReg1)]
                end
           else if (RS.member(spills, ra))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, ra), M.Lw( r, (lab, tmpReg1))]
                end
           else [M.Lw( r, (lab,  ra))]

       | M.Sw(r,(lab,ra)) =>
           if (RS.member(spills, r) andalso RS.member(spills, ra))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Move(tmpReg2, ra), M.Sw(tmpReg1, (lab, tmpReg2))]
                end
           else if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Sw(tmpReg1, (lab,  ra))]
                end
           else if (RS.member(spills, ra))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, ra), M.Sw( r, (lab, tmpReg1))]
                end
           else [M.Sw( r, (lab,  ra))]

       | M.Branchz(i,r,lab) => 
           if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Branchz(i, tmpReg1, lab)]
                end
           else [M.Branchz(i,  r, lab)]

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
             [M.Move(tmpReg1, r1), M.Branch(i, tmpReg1,  r2, lab)]
                end
           else if (RS.member(spills, r2))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r2), M.Branch(i,  r1, tmpReg1, lab)]
                end
           else [M.Branch(i,  r1,  r2, lab)]

       | M.J(lab) => [M.J lab]
       | M.Jal(lab) => [M.Jal lab]
       | M.Jr(r,also) => 
           if (RS.member(spills, r))
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Jr(tmpReg1, also)]
                end
           else [M.Jr(r, also)]
       | M.Jalr(r1,r2,use,def) => 
           if (RS.member(spills, r1) andalso RS.member(spills, r2))
           then let val tmpReg1 = M.newReg(); val tmpReg2 = M.newReg() in
             nonSpillL := tmpReg1 :: (tmpReg2 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r2), M.Jalr(tmpReg2, tmpReg1, use, 
             def), M.Move(r1,tmpReg2)]
                end
           else if RS.member(spills, r2)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r2), M.Jalr(r1, tmpReg1, use, def)]
                end
           else if RS.member(spills, r1)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Jalr(tmpReg1, r2, use, def), M.Move(r1, tmpReg1)]
                end
           else [M.Jalr(r1, r2, use, def)]

       | M.Syscall => [M.Syscall]
       | M.Nop => [M.Nop]
       | M.Leave => [M.Leave]
       | M.Ret => [M.Ret]
       | M.Push(r) =>
           if RS.member(spills, r)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Move(tmpReg1, r), M.Push(tmpReg1)]
                end
           else
             [M.Push(r)]
      | M.Pop(r) =>
           if RS.member(spills, r)
           then let val tmpReg1 = M.newReg() in
             nonSpillL := (tmpReg1 :: (!nonSpillL)) ;
             [M.Pop(tmpReg1), M.Move(r, tmpReg1)]
                end
           else
             [M.Pop(r)]
     | M.Branch2(c, l) => [M.Branch2(c, l)]

      
   (* make restore $sp register in function epilog *)
   fun make_end (instrL : M.funcode, index) =
     case instrL of
       (l, instrs)::[] => if (!index) = ~1 then [(l, instrs)] else [(l,
       instrs)]
     | h::t => h :: make_end (t,index)
   fun make_first (insrL : M.instruction list) (first:M.instruction list) index =
     case insrL of
       instr :: t =>
        (case instr of
           M.Move(r1, r2) =>
            (if(r1 = M.reg "%ebp" andalso r2 = M.reg "%esp")
            then
              first @ (instr :: ((M.Arithi(M.Addi, M.reg "%esp", M.reg "%esp", M.immed (~((!index + 1)*4)))) :: t))
            else 
              make_first t (first @ [instr]) index)
         | _ => make_first t (first @ [instr]) index)
    | [] => first

   fun alloc(instrL as ((funlab,block)::rest) : M.funcode) = 
   let
     val index = ref (~1)
     (* make instruction that was made by 2nd general spill method *)
     fun alloc_sub (instrL as ((funlab, block) :: rest)) =
     let 
       val nonSpillL : M.reg list ref = ref []
       fun spillCost x = if(List.exists (fn reg => reg = x) (!nonSpillL)) then
           Color.spillCostInfinity else 1
       
       val spillL : (int * M.reg) list ref = ref []
       val ig = Liveness.interference_graph instrL
       val movegraph = IG.newGraph()
       val _ = app (fn (_,l) => app (getmove movegraph) l) instrL

      (*val _ = print "###### Move graph\n"
       val _ = Liveness.printgraph print movegraph*)
       val palette = M.list2set (M.reg"%eip"::M.callerSaved @

       M.calleeSaved)
       val coloring = Color.color {interference = ig, moves=movegraph, 
	                  spillCost = spillCost, palette=palette}
       val _ = Color.verify{complain=ErrorMsg.impossible, func=instrL, 
                            spillCost=spillCost, palette=palette, 
                            coloring=coloring}
       (*val _ = print "Register Allocation verified.\n"*)
       val {alloc,spills} = coloring
       (*val _ = (print "Spills: "; 
                RS.app (fn r => (print (M.reg2name r); print " ")) spills;
	        print "\n")*)
    in 
      if(RS.isEmpty spills) then (instrL, alloc)
      else
        let
          val instrL = List.map (fn (l,instrs) => (l, flat(List.map (rename_spills
            (spills, spillL, index, nonSpillL)) instrs))) instrL
       
          val instrL = (List.map (fn (l,instrs) => (l, flat(List.map (after_spills
            (spills, spillL, index)) instrs))) instrL);
    (* this is loop function when spillset isn't empty. If spillset is empty, it finish. *)
      fun loop spillset (finalinstrL, finalAlloc) =
       if(RS.isEmpty (spillset)) then (finalinstrL, finalAlloc)
       else 
        let 
          val ig = Liveness.interference_graph (finalinstrL)
          val movegraph = IG.newGraph()
          val _ = app (fn (_, l) => app (getmove movegraph) l) (finalinstrL)
          val coloring = Color.color {interference = ig, moves=movegraph, 
	                  spillCost = spillCost, palette=palette}
          val _ = Color.verify{complain=ErrorMsg.impossible, func=(finalinstrL), 
                               spillCost=spillCost, palette=palette, 
                               coloring=coloring}
          val {alloc, spills} = coloring
        in
          if(RS.isEmpty spills) then (finalinstrL, alloc)
          else (
            let
              val instrL = (List.map (fn (l,instrs) => (l, flat(List.map (rename_spills
                (spills, spillL, index, nonSpillL)) instrs))) (finalinstrL))
              val instrL = (List.map (fn (l,instrs) => (l, flat(List.map (after_spills
                (spills, spillL, index)) instrs))) instrL);
              (*val _ = print("hi\n")          *)
            in
              loop spills (instrL, alloc)
            end )
        end
        in
          loop spills (instrL, alloc)
        end
    end
    in
      let
        val (finalinstr, finalalloc) = alloc_sub instrL
        val finalinstrL = List.map (fn (l, instrs) => (l, List.map (M.rename_regs finalalloc) instrs)) (finalinstr) 
      in
       (* make $sp := $sp - spill registers number *)
        let 
          val ((funlab, block)::rest) = finalinstrL; 
          val st_instrL = 
            if (!index) = ~1 then finalinstrL 
            else (funlab, make_first block [] index)::rest
          val ed_instrL = make_end (st_instrL, index)
        in
          ed_instrL
        end
      end
    end
end
