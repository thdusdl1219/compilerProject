signature CODEGEN = sig
  val codegen : Absyn.prog -> X86.program
end

structure Codegen :> CODEGEN = 
  struct

    structure A = Absyn
    structure M = X86
    structure E = ErrorMsg

  local
    (* Last label emitted. *)
    val last_lab = ref (NONE: M.lab option)
    (* List of instructions being generated in the current codeblock.
     * For efficiency, this list is kept in reverse order. *)
    val ilist = ref (nil:M.instruction list)
    (* List of codeblocks generated, in reverse order *)
    val blist = ref (nil:M.codeblock list)
    (* List of functions generated, in reverse order *)
    val flist = ref (nil:M.funcode list)
  in
    (* Here's the protocol for using these functions,
       described as a regular expression:

       init_lists ( (emit_label emit* )+ finish_fun )* finish_prog
    *)

    fun init_lists () = (ilist := nil; blist := nil; flist := nil; 
                         last_lab := NONE)

    fun finish_block () = 
           case (!last_lab, !ilist)
            of (NONE, nil) => ()
             | (NONE, _) => E.impossible "No start label"
             | (SOME lab, il) => 
                  (blist := (lab, rev il) :: (!blist);
                   ilist := nil;
                   last_lab := NONE)

    fun finish_fun () = (finish_block();
                         flist := (rev(!blist))::(!flist);
                         blist := nil)

    fun finish_prog() = 
	   case !last_lab
             of SOME _ => E.impossible "finish_prog without finish_fun"
              | NONE => rev(!flist) before flist := nil

    (* Append an instruction to the list of generated instructions. *)
    fun emit i = ilist := i::(!ilist)

    fun emit_label l = (finish_block(); last_lab := SOME l)
  end

    val newline_lab = M.thislab "NL"    
    val intprint_lab = M.thislab "IP"

    (* Memory management functions. *) 

    val heap_size = 32000 (* in bytes -- should be less than 64KB *)
    val init_lab = M.thislab("init")
    val alloc_lab = M.thislab("alloc")

    (* Emits a call to alloc, to allocate 'size' bytes, and put the 
     * returned address in 'ret_reg'. *) 
    fun emit_alloc_call (size:M.immed, ret_reg:M.reg) = 
      let val arg_tmp = M.newReg()
      in
        (
         emit (M.Li(arg_tmp, size));
         emit (M.Push(arg_tmp));
         emit (M.Jal(alloc_lab));
         emit(M.Arithi(M.Addi, M.reg "%esp", M.reg "%esp", M.immed 4));
         emit (M.Move(ret_reg, M.reg("%eax")))
         )
      end
          
    fun emit_alloc_func () =
      let val eax_temp = M.newReg(); val li_temp = M.newReg(); val ebx_temp = M.newReg(); val ecx_temp = M.newReg() ; val edx_temp = M.newReg()
      in
      (emit_label alloc_lab;
       emit (M.Push(M.reg("%ebp")));
       emit (M.Move(M.reg("%ebp"), M.reg("%esp")));
       emit (M.Arithi(M.Addi, M.reg("%esp"), M.reg("%esp"), M.immed (~4)));
       emit (M.Lw(M.reg("%eax"), (M.immed 8, M.reg("%ebp"))));
       emit (M.Sw(M.reg("%eax"), (M.immed 0, M.reg("%esp"))));
       emit (M.Jal(M.thislab "malloc"));
       emit_label (M.thislab "alloc.epilog"); 
       emit (M.Leave);
       emit (M.Ret);
       finish_fun())
      end

    fun emit_init_func () = 
        (emit_label (M.thislab "main");
(*         emit (M.Push(M.reg("%ebp")));
         emit (M.Move(M.reg("%ebp", "%esp"))); *)
         emit (M.Jal(M.thislab "_main"));
         emit_label (M.thislab "main.epilog");
         emit (M.Ret);
         finish_fun())

    fun emit_printint_func() =
      let val done_lab = M.freshlab()
      in
      (emit_label (M.thislab "_printint");
       emit (M.Push(M.reg("%ebp")));
       emit (M.Move(M.reg("%ebp"), M.reg("%esp")));
       emit (M.Arithi(M.Addi, M.reg("%esp"), M.reg("%esp"), M.immed (~8)));
       emit (M.Lw(M.reg("%eax"), (M.immed 8, M.reg("%ebp"))));
       emit (M.Sw(M.reg("%eax"), (M.immed 4, M.reg("%esp"))));
       emit (M.La(M.reg("%eax"), intprint_lab));
       emit (M.Sw(M.reg("%eax"), (M.immed 0, M.reg("%esp"))));
       emit (M.Jal(M.thislab "printf"));
       (* Print a newline after the integer, for clarity. *)
       emit_label (M.thislab "_printint.epilog");
       emit (M.Leave);
       emit (M.Ret);
       finish_fun())
      end

    datatype value = Reg of M.reg | Lab of M.lab

    (* A function environment maps: A.id -> M.lab * A.func *)

    fun fun_label id = M.thislab("_" ^ Symbol.name id)

    fun add_fun_to_env (id,env) = 
            Symbol.enter (env, id, Lab(fun_label id))

    (* A variable environment maps: A.id -> M.reg *)

    fun fun2mips_arith_op A.Add = M.Add
      | fun2mips_arith_op A.Sub = M.Sub
      | fun2mips_arith_op A.Mul = M.Mul
      | fun2mips_arith_op _      = E.impossible "Arith op expected"

    (* Remove Pos and Constrain, to simplify pattern matching. *)
    fun strip(A.Pos(_,e))     = strip e
      | strip(A.Constrain(e,_)) = strip e
      | strip(A.Op(oper,el))  = A.Op(oper, map strip el)
      | strip(A.Tuple(el))    = A.Tuple(map strip el)
      | strip(A.Proj(i,e))    = A.Proj(i,strip e)
      | strip(A.If(e1,e2,e3)) = A.If(strip e1, strip e2, strip e3)
      | strip(A.Call(e1,e2))  = A.Call(strip e1, strip e2)
      | strip(A.While(e1,e2)) = A.While(strip e1, strip e2)
      | strip(A.Let(i,e1,e2)) = A.Let(i,strip e1, strip e2)
      | strip(e)                = e
  
  val data_size = 4

  fun make_expl env expl rega addr = 
    case expl of 
         h::t => let val r1 = gen_exp env h in emit(M.Sw(r1, (M.immed addr, rega))); make_expl env t rega (addr+data_size) end
       | [] => () 

    (* gen_exp: generates code for one expression 
     *    inputs: env:  environment 
     *            exp:  the expression to emit code for
     *    output: M.reg -- if ret value is <>, we return r0
     *)
  and gen_exp env : A.exp -> M.reg = 
  let
      fun gen (A.Id id) =       
              (case Symbol.look (env, id) of
                 SOME (Reg r) => r
               | SOME (Lab lab) => 
	               let val r = M.newReg()
	                in emit (M.La(r, lab));
	                   r
	               end
               | NONE => E.impossible ("Can't find " ^ Symbol.name id))

           (* IMPLEMENT ME! *)
           (* int 32bit consider... *)
        | gen (A.Int i) =
          let val r = M.newReg()
            in emit (M.Li(r, M.immed i));
              r
            end
        | gen (A.Op (oper, expl)) = 
          (case oper of
                A.Ref => let val r = M.newReg(); val r1 = gen_exp env (hd(expl))
                        in emit_alloc_call(M.immed data_size, r);
                           emit (M.Sw(r1, (M.immed 0, r)));
                           r
                        end
              | A.Get => let val r1 = gen_exp env (hd expl); val r = M.newReg()
                        in emit (M.Lw(r, (M.immed 0, r1))); r end
              | A.Set => let val r1 = gen_exp env (hd expl); 
                val r2 = gen_exp env (List.nth(expl, 1)) in 
                  emit (M.Sw(r2, (M.immed 0, r1))); r2 
                end (* I don't know what is return.. *)
              | A.Add => 
                  let val mop = fun2mips_arith_op(oper); val r = M.newReg() 
                    in (case (List.nth(expl,0), List.nth(expl, 1)) of
                             (_, A.Int i) => if( i > 65535) then (emit(M.Arith3(mop, r, gen_exp env (List.nth(expl, 0)), gen_exp env (List.nth(expl, 1)))); r) else (emit(M.Arithi(M.Addi, r, gen_exp env (List.nth(expl, 0)), M.immed i));r)
                           | (A.Int i, _) => if( i > 65535) then (emit(M.Arith3(mop, r, gen_exp env (List.nth(expl, 0)), gen_exp env (List.nth(expl, 1)))); r) else (emit(M.Arithi(M.Addi, r, gen_exp env (List.nth(expl, 1)), M.immed i)); r) 
                           | (_, _) => (emit(M.Arith3(mop,r,gen_exp env (List.nth(expl, 0)),gen_exp env (List.nth(expl, 1))));r )
                      )
                    end
              | A.Sub => 
                  let val mop = fun2mips_arith_op(oper); val r = M.newReg()
                  in if(List.nth(expl, 0) = A.Int 0) 
                       then
                         (case List.nth(expl, 1) of
                               A.Int i => (emit(M.Li(r, M.immed (i * (~1)))); r) 
                             | _ => (emit(M.Arith2(M.Neg, r, gen_exp env (List.nth(expl, 1)) )); r) 
                        )
                       else
                         (case (List.nth(expl, 0), List.nth(expl, 1)) of
                               (A.Int i, A.Int i2) => (emit(M.Li(r,M.immed (i - i2)));r)
                             | (_, A.Int i) => (emit(M.Arithi(M.Addi, r, gen_exp env (List.nth(expl, 0)), M.immed (i*(~1))));r)
                             | (_, _) =>  (emit(M.Arith3(mop,r,gen_exp env (List.nth(expl, 0)),gen_exp env (List.nth(expl, 1)) )); r) 
                          )
                  end
              | A.Mul => 
                  let val mop = fun2mips_arith_op(oper); val r = M.newReg(); val r1 = gen_exp env (List.nth(expl, 0)); val r2 = gen_exp env (List.nth(expl, 1)); 
                    in
                      emit(M.Arith3(mop,r,r1,r2));
                      r
                    end
              | A.LT => 
                  let val r = M.newReg(); val r1 = gen_exp env (List.nth(expl, 0)); val r2 = gen_exp env (List.nth(expl, 1)); val else_lab = M.freshlab(); val done_lab = M.freshlab()
                    in
                      emit(M.Branch(M.Lt, r1, r2, else_lab)); 
                      emit_label(M.freshlab());
                      emit(M.Li(r, M.immed(0)));
                      emit(M.J(done_lab));
                      emit_label(else_lab);
                      emit(M.Li(r, M.immed(1)));
                      emit_label(done_lab);
                      r
                    end
              | A.Eq => 
                  let val r = M.newReg(); val r1 = gen_exp env (List.nth(expl, 0)) ; val else_lab = M.freshlab(); val r2 = gen_exp env (List.nth(expl, 1)); val done_lab = M.freshlab()
                    in 
                      emit(M.Branch(M.Eq, r1, r2, else_lab));
                      emit_label(M.freshlab());
                      emit(M.Li(r, M.immed(0)));
                      emit(M.J(done_lab));
                      emit_label(else_lab);
                      emit(M.Li(r, M.immed(1)));
                      emit_label(done_lab);
                      r
                    end
          )
        | gen (A.Tuple expl) = 
          let val r = M.newReg() 
            in if (length(expl) = 0) then (emit(M.Li(r, M.immed 0)); r)
              else (emit_alloc_call(M.immed (data_size * length(expl)), r);
              make_expl env expl r 0; r) end
        | gen (A.Proj (i, exp)) = 
          let val r1 = gen_exp env exp; val r = M.newReg()
            in emit(M.Lw(r, (M.immed (i * data_size), r1))); r end

        | gen (A.If( A.Op(A.LT, r1::[r2]), exp1, exp2)) =
          let 
            val else_lab = M.freshlab()
            val r = M.newReg()
            val done_lab = M.freshlab()
          in
            emit(M.Branch(M.Ge, gen_exp env r1, gen_exp env r2, else_lab));
            emit_label(M.freshlab());
            emit(M.Move(r, gen_exp env exp1));
            emit(M.J(done_lab));
            emit_label(else_lab);
            emit(M.Move(r, gen_exp env exp2));
            emit_label(done_lab);
            r
          end

        | gen (A.If( A.Op(A.Eq, r1::[r2]), exp1, exp2)) =
          let 
            val else_lab = M.freshlab()
            val r = M.newReg()
            val done_lab = M.freshlab()
          in
            emit(M.Branch(M.Ne, gen_exp env r1, gen_exp env r2, else_lab));
            emit_label(M.freshlab());
            emit(M.Move(r, gen_exp env exp1));
            emit(M.J(done_lab));
            emit_label(else_lab);
            emit(M.Move(r, gen_exp env exp2));
            emit_label(done_lab);
            r
          end

        | gen (A.If (exp1, exp2, exp3)) =
          let val else_lab = M.freshlab (); val r = M.newReg(); val done_lab = M.freshlab ()
          in (
            case (exp2, exp3) of
                (* (A.Int(0), A.Int(1)) =>
                  let val zeroR = M.newReg ()
                  in
                    (emit(M.Li(zeroR, M.immed 0));
                     emit(M.Arith3(M.Seq, r, gen_exp env exp1, zeroR)); r)
                  end *)
                 (A.Int(1), A.Int(0)) => (emit(M.Move(r, gen_exp env exp1)); r)
        (*       | (A.If(if_exp1, A.Int(1), A.Int(0)), A.Int(0)) =>
                  
               | (A.Int(1), A.If(exp2, A.Int(1), A.Int(0))) => *)
               | _ =>
                  (emit(M.Branchz(M.Eq, gen_exp env exp1, else_lab)); 
                      emit_label(M.freshlab());

                  emit(M.Move(r, gen_exp env exp2)) ; emit(M.J(done_lab)); emit_label (else_lab) ; emit(M.Move(r, gen_exp env exp3)) ; emit_label (done_lab); r)
            ) 
          end

        | gen (A.While(exp1, exp2)) = 
          let val check_lab = M.freshlab (); val done_lab = M.freshlab (); val r = M.newReg() 
            in 
              emit_label(check_lab);
              (case (exp1) of
                A.Op(A.LT, r1::[r2]) => emit(M.Branch(M.Ge, gen_exp env r1, gen_exp env r2, done_lab))
                | A.Op(A.Eq, r1::[r2]) => emit(M.Branch(M.Ne, gen_exp env r1, gen_exp env r2, done_lab))
                | _ => emit(M.Branchz(M.Eq, gen_exp env exp1, done_lab))
              );
              emit_label(M.freshlab());
              emit(M.Move(r, gen_exp env exp2));
              emit(M.J(check_lab)); 
              emit_label(done_lab); r 
            end (* I don't know what is return... *)
        | gen (A.Call(A.Id fid, exp2)) =
            let val res_tmp = M.newReg(); val arg_tmp = M.newReg()
            in
              emit(M.Move(arg_tmp, gen_exp env exp2));
              emit(M.Push(arg_tmp));
              (case Symbol.look (env, fid) of
                 SOME (Reg r) =>
                  (emit (M.Jalr(M.reg ("%eip"), r, M.reg "%eax" :: ((M.reg "%eip") :: M.callerSaved), [])))
               | SOME (Lab lab) =>
                  emit (M.Jal lab)
               | NONE => E.impossible("Can't find " ^ Symbol.name fid);
               emit(M.Arithi(M.Addi, M.reg "%esp", M.reg "%esp", M.immed 4));
               emit(M.Move(res_tmp, M.reg("%eax")));
               res_tmp)
            end

        | gen (A.Call(exp1, exp2)) = 
          let val r1 = M.newReg(); val r2 = M.newReg(); val r3 = M.newReg(); val r = M.newReg() in
            emit(M.Move(r1, gen_exp env exp1)); emit(M.Move(r2, gen_exp env exp2)); emit(M.Move(r3, r1));
            emit(M.Push(r2)); emit(M.Jalr(M.reg "%eip", r3, M.reg "%eax" ::((M.reg "%eip") ::M.callerSaved), [])); 
            emit(M.Arithi(M.Addi, M.reg "%esp", M.reg "%esp", M.immed 4));
            emit(M.Move(r, M.reg "%eax")); r end (* jalr 잘 모르겠당. *) 
        | gen (A.Let(id1, exp1, exp2)) = 
          let val env = Symbol.enter(env, id1, Reg(gen_exp env exp1)) in gen_exp env
          exp2 end
        | gen _ = E.impossible "unimplemented translation"

     in gen
    end

    (* gen_func: generates code for one function
     *    inputs: fenv: functions environment
     *            func: the function to be generated
     *)


    fun gen_func (fenv, (f,x,t1,t2,exp)) = 
          (  (* IMPLEMENT ME! *)
           let val ebx_tmp = M.newReg(); val esi_tmp = M.newReg(); val edi_tmp = M.newReg(); val a0_tmp = M.newReg(); val fenv = Symbol.enter(fenv, x, Reg(a0_tmp))
            in 
           emit_label (fun_label f);
           emit (M.Push(M.reg("%ebp")));
           emit (M.Move(M.reg("%ebp"), M.reg("%esp")));
           emit (M.Move(ebx_tmp, M.reg "%ebx"));
           emit (M.Move(esi_tmp, M.reg "%esi"));
           emit (M.Move(edi_tmp, M.reg "%edi"));
           emit (M.Lw(a0_tmp, (M.immed 8, M.reg "%ebp"))); 
           emit (M.Move(M.reg "%eax", gen_exp fenv (strip exp)));
           emit (M.Move(M.reg "%ebx", ebx_tmp));
           emit (M.Move(M.reg "%esi", esi_tmp));
           emit (M.Move(M.reg "%edi", edi_tmp)); 
           emit_label (Symbol.symbol(Symbol.name (fun_label f) ^ ".epilog"));
           emit (M.Leave);
           emit (M.Ret);
           finish_fun ()
            end
          )

    (* codegen: generates code for a program 
     *    input:  A.prog
     *    output: M.program 
     *)
    fun codegen (fundec_list :A.prog) = 
      (* 1. Generate functions-env
       * 2. Emit runtime-system functions
       * 3. For each function, generate code for it
       *)
      let
        val fenv = foldl add_fun_to_env Symbol.empty 
                     (map #1 Absyn.externals @ map (#1 o #2) fundec_list)
      in
         init_lists(); 
         emit_init_func();
         emit_alloc_func();
         emit_printint_func();
         List.app (fn (_,fd) => gen_func (fenv, fd)) fundec_list;          
         ([(newline_lab,"\\n"), (intprint_lab, "%d\\n")], finish_prog())
      end
  end
