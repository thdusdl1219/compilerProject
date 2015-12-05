signature TYPECHECK =
sig
  val tc : Absyn.prog -> unit
  (* if there are errors, these are reported through ErrorMsg.error *)

  val sub: Absyn.tp * Absyn.tp -> bool
  val join: (string->unit) -> Absyn.tp * Absyn.tp -> Absyn.tp
  val tc_exp : (Absyn.tp Symbol.table) -> (int*int) -> Absyn.exp -> Absyn.tp
  val tp2string : Absyn.tp -> string 
end

structure TypeCheck :> TYPECHECK =
struct

 structure A = Absyn
     
 fun list2string nil = ""
   | list2string [t] = t
   | list2string (h::t) = h ^ "," ^ list2string t

 fun tp2string A.Inttp = "int"
   | tp2string (A.Tupletp tps) = "<" ^ (list2string (map tp2string tps)) ^ ">"
   | tp2string (A.Arrowtp (tp1, tp2)) = tp2string tp1 ^ " -> " ^ tp2string tp2
   | tp2string (A.Reftp tp) = tp2string tp ^ " ref"

 type context = A.tp Symbol.table

 exception UNIMPLEMENTED

  fun list_compare f (l1, l2) = 
    case (l1, l2) of
         ([], []) => true
       | (h1::t1, h2::t2) => if(f (h1, h2)) then list_compare f (t1, t2) else false
       | _ => false
   fun list_compare2 (l1, l2) = 
     case (l1, l2) of
          ([], []) => true
        | (h1::t1, []) => true
        | (h1::t1, h2::t2) => if(h1 = h2) then list_compare2 (t1, t2) else (false )
        | _ => ( false)

(* subtyping *)
 fun sub (t1,t2) = case (t1,t2) of 
                        (A.Inttp, A.Inttp) => (true)
                      | (A.Tupletp a1, A.Tupletp b1) => (list_compare sub (a1, b1)
                        orelse list_compare2 (a1, b1))
                      | (A.Arrowtp(ap1, ap2), A.Arrowtp(bp1, bp2)) => sub (bp1,
                        ap1) andalso sub (ap2, bp2)
                      | (A.Reftp ap1, A.Reftp bp1) => sub (ap1, bp1) andalso sub
                        (bp1, ap1)
                      | _ => false
                   

 fun check_sub pos (tp1, tp2) = 
   if sub (tp1, tp2) then ()
   else ErrorMsg.error (pos, tp2string(tp1) ^ " isn't subtype of " ^ tp2string(tp2))


(* subtype join *)
 fun join complain (t1,t2) : A.tp = 
   case join_inner complain (t1,t2) of SOME n => n
      | NONE => ((ErrorMsg.error ((0,0),(tp2string(t1) ^ " do not join " ^ tp2string(t2)))); A.Tupletp([]))

  and join_inner complain (t1,t2) : A.tp option = 
   case (t1, t2) of
        (A.Inttp, A.Inttp) => SOME A.Inttp
      | (A.Tupletp tlist1, A.Tupletp tlist2) => SOME (A.Tupletp (joinlist complain (tlist1, tlist2) []))
      | (A.Arrowtp (tp1, tp2), A.Arrowtp (tp3, tp4)) => 
          (case join_inner complain(tp2, tp4) of 
               SOME n => 
               (case meet_inner complain (tp1, tp3) of
                     SOME m => SOME(A.Arrowtp(m,n))
                   | NONE => NONE)
             | NONE => NONE )
      | (A.Reftp tp1, A.Reftp tp2) => if (tp1 = tp2) then SOME (A.Reftp tp1) else
        (complain "t1 and t2 do not join1"; NONE) 
      | _ => (complain ((tp2string t1) ^ " , " ^ (tp2string t2) ^ "isn't join");NONE)

 and joinlist complain (tl1, tl2) acc =
  case (tl1, tl2) of
       (h1::t1, h2::t2) => 
        (case join_inner complain (h1,h2) of
              SOME n => (joinlist complain (t1, t2) (acc @ [n]))
            | NONE => acc)
     | (h1::t1, []) => (acc)
     | ([], h2::t2) => (acc)
     | ([], []) => acc

 and meet complain (t1, t2) : A.tp =
      case join_inner complain (t1,t2) of SOME n => n
        | NONE => ((ErrorMsg.error ((0,0),(tp2string(t1) ^ " do not join " ^ tp2string(t2)))); A.Tupletp([]))

 and meet_inner complain (t1, t2) =
  case (t1, t2) of
       (A.Inttp, A.Inttp) => SOME A.Inttp
    |  (A.Tupletp tlist1, A.Tupletp tlist2) => SOME(A.Tupletp (meetlist complain (tlist1, tlist2) []))
    |  (A.Arrowtp (tp1, tp2), A.Arrowtp (tp3, tp4)) => 
        (case join_inner complain(tp1, tp3) of 
               SOME n => 
               (case meet_inner complain (tp2, tp4) of
                     SOME m => SOME(A.Arrowtp(n,m))
                   | NONE => NONE)
             | NONE => NONE )
    |  (A.Reftp tp1, A.Reftp tp2) => if (tp1 = tp2) then SOME(A.Reftp tp1) else
      (complain "t1 and t2 do not meet"; NONE)
    |  _ => (complain "t1 and t2 do not meet"; NONE)

 and meetlist complain (tl1 , tl2) acc = 
   case (tl1, tl2) of
        (h1::t1, h2::t2) => 
          (case meet_inner complain (h1,h2) of 
                SOME n => (meetlist complain (t1,t2) (acc @ [n])) 
              | NONE => acc)
      | (h1::t1, []) => (meetlist complain (t1,[]) (acc @ [h1]))
      | ([], h2::t2) => (meetlist complain ([], t2) (acc @ [h2]))
      | ([], []) => acc
      

(* expression typing *)
 fun tc_exp ctxt pos e : A.tp =
   case e of
        A.Id id => let val typ = Symbol.look(ctxt, id) in 
          (case typ of
                SOME n => n
              | NONE => (ErrorMsg.error (pos, "Id : " ^ Symbol.name(id) ^ "is not in context"); A.Tupletp([])))end
      | A.Int n => A.Inttp
      | A.Op (oper, exps) =>
          (case oper of
                A.Ref => if(tl exps = []) then A.Reftp(tc_exp ctxt pos (hd exps)) else (ErrorMsg.error (pos, "explist is not null"); A.Reftp(tc_exp ctxt pos (hd exps)))
              | A.Get => if(tl exps = []) then let val exptype = tc_exp ctxt pos (hd exps) in 
                (case exptype of 
                      A.Reftp tp => tp
                    | _ => (ErrorMsg.error (pos, "exptype " ^ tp2string(exptype)
                    ^ "isn't Reftp"); A.Inttp)) end else (ErrorMsg.error (pos,
                    "explist is not null"); A.Inttp)
              | A.Set => let val exptype = tc_exp ctxt pos (hd exps) in 
                (case exptype of 
                      A.Reftp tp1 => let val exptype2 = tc_exp ctxt pos
                      (List.nth(exps, 1)) in if (sub(exptype2,tp1)) then
                        A.Tupletp([]) else (ErrorMsg.error (pos,tp2string(exptype2)^" isn't subtype of "^tp2string(tp1)); A.Tupletp([])) end
                    | _ => (ErrorMsg.error (pos, tp2string(exptype)^" is not Ref type"); A.Tupletp([]))) end
               | _ =>  
                   (case exps of
                         h1::h2::t => if (sub((tc_exp ctxt pos h1),A.Inttp) andalso sub((tc_exp ctxt pos h2),A.Inttp)) then A.Inttp else (ErrorMsg.error (pos, "exp type isn't Int"); A.Inttp)
                       | _ => (ErrorMsg.error (pos, "Op want two elements"); A.Inttp)))
        | A.Tuple exps => 
            (case exps of
                  h::t => let val A.Tupletp(typs) =
                  (tc_exp ctxt pos (A.Tuple(t))) in A.Tupletp((tc_exp ctxt pos h)::typs) end
                | [] => A.Tupletp([]))
        | A.Proj (n, exp) => let val exptype = tc_exp ctxt pos exp in 
          (case exptype of
                A.Tupletp tlist => if(length tlist > n andalso n >= 0) then List.nth(tlist,n) else  (ErrorMsg.error (pos, "index overflow!"); A.Inttp) 
              | _ => (ErrorMsg.error (pos, "You need Tupletp but type is "^tp2string(exptype)); A.Inttp)) end
        | A.Call (exp1, exp2) => let val exp1type = tc_exp ctxt pos exp1 in 
          (case exp1type of
                A.Arrowtp (tp1, tp2) => let val exptype = tc_exp ctxt pos exp2 in if(sub(exptype,tp1)) then tp2 else (ErrorMsg.error (pos, tp2string(exptype) ^" isn't subtype of "^tp2string(tp1)); tp2) end
              | _ => (ErrorMsg.error (pos, "exp1 type isn't function type!"); A.Inttp)) end
        | A.If (exp1, exp2, exp3) => let val exptype2 = tc_exp ctxt pos exp1 in if (sub(exptype2,A.Inttp)) then (let val exptype = tc_exp ctxt pos exp2 in (join print (exptype, tc_exp ctxt pos exp3)) end) else (ErrorMsg.error (pos, tp2string(exptype2)^" isn't subtype of "^tp2string(A.Inttp)); A.Tupletp([])) end
        | A.While (exp1, exp2) => let val exptype1 = tc_exp ctxt pos exp1 in if (sub(exptype1, A.Inttp)) then (
        let val exptype2 = tc_exp ctxt pos exp2 in if sub(exptype2,A.Tupletp([])) then A.Tupletp([]) else (ErrorMsg.error (pos, tp2string(exptype2) ^ " isn't subtype of "^tp2string(A.Tupletp([]))); A.Tupletp([])) end
        ) else (ErrorMsg.error (pos, tp2string(exptype1)^ " isn't subtype of "^tp2string(A.Inttp)); A.Tupletp([])) end
        | A.Let(x, exp1, exp2) => let val ctxt2 = Symbol.enter(ctxt, x, tc_exp ctxt pos exp1) in tc_exp ctxt2 pos exp2 end 
        | A.Constrain(exp1, ty1) => let val exptype = tc_exp ctxt pos exp1 in if(sub(exptype,ty1)) then ty1 else (ErrorMsg.error (pos, tp2string(exptype)^" isn't subtype of "^tp2string(ty1)); ty1)           end
        | A.Pos(pos2, exp1) => tc_exp ctxt pos2 exp1 




 fun tc_fundec ctxt ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
 let val ctxt' = Symbol.enter(ctxt,x,tp1)
     val tp = tc_exp ctxt' pos exp
  in (check_sub pos (tp, tp2))
 end 

 fun do_another_fun ((pos, fdec), ctxt) = let val (f, x, tp1, tp2, exp1) = fdec in 
   if (Symbol.look (ctxt, f) = NONE) then 
     (let val ctxt2 = Symbol.enter(ctxt, f, A.Arrowtp(tp1,tp2)) in (if(Symbol.name f = "main") then (if (sub(tp1,A.Inttp) andalso sub(tp2,A.Inttp)) then ctxt2 else (ErrorMsg.error(pos, "Main type error"); ctxt2)) else ctxt2) end) else (ErrorMsg.error(pos, "You have same function name"); ctxt) end

 fun build_global_context (fundecs) =
    foldl do_another_fun (Symbol.enter(Symbol.empty, Symbol.symbol("printint"), A.Arrowtp(A.Inttp, A.Tupletp([])))) fundecs

 fun tc (fundecs : A.prog)  = 
  let val ctxt = build_global_context(fundecs) 
   in if(Symbol.look(ctxt, Symbol.symbol("main")) = NONE) then ErrorMsg.error((0,0), "no main") else app (tc_fundec ctxt) fundecs
  end 
							     
end
