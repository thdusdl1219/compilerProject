(* SimpleFun interpreter
   Authors: David Walker, Yitzhak Mandelbaum, Andrew Appel, etc.
*)
structure Eval : EVAL =
struct

 structure A = Absyn

 datatype value =
    IntV of int
  | FunV of A.func
  | ExternV of A.id
  | TupleV of value list
  | LocV of int   

 type externalFun = value -> value
 
 val externals : (string * externalFun) list = [
   ("printint", fn IntV i => (print (Int.toString i); (TupleV [])))
 ]

 type environment = value Symbol.table
 val externEnv : environment =
   List.foldl (fn ((name,body),env) => 
     Symbol.enter (env, Symbol.symbol name, ExternV (Symbol.symbol name)))
   Symbol.empty externals

 val externFuns : externalFun Symbol.table = 
   List.foldl (fn ((name,body),env) => Symbol.enter (env, Symbol.symbol name, body))
   Symbol.empty externals 

 exception Eval

 fun eval_exp (topenv: environment, 
	       env: environment, 
               pos: ErrorMsg.pos2, 
               heap: value Heap.heap) = 
 let fun eval_err s = (ErrorMsg.error(pos, s); raise Eval)  
    fun ee x = 
    (case x of
      A.Int(i) => IntV i
    | A.Id(id) => 
	(case Symbol.look(env,id)
	 of SOME x => x
	  | NONE => eval_err("Identifier " ^ Symbol.name id ^ " unknown."))
    | A.Op (oper, elist) =>
 	(case (oper, map ee elist)
         of (A.Add, [IntV i1, IntV i2]) => IntV (i1 + i2)
          | (A.Sub, [IntV i1, IntV i2]) => IntV (i1 - i2)
          | (A.Mul, [IntV i1, IntV i2]) => IntV (i1 * i2)
          | (A.LT, [IntV i1, IntV i2]) => IntV (if i1 < i2 then 1 else 0)
          | (A.Eq, [IntV i1, IntV i2]) => IntV (if i1 = i2 then 1 else 0)
          | (A.Ref, [v]) => 
	      let val l = Heap.fresh_loc heap
               in Heap.set_loc heap l v; LocV l
              end
          | (A.Get, [LocV l]) => Heap.get_loc heap l
          | (A.Get, [_]) => eval_err "Get of a nonlocation."
          | (A.Set, [LocV l, v]) => (Heap.set_loc heap l v; TupleV[])
          | (A.Set, [_,_]) => eval_err "Set of a nonlocation."
          | (_,_) => eval_err "Invalid operator arguments.")
    | A.If(e1,e2,e3) =>
 	(case  ee e1
	 of IntV 0 => ee e3
          | IntV _ => ee e2
 	  | _ => eval_err "Expected an integer for test expression.")
    | A.While(e1,e2) =>
 	(case  ee e1
	 of IntV 0 => TupleV[]
          | IntV _ => (ee e2; ee (A.While (e1,e2)))
 	  | _ => eval_err "Expected an integer for test expression.")
    | A.Call (ef,ea) => 
      let val fun_val = ee ef
          val arg_val = ee ea
      in case fun_val
         of FunV (f_id,arg_id,a_tp,f_tp,f_body) =>
            eval_exp (topenv, Symbol.enter(topenv,arg_id,arg_val), pos, heap) f_body
         | ExternV id => 
           (case Symbol.look(externFuns,id)
            of SOME f => (f arg_val handle _ => eval_err ("Error in "^ Symbol.name id))
             | NONE => eval_err "Invalid external function.")
         | _ => eval_err "Invalid function."
      end
    | A.Tuple es => TupleV (List.map ee es)
    | A.Proj (i,e) => 
      (case ee e
       of  TupleV l => (List.nth (l,i) 
	                handle Subscript => eval_err "No such element in tuple.")
        | _ => eval_err "Expected tuple.")
    | A.Let (id,e1,e2) => 
     let val v1 = ee e1 
         val env' = Symbol.enter(env,id,v1)
      in eval_exp (topenv, env', pos, heap) e2
     end
    | A.Constrain (e,tp) => ee e
    | A.Pos (pos',e) => eval_exp (topenv, env, pos', heap) e)
  in ee
  end

 fun add_a_fun ((pos, f as (f_id,_,_,_,_)), env) = Symbol.enter(env,f_id,FunV f)

 fun eval_prog funs = 
 let val env = List.foldl add_a_fun externEnv funs 
     val heap : value Heap.heap = Heap.empty()
  in eval_exp (env, env, (0,0), heap) 
       (A.Call(A.Id(Symbol.symbol "main"), A.Int 0))
 end
end
