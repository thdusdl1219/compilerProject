(* Created: 1/12/2005
   Authors: Yitzhak Mandelbaum, etc.
*)
signature FUN_PP = sig
  exception PrettyPrint_failed
  
  (* Print program, printing and,or,not if-expressions as and,or,not. *)
  val print_prog: Absyn.prog -> unit

  (* Print program exactly as is. *)
  val print_prog': Absyn.prog -> unit

  val print_func : Absyn.func -> unit

  val print_exp: Absyn.exp -> unit

  val print_val : Eval.value -> unit

end
