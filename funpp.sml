(* 
   Created: 12/14 2004
   Authors: David Walker, Yitzhak Mandelbaum, etc.
*)
structure FunPP : FUN_PP =
  struct

  structure A = Absyn

  exception PrettyPrint_failed
  open PP Eval
  infixr 6 ^^
  infixr 6 ^/
  val unexpand = ref true

  val kwd = text
   fun pp_id id = text (Symbol.name id)
   fun pp_exp prec up_prec d =
     if up_prec > prec 
       then (agrp ((kwd "(") ^^ (nest 1 d) ^^ (kwd ")")))
     else (agrp d)
   fun pp_int i = text (Int.toString i)
     
   fun pp_loc l = text ("l"^(Int.toString l))

   fun pp_tp A.Inttp = kwd "int"
     | pp_tp (A.Tupletp tps) =
     hgrp((kwd "<")^^(nest 1 (commalist pp_tp tps))^^(kwd ">"))
     | pp_tp (A.Arrowtp(tp1,tp2)) =
     agrp((pp_tp tp1)^/ (nest 3 ((kwd "->") ^/ (pp_tp tp2))))
     | pp_tp (A.Reftp(tp)) = (pp_tp tp) ^/ (kwd "ref") 

     val prec_level = ref 0
     fun cur_prec () = !prec_level
     fun new_prec () = ((prec_level := !prec_level + 1);!prec_level)

     datatype assc =  Left | Right | Non

     val set_op =   (":=", new_prec(), Left) 
     val get_op =  ("!", new_prec(), Left) 
     val or_op =    ("||",new_prec (),Left)
     val and_op =   ("&",new_prec (),Left)
     val not_op =   ("not ",new_prec (),Left)
     val lt_op =    ("<",new_prec (),Left)
     val eq_op =    ("=",cur_prec (),Left)
     val plus_op =  ("+",new_prec (),Left)
     val sub_op =   ("-",cur_prec (),Left)
     val mul_op =   ("*",new_prec (),Left)
     fun proj_op i =    ("#" ^ (Int.toString i) ^ " ",cur_prec (),Left)
     val uminus_op =    ("-",cur_prec (),Left)
     val ref_op    = ("ref ", new_prec(), Left)
       
     fun pp_binop (op_str,prec,assc) up_prec e1 e2 = let
       val (l,r) = case assc of
	 Left => (0,1)
       | Right => (1,0)
       | Non => (0,0) 
     in  pp_exp prec up_prec 
       ((pe (prec+l) e1) ^/
	(kwd op_str)    ^/
	(pe (prec+r) e2))
     end
     and pp_unop (op_str,prec,assc) up_prec e = let
       val i = case assc of
	 Non => 0
       | _ => 1
     in pp_exp prec up_prec ((kwd op_str)^^pe (prec+i) e)
     end
     and pe up_prec e = 
       (case e of
	  A.Int i => pp_int i
	| A.Id id => pp_id id
	| A.Op (A.Add,[e1,e2]) => pp_binop plus_op up_prec e1 e2
	| A.Op (A.Sub,[A.Int 0,e]) => pp_unop uminus_op up_prec e
	| A.Op (A.Sub,[e1,e2]) => pp_binop sub_op up_prec e1 e2
	| A.Op (A.Mul,[e1,e2]) => pp_binop mul_op up_prec e1 e2
	| A.Op (A.LT,[e1,e2]) => pp_binop lt_op up_prec e1 e2
	| A.Op (A.Eq,[e1,e2]) => pp_binop eq_op up_prec e1 e2
	| A.Op (A.Set,[e1,e2]) => pp_binop set_op up_prec e1 e2   
        | A.Op (A.Ref,[e]) => pp_unop ref_op up_prec e            
        | A.Op (A.Get,[e]) => pp_unop get_op up_prec e            
        | A.Op _ => kwd"UNKNOWN OP"
	| A.If (e1,e2,e3) => let
	    fun dflt (e1,e2,e3) =  agrp
	    (((kwd "if ")^^(pe 0 e1))
	     ^^(nest 2 ((kwd " then")^/(pe 0 e2)))
	     ^/(nest 2 ((kwd "else")^/(pe 0 e3))))
	  in if !unexpand then
	      case (e1,e2,e3) of
		(e1, A.If (e,A.Int 1,A.Int 0), A.Int 0) => pp_binop and_op up_prec e1 e
	      | (e1, A.Int 1, A.If (e,A.Int 1,A.Int 0)) => pp_binop or_op up_prec e1 e
	      | (e1, A.Int 0, A.Int 1) => pp_unop not_op up_prec e1
	      | (e1,e2,e3) => dflt(e1,e2,e3)
	     else dflt(e1,e2,e3)
	  end
        | A.While (e1,e2) => agrp (((kwd "while ")^^(pe 0 e1))
	     ^^(nest 2 ((kwd " do")^/(pe 0 e2))))
	| A.Call (ef,ea) => 
	    agrp
	    ((pe 0 ef)^^(kwd "(")^^(pe 0 ea)^^(kwd ")"))
	| A.Tuple es => 
	    agrp
	    ((kwd "<")^^(commalist (pe 0) es)^^(kwd ">"))
	| A.Proj (i,e) => pp_unop (proj_op i) up_prec e
	| A.Let (id,e1,e2) =>
	      (vgrp
	       ((kwd "let ")^^(pp_id id)^^(kwd " = ")^^(pe 0 e1)^^
		(if isLet e2 then ((kwd " in ")^/(pe 0 e2))
		 else (nest 2 ((kwd " in ")^/(pe 0 e2))))))
        | A.Constrain (e,t) =>
             agrp ((kwd "(")^^(pe 0 e)^^(kwd ":")^^(pp_tp t)^^(kwd ")"))
	| A.Pos (pos,e) => pe up_prec e)
     and isLet (A.Let _) = true
       | isLet (A.Pos(p,e)) = isLet(e)
       | isLet _ = false
     fun pp_fundec (pos, (name,arg,arg_tp,fun_tp,e)) =
       agrp ((kwd "fun ")^^(pp_id name)^^
	     (agrp (kwd "(")^^(pp_id arg)^^(kwd ":")^^(pp_tp arg_tp)^^(kwd "):")^^(pp_tp fun_tp)^^
	      (agrp (nest 3 ((kwd " =") ^/ pe 0 e)))))
     fun pp_prog fs = 	
       vgrp ((list (break) pp_fundec fs) ^/ empty)       

     fun print_prog p = let
       val curr = !unexpand
     in unexpand := true;
       ppToFile TextIO.stdOut 74 (pp_prog p);
       unexpand := curr
     end

     fun print_prog' p = let
       val curr = !unexpand
     in unexpand := false;
       ppToFile TextIO.stdOut 74 (pp_prog p);
       unexpand := curr
     end

     fun print_func f = 
	 ppToFile TextIO.stdOut 74 (pp_fundec ((0,0),f))

     fun print_exp e = 
       ppToFile TextIO.stdOut 74 (pe 0 e)

     fun pv (IntV i) = pp_int i
       | pv (FunV f) = pp_fundec ((0,0),f)
       | pv (ExternV id) = pp_id id
       | pv (TupleV vs) = agrp ((kwd "<")^^(commalist pv vs)^^(kwd ">"))
       | pv (LocV l) = (pp_loc l)
	     
     fun print_val v = ppToFile TextIO.stdOut 74 (pv v)
end
