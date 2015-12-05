structure Absyn =
struct
    type id = Symbol.symbol

    datatype tp =
       Inttp                (* integer type *)
     | Tupletp of tp list   (* n-ary tuples; n can be 0 *)
     | Arrowtp of tp * tp   (* function type with argument and result *)
     | Reftp of tp          (* ref type *)

    datatype oper =
       Add      (* 2-argument addition operation *)
     | Sub      (* 2-argument subtraction operation *)
     | Mul      (* 2-argument multiplication operation *)
     | LT       (* less than operator; 
                         return 1 when true; 0 when false *)
     | Eq       (* tests two integers for equality;
                         return 1 when true; 0 when false *)
     | Ref      (* ref constructor *)
     | Get      (* ref dereference *)
     | Set      (* ref assignment *)

    datatype exp =
       Int of int              (* constant integer *)
     | Id of id		       (* identifier *)
     | Op of oper * exp list   (* unary or binary operator; expressions are 
                                  evaluated left-to-right *)
     | Tuple of exp list       (* create a pair, triple etc. *)
     | Proj of int * exp       (* get ith field from tuple; Proj(0,e) 
                                 retrieves the first field of the tuple e *) 
     | If of exp * exp * exp   (* if first argument is 0 take the first 
                                  branch; otherwise take the second branch *)
     | While of exp * exp
     | Call of exp * exp       (* function call; call-by-value: evaluate the
                                  first expression until you get a value;
				  evaluate the second expression until you 
                                  get an argument; check the first value is a
                                  function; apply the function to the argument *)
     | Let of id * exp * exp   (* evaluate the first expression; bind the 
                                  resulting value to the identifier; 
                                  evaluate the second expression *)
     | Constrain of exp * tp
     | Pos of ErrorMsg.pos2 * exp
                               (* marks position of expression in source *)
	     
    (* a recursive function  (fun f (x1 : t1) : t2 = e) is (f,x1,t1,t2,e) *)
    type func = id * id * tp * tp * exp    
    type fundec = ErrorMsg.pos2 * func
      
    type prog = fundec list
  
  val externals : (id * tp) list = [
   (Symbol.symbol "printint", Arrowtp(Inttp, Tupletp[]))
  ]

end
