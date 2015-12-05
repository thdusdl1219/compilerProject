structure Mips :> MIPS  =
struct

  exception IMMTooLarge

  val regnames = [ "$zero", "$at", "$v0", "$v1","$a0","$a1" ,"$a2", "$a3",
		   "$t0" ,"$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
		   "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
		   "$t8", "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra"]

  type reg = int

  fun reg (name : string) : reg  = 
   let fun f (i, s::rest) = if name=s then i else f (i+1, rest)
	 | f (_, nil) = 
	(case (String.sub(name,0), 
	       Int.fromString( String.substring (name, 2, (String.size name - 2))))
	 of (#"x", SOME i) => i +32  (* 32 is register $x0 *)
	  | _ => raise Subscript)
	 handle Subscript => ErrorMsg.impossible ("illegal register name: "^name)
    in f (0, regnames)
   end

  fun isvirtual r =  (r > 31)
  fun reg2name r = if isvirtual r then "$x" ^ Int.toString(r-32)
		   else List.nth(regnames,r) 
  fun regs2names rs =
   let fun f [] = [] | f [r] = [reg2name r] | f(r::rs) = reg2name r :: "," :: f rs
    in String.concat (f rs)
   end

  fun reg2int r = r

  val comparereg = Int.compare

  fun incr x = (x:= !x +1)
  val regCount = ref 32

  val registers = List.map  reg  regnames

  val callerSaved = map reg ["$a0","$a1","$a2","$a3", "$t0", "$t1",
                             "$t2","$t3","$t4","$t5","$t6","$t7","$t8","$t9",
                             "$v0","$v1"]
  val calleeSaved = map reg [ "$s0","$s1","$s2","$s3","$s4","$s5","$s6","$s7"]

  fun newReg () = !regCount before incr regCount

  structure RegSet =  RedBlackSetFn(type ord_key = reg 
			       val compare = comparereg) 

  structure RegTb= IntMapTable(type key = reg 
			     val getInt  = reg2int)

  type lab = Symbol.symbol

  val labCount = ref 0

  fun freshlab () = (incr labCount ; Symbol.symbol ("L"^(Int.toString (!labCount))))

  val thislab = Symbol.symbol
  val lab2string = Symbol.name

  type immed  = int
  fun immed x = if x> 65535 then raise IMMTooLarge else x
  fun immed2int im = im
  fun compareimmed (im1, im2) = Int.compare (im1,im2)

  val wordSize = 4
  val wordSizeImmed = immed(4)

  type address = immed * reg

  fun compareaddress _ = raise (Fail "fixme")

  (* Instructions *)

  datatype instruction = 
    Arith2 of aop2 * reg * reg
    | Arith3 of aop3 * reg * reg * reg
    | Arithi of aopi * reg * reg * immed
    | Li of reg * immed
    | La of reg * lab
    | Lw of reg * address
    | Sw of reg * address
    | Move of reg * reg
    | Branchz of comparecode1 * reg * lab
    | Branchu of comparecode2 * reg * reg * lab
    | Branch of comparecode1 * reg * reg * lab
    | J of lab
    | Jal of lab
    | Jr of reg * reg list
    | Jalr of reg * reg * reg list * reg list
    | Nop
    | Syscall

  and aop2 = Abs | Neg | Not
  and aop3 = Add | And | Mulo | Div | Or | Rem | Sub | Xor| Seq | Slt
  and aopi = Addi | Andi | Ori | Xori
  and comparecode1 = Lt | Eq | Ne | Ge | Gt | Le 
  and comparecode2 = Ltu | Geu | Gtu | Leu


  type codeblock  = lab * instruction list 
  type stringblock = lab * string 
  type funcode = codeblock list   
  type program = stringblock list * funcode list

  fun reset () = (regCount := 32; labCount := 0)

  (* convert the data block to string*)
  fun data2string sL = 
       List.foldl (fn ((lab, str), s) 
		      => (lab2string lab)^":"^"\t.asciiz\t"^"\""^str^"\""^"\n"^s)
		   "" sL


  (* convert instruction to string*)
  fun instr2string instr = 
     let fun int2string x = if x < 0 then "-"^(Int.toString(~x))
			    else Int.toString x
     in
     case instr of
	 Arith2(op2, rd, rs) => 
	 let val (opname, oper) = 
	       case op2 of Abs => ("abs","abs ")
			 | Neg => ("neg", "-")
			 | Not => ("not", "~")
	 in  "\t"^opname^"\t"^(reg2name rd)^", "^ reg2name rs^
	     "\t# " ^ reg2name rd ^" := " ^ oper ^ reg2name rs ^ "\n"
	 end
       | Arith3 (op3, rd, rs, rt) => 
	 let val (opname, oper) = 
	    case op3 of Add => ("add","+")
		      | And => ("and","&")
		      | Mulo => ("mulo","*")
		      | Div => ("div","/")
		      | Or =>  ("or","|")
		      | Rem => ("rem","%")
		      | Sub => ("sub","-")
		      | Xor => ("xor","^")
		      | Seq => ("seq","==")
		      | Slt => ("slt","<")
	  in  "\t"^ opname^"\t"
	      ^(reg2name rd)^", "^ (reg2name rs)^", "^ (reg2name rt)
	      ^ "\t# " ^ reg2name rd ^" := " ^ reg2name rs 
	      ^ oper ^ reg2name rt ^ "\n"
	  end
       | Arithi (opi, rt, rs, immed) => 
	 let val (opname, oper) = 
		 case opi of Addi => ("addi","+")
			   | Andi => ("andi","&")
			   | Ori  => ("ori","|")
			   | Xori => ("xori","^")
	 in  "\t"^ opname ^" \t"^ (reg2name rt)^", "
	     ^ (reg2name rs)^", " ^ (int2string (immed2int immed))
	     ^"\t# " ^ reg2name rt ^" := " ^ reg2name rs 
	     ^ oper ^ (int2string (immed2int immed)) ^ "\n"
	 end	               

       | Li (reg, im)   =>
	     "\t"^"li \t"^ reg2name reg ^", "^ int2string (immed2int im)
	     ^ "\t\t# " ^ reg2name reg ^" := " ^ int2string (immed2int im)
	     ^ "\n"

       | La (reg, lab) =>
	     "\t"^"la \t"^  (reg2name reg)^", "^lab2string lab 
	     ^ "\t# " ^ reg2name reg ^" := " ^ lab2string lab 
	     ^"\n"

       | Lw (reg, (im, r)) => 
	     "\t"^"lw \t"^  (reg2name reg)^", "^ int2string (immed2int im)
	     ^ "("^ reg2name r ^ ")" 
	     ^ "\t# " ^ reg2name reg ^" := [" ^ reg2name r ^"+"
	     ^ int2string (immed2int im) ^ "]\n"

       | Sw (reg, (im, r)) => 
	    "\t"^"sw \t"^ reg2name reg ^", "^ int2string (immed2int im) 
	   ^"("^ reg2name r ^")" 
	     ^ "\t# [" ^ reg2name r ^"+"
	     ^ int2string (immed2int im) ^ "] := " ^ reg2name reg ^"\n"

       | Move (reg1, reg2) =>
         (if reg1=reg2 then "\t\t\t"  
          else "\t"^"move \t"^(reg2name reg1)^", "^ (reg2name reg2))
         ^"\t# " ^ reg2name reg1 ^ " := " ^ reg2name reg2 ^"\n"
       | Branchz (comparecode, reg, lab) => 
	 let val (opname, oper) = 
	   case comparecode 
	    of Lt => ("bltz","<")
	     | Eq => ("beqz","==")
	     | Ne => ("bnez","!=")
	     | Ge => ("bgez",">=")
	     | Gt => ("bgtz",">")
	     | Le => ("blez","<=")
	  in "\t"^ opname ^ "\t"^ reg2name reg ^ ", " ^ lab2string lab 
	     ^"   \t# if (signed) " ^ reg2name reg ^ " " ^ oper ^ " 0 goto " 
		    ^lab2string lab ^"\n"  
	 end

       | Branchu (comparecode, reg1, reg2, lab) => 
	 let val (opname, oper) = 
	   case comparecode 
	    of Ltu => ("bltu","<")
	     | Geu => ("bgeu",">=")
	     | Gtu => ("bgtu",">")
	     | Leu => ("bleu","<=")
	  in "\t"^ opname ^ "\t"^ reg2name reg1 ^ ", " ^ reg2name reg2 
	     ^ ", " ^ lab2string lab 
	     ^"\t# if (unsigned) " ^ reg2name reg1 ^ " " ^ oper 
		    ^ " " ^ reg2name reg2 ^ " goto " ^lab2string lab ^"\n"  
	 end

       | Branch (comparecode, reg1, reg2, lab) => 
	 let val (opname, oper) = 
	   case comparecode 
	    of Lt => ("blt","<")
	     | Eq => ("beq","==")
	     | Ne => ("bne","!=")
	     | Ge => ("bge",">=")
	     | Gt => ("bgt",">")
	     | Le => ("ble","<=")
	  in "\t"^ opname ^ "\t"^ reg2name reg1 ^ ", " 
	     ^ reg2name reg2 ^ ", " ^ lab2string lab 
	     ^"\t# if (signed) " ^ reg2name reg1 ^ " " ^ oper 
		   ^ reg2name reg2 ^ " goto " 
		    ^lab2string lab ^"\n"  
	 end

       | J (lab) => "\t"^"j \t"  ^(lab2string lab)^"\t\t# goto "
				 ^(lab2string lab)^"\n"
       | Jal (lab) => "\t"^"jal \t" ^(lab2string lab)^"\t\t# call "
				 ^(lab2string lab)^"\n"
       | Jr (reg,also) => "\t"^"jr \t"^(reg2name reg) ^
		 "\t\t# also uses: "^ regs2names also ^ "\n"
       | Jalr (reg1, reg2, use, def)  => "\t"^"jalr \t"^ (reg2name reg1)
			     ^ ", "^ (reg2name reg2)^ "\t# (" 
            ^ regs2names def ^") := "
	    ^ (if reg2name reg1 = "$ra" then "" else "weird ") 
            ^ "call " ^ reg2name reg1 ^"(" ^ regs2names use ^ ")\n"
       | Syscall =>  "\t"^"syscall \n"
       | Nop => "nop"
     end

  (* convert code block to string*)
     fun code2string cL = 
       List.foldl (fn ((lab, instrL),s) 
		      =>  s ^ (List.foldl (fn (x,y) => y^x ) ((lab2string lab)^":"^"\n") 
				    (List.map instr2string instrL)) 

		 )
		"" cL 

     (* print the assembly program to the output stream*)
     fun printAssem ( os, (strblockL, funL))  =
	 let val s = "\t.data \n"^(data2string strblockL)^"\t.text \n"^ (code2string (List.concat funL) )
	 in
	     TextIO.output(os, s)
	 end

  type def_use = {def: RegSet.set, use: RegSet.set}
  fun list2set l = RegSet.addList(RegSet.empty,l)

  val instr_def_use : instruction -> def_use =
  fn Arith2(_,rd,rs) => {def=list2set[rd], use=list2set[rs]}
   | Arith3(_,rd,rs,rt) => {def=list2set[rd], use=list2set[rs,rt]}
   | Arithi(_,rt,rs,_) => {def=list2set[rt], use=list2set[rs]}
   | Li(r,_) => {def=list2set[r], use=RegSet.empty}
   | La(r,_) => {def=list2set[r], use=RegSet.empty}
   | Lw(r,(_,ra)) => {def=list2set[r], use=list2set[ra]}
   | Sw(r,(_,ra)) => {def=RegSet.empty, use=list2set[r,ra]}
   | Move(rd,rs) => {def=list2set[rd], use=list2set[rs]}
   | Branchz(_,r,_) => {def=RegSet.empty, use=list2set[r]}
   | Branchu(_,r1,r2,_) => {def=RegSet.empty, use=list2set[r1,r2]}
   | Branch(_,r1,r2,_) => {def=RegSet.empty, use=list2set[r1,r2]}
   | Jal(_) => {def=list2set[reg "$ra"], use=RegSet.empty}
   | Jr(r,also) => {def=RegSet.empty, use=list2set(r::also)}
   | Jalr(r1,r2,use,def) => {def=list2set(r1 :: def), use=list2set(r2::use)}
   | Syscall => ErrorMsg.impossible "Syscall"
   | Nop => {def=RegSet.empty, use=RegSet.empty}
   | J _ => {def=RegSet.empty, use=RegSet.empty}
 (*  | e => (print(instr2string e); {def=RegSet.empty, use=RegSet.empty})*)

   local val info =  [NONE,
   (* print_int *)    SOME{def=RegSet.empty, use=list2set[reg "$a0"]},
   (* print_float *)  NONE,
   (* print_double *) NONE,
   (* print_string *) SOME{def=RegSet.empty, use=list2set[reg "$a0"]},
   (* read_int *)     SOME{def=list2set[reg "$v0"], use=RegSet.empty},
   (* read_float *)   NONE,
   (* read_double *)  NONE,
   (* read_string *)  SOME{def=RegSet.empty, use=list2set[reg "$a0", reg "$a1"]},
   (* sbrk *)         SOME{def=list2set[reg "$v0"], use=list2set[reg "$a0"]},
   (* exit *)         SOME{def=RegSet.empty, use=RegSet.empty},
   (* print_char *)   SOME{def=RegSet.empty, use=list2set[reg "$a0"]},
   (* read_char *)    SOME{def=list2set[reg "$a0"], use=RegSet.empty},
   (* open *)         SOME{def=list2set[reg "$a0"], 
	                   use=list2set (map reg ["$a0","$a1","$a2"])},
   (* read *)         SOME{def=list2set[reg "$a0"], 
	                   use=list2set (map reg ["$a0","$a1","$a2"])},
   (* write *)        SOME{def=list2set[reg "$a0"], 
	                   use=list2set (map reg ["$a0","$a1","$a2"])},
   (* close *)        SOME{def=RegSet.empty, use=list2set[reg "$a0"]},
   (* exit2 *)        SOME{def=RegSet.empty, use=list2set[reg "$a0"]}]
 in
  fun syscall_def_use (num: int) : def_use option =
       List.nth(info,num) handle Subscript => NONE
 end  

 type allocation = reg RegTb.table

 fun rename_regs (table: allocation) =
 let fun f r = if isvirtual r
	       then case RegTb.look(table,r) of SOME x => (x) 
                  (*  | NONE => ErrorMsg.impossible ("rename_regs: "^
	                              reg2name r)*)
                    | NONE => (r)
	       else r
 in
  fn Arith2(i,rd,rs) => Arith2(i, f rd, f rs)
   | Arith3(i,rd,rs,rt) => Arith3(i, f rd, f rs, f rt)
   | Arithi(i,rt,rs,n) => Arithi(i, f rt, f rs, n)
   | Li(r,n) => Li(f r, n)
   | La(r,lab) => La(f r, lab)
   | Lw(r,(lab,ra)) => Lw(f r, (lab, f ra))
   | Sw(r,(lab,ra)) => Sw(f r, (lab, f ra))
   | Move(rd,rs) => Move(f rd, f rs)
   | Branchz(i,r,lab) => Branchz(i, f r, lab)
   | Branchu(i,r1,r2,lab) => Branchu(i, f r1, f r2, lab)
   | Branch(i,r1,r2,lab) => Branch(i, f r1, f r2, lab)
   | J(lab) => J lab
   | Jal(lab) => Jal lab
   | Jr(r,also) => Jr(f r, map f also)
   | Jalr(r1,r2,use,def) => Jalr(f r1, f r2, map f use, map f def)
   | Syscall => Syscall
   | Nop => Nop
 end

end
