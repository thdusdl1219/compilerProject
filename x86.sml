structure X86 :> X86s =
struct
  val regnames = [ "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%esp", "%ebp", "%eip"]

  type reg = int


  fun reg (name : string) : reg  = 
    let 
      fun f (i, s::rest) = if name=s then i else f (i+1, rest)
	      | f (_, nil) = 
	        (case (String.sub(name,0), Int.fromString( String.substring (name, 2, (String.size name - 2))))
	          of (#"x", SOME i) => i + 9  (* 32 is register $x0 *)
	            | _ => raise Subscript)
	          handle Subscript => ErrorMsg.impossible ("illegal register name: "^name)
    in f (0, regnames)
    end
  
  fun isvirtual r = (r > 8)
  fun reg2name r =
    if isvirtual r then "$x" ^ Int.toString(r-9)
    else List.nth(regnames, r)
  fun regs2names rs =
    let fun f [] = [] | f [r] = [reg2name r] | f(r::rs) = reg2name r :: "," :: f rs
    in String.concat (f rs)
    end

  fun reg2int r = r

  val comparereg = Int.compare

  fun incr x = (x:= !x +1)
  val regCount = ref 9

  val registers = List.map  reg  regnames

  val callerSaved = map reg [ "%eax", "%ecx", "%edx" ]
  val calleeSaved = map reg [ "%ebx", "%esi", "%edi", "%esp", "%ebp"]

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
  fun immed x = x
  fun immed2int im = im
  fun compareimmed (im1, im2) = Int.compare (im1,im2)

  val wordSize = 4
  val wordSizeImmed = immed(4)

  type address = immed * reg

  fun compareaddress _ = raise (Fail "fixme")

  datatype instruction =
      Arith1 of aop1 * reg
    | Arith2 of aop2 * reg * reg
    | Arith3 of aop3 * reg * reg * reg
    | Arithi of aopi * reg * reg * immed
    | Li of reg * immed
    | La of reg * lab
    | Lw of reg * address
    | Sw of reg * address
    | Move of reg * reg
    | Branchz of comparecode1 * reg * lab
    | Branch of comparecode1 * reg * reg * lab
    | J of lab
    | Jal of lab
    | Jr of reg * reg list
    | Jalr of reg * reg * reg list * reg list
    | Nop
    | Syscall
    | Leave
    | Ret
    | Push of reg
    | Pop of reg
    | Branch2 of comparecode1 * lab

  and aop1 = Div
  and aop2 = Abs | Neg | Not
  and aop3 = Add | And | Or | Sub | Xor | Mul
  and aopi = Addi | Andi | Ori | Xori
  and comparecode1 = Lt | Eq | Ne | Ge | Gt | Le 

  type codeblock  = lab * instruction list 
  type stringblock = lab * string 
  type funcode = codeblock list   
  type program = stringblock list * funcode list

  fun reset () = (regCount := 9; labCount := 0)

  fun data2string sL = 
       List.foldl (fn ((lab, str), s) 
		      => (lab2string lab)^":"^"\n\t.string\t"^"\""^str^"\""^"\n"^s)
		   "" sL

  fun instr2string instr = 
    let 
      fun int2string x = 
        if x < 0 then "$-" ^ (Int.toString(~x))
        else "$" ^ (Int.toString x)
      fun addrint2string x = 
        if x < 0 then "-" ^ (Int.toString(~x))
        else (Int.toString x)
      fun mov2string rs rd = 
       ( if(rs = rd) then
          "\t\t\t"
        else
          "\t" ^ "movl" ^ "\t" ^ reg2name rs ^ ", " ^ reg2name rd ) ^
          "\t#" ^ reg2name rd ^ " := " ^ reg2name rs ^ "\n"

    in
      case instr of
        Arith1 (op1, rd) => (
          case op1 of
            Div =>
              "\t" ^ "div" ^ "\t" ^ reg2name rd ^ "\n") (* need change! *)
      | Arith2 (op2, rd, rs) => (
          case op2 of 
            Abs =>
              mov2string rs rd ^
              "\t" ^ "neg" ^ "\t" ^ reg2name rd ^ "\n" ^
              "\t" ^ "cmp" ^ "\t" ^ reg2name rs ^ ", " ^ reg2name rd ^ "\n" ^
              "\t" ^ "cmovl" ^ "\t" ^ reg2name rs ^ ", " ^ reg2name rd ^ 
              "\t#" ^ reg2name rd ^ " := " ^ "abs " ^ reg2name rs ^ "\n"
          | Neg =>
              mov2string rs rd ^
              "\t" ^ "neg" ^ "\t" ^ reg2name rd ^ 
              "\t#" ^ reg2name rd ^ " := " ^ "-" ^ reg2name rs ^ "\n"
          | Not =>
              mov2string rs rd ^
              "\t" ^ "not" ^ "\t" ^ reg2name rd ^ 
              "\t#" ^ reg2name rd ^ " := " ^ "~" ^ reg2name rs ^ "\n" )

      | Arith3(op3, rd, rs, rt) => (
          case op3 of
            Add =>
              if (rd = rs) then
	              "\t"^ "add" ^"\t"
	              ^ (reg2name rt) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rd
	              ^ "+" ^ reg2name rt ^ "\n"
              else
                mov2string rt rd
	              ^ "\t"^ "add" ^"\t"
	              ^ (reg2name rs) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rs
	              ^ "+" ^ reg2name rt ^ "\n"
          | And => 
              if (rd = rs) then
	              "\t"^ "and" ^"\t"
	              ^ (reg2name rt) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rd
	              ^ "&" ^ reg2name rt ^ "\n"
              else
                mov2string rt rd
	              ^ "\t"^ "and" ^"\t"
	              ^ (reg2name rs) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rs
	              ^ "&" ^ reg2name rt ^ "\n"
          
          | Or => 
              if (rd = rs) then
	              "\t"^ "or" ^"\t"
	              ^ (reg2name rt) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rd
	              ^ "|" ^ reg2name rt ^ "\n"
              else
                mov2string rt rd
	              ^ "\t"^ "or" ^"\t"
	              ^ (reg2name rs) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rs
	              ^ "|" ^ reg2name rt ^ "\n"
          | Sub =>
              if(rt = rd) then
                "\t" ^ "neg" ^ "\t" ^ reg2name rd ^ "\n" 
  	            ^ "\t"^ "add" ^"\t"
	              ^ (reg2name rs) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rs
	              ^ "-" ^ reg2name rt ^ "\n"
              else
                mov2string rs rd
	              ^ "\t"^ "sub" ^"\t"
	              ^ (reg2name rt) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rs
	              ^ "-" ^ reg2name rt ^ "\n"
          | Xor =>
              if (rd = rs) then
	              "\t" ^ "xor" ^"\t"
	              ^ (reg2name rt) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rd
	              ^ "^" ^ reg2name rt ^ "\n"
              else
                mov2string rt rd
	              ^ "\t" ^ "xor" ^"\t"
	              ^ (reg2name rs) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rs
	              ^ "^" ^ reg2name rt ^ "\n" 
          | Mul =>
              if (rd = rs) then
	              "\t" ^ "imul" ^"\t"
	              ^ (reg2name rt) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rd
	              ^ "*" ^ reg2name rt ^ "\n"
              else
                mov2string rt rd
	              ^ "\t" ^ "imul" ^"\t"
	              ^ (reg2name rs) ^ ", " ^ (reg2name rd)
	              ^ "\t# " ^ reg2name rd ^ " := " ^ reg2name rs
	              ^ "*" ^ reg2name rt ^ "\n" )

      | Arithi (opi, rt, rs, immed) =>
          let val (opname, oper) =
            case opi of
              Addi => ("add", "+")
            | Andi => ("and", "&")
            | Ori => ("or", "|")
            | Xori => ("xor", "^")
          in
            mov2string rs rt
	          ^ "\t" ^ opname ^ " \t"
	          ^ (int2string (immed2int immed)) ^ ", " ^ (reg2name rt)	          
            ^ "\t# " ^ reg2name rs ^ " := " ^ reg2name rs 
	          ^ oper ^ (int2string (immed2int immed)) ^ "\n"
          end
      | Li (reg, im) =>
	        "\t" ^ "movl \t" ^ int2string (immed2int im) ^", "^ reg2name reg
	        ^ "\t# " ^ reg2name reg ^" := " ^ int2string (immed2int im)
	        ^ "\n"
      | La (reg, lab) =>
	        "\t" ^ "lea \t" ^ lab2string lab ^ ", " ^  (reg2name reg)
	        ^ "\t# " ^ reg2name reg ^ " := " ^ lab2string lab 
	        ^ "\n"
      | Lw (reg, (im, r)) =>
	        "\t" ^ "movl \t" ^ addrint2string (immed2int im)
	        ^ "(" ^ reg2name r ^ ")" ^ ", " ^  (reg2name reg)
	        ^ "\t# " ^ reg2name reg ^ " := [" ^ reg2name r ^ "+"
	        ^ addrint2string (immed2int im) ^ "]\n"
      | Sw (reg, (im, r)) =>
	        "\t" ^ "movl \t" ^ reg2name reg ^ ", " ^ addrint2string (immed2int im) 
	        ^ "("^ reg2name r ^")" 
	        ^ "\t# [" ^ reg2name r ^"+"
	        ^ addrint2string (immed2int im) ^ "] := " ^ reg2name reg ^"\n"
      | Move (reg1, reg2) => mov2string reg2 reg1
      | Branchz (cmp, reg, lab) =>
          let 
            val jmpinst =
              case cmp of
                Lt =>
                  "\t" ^ "jl \t" ^ lab2string lab ^ "\n"
              | Eq =>
                  "\t" ^ "je \t" ^ lab2string lab ^ "\n"
              | Ne =>
                  "\t" ^ "jne \t" ^ lab2string lab ^ "\n"
              | Ge =>
                  "\t" ^ "jge \t" ^ lab2string lab ^ "\n"
              | Gt =>
                  "\t" ^ "jg \t" ^ lab2string lab ^ "\n"
              | Le =>
                  "\t" ^ "jle \t" ^ lab2string lab ^ "\n"
          in
            ("\t" ^ "cmp \t" ^ int2string 0 ^", "^ reg2name reg ^ "\n" ^
            jmpinst)
          end
      | Branch (cmp, reg1, reg2, lab) =>
          let 
            val jmpinst =
              case cmp of
                Lt =>
                  "\t" ^ "jl \t" ^ lab2string lab ^ "\n"
              | Eq =>
                  "\t" ^ "je \t" ^ lab2string lab ^ "\n"
              | Ne =>
                  "\t" ^ "jne \t" ^ lab2string lab ^ "\n"
              | Ge =>
                  "\t" ^ "jge \t" ^ lab2string lab ^ "\n"
              | Gt =>
                  "\t" ^ "jg \t" ^ lab2string lab ^ "\n"
              | Le =>
                  "\t" ^ "jle \t" ^ lab2string lab ^ "\n"
          in
            ("\t" ^ "cmp \t" ^ reg2name reg2 ^", "^ reg2name reg1 ^ "\n" ^
            jmpinst)
          end
      | J (lab) => "\t"^"jmp \t"  ^(lab2string lab)^"\t\t# goto "
				^ (lab2string lab) ^"\n"
      | Jal (lab) => "\t"^"call \t" ^(lab2string lab)^"\t\t# call "
				^ (lab2string lab) ^"\n"
      | Jr (reg,also) => "\t"^"jmp \t*"^(reg2name reg) ^
		    "\t\t# also uses: " ^ regs2names also ^ "\n"
      | Jalr (reg1, reg2, use, def) => 
        "\t"^"call \t*"^ (reg2name reg2)
			  ^ "\t# (" 
        ^ regs2names def ^") := "
	      ^ (if reg2name reg1 = "%eip" then "" else ErrorMsg.impossible ("jalr reg1 != %eip")) 
        ^ "call " ^ reg2name reg2 ^"(" ^ regs2names use ^ ")\n"
      | Syscall =>  "\t"^"int \t$0x80 \n"
      | Nop => "\tnop\n"
      | Leave => "\tleave\n"
      | Ret => "\tret\n"
      | Push (reg) => "\t"^"push \t"^(reg2name reg) ^
		    "\t# push " ^ reg2name reg ^ "\n"
      | Pop (reg) => "\t"^"pop \t"^(reg2name reg) ^
		    "\t# pop " ^ reg2name reg ^ "\n"
      | Branch2 (cmp, lab) =>
          let 
            val jmpinst =
              case cmp of
                Lt =>
                  "\t" ^ "jl \t" ^ lab2string lab ^ "\n"
              | Eq =>
                  "\t" ^ "je \t" ^ lab2string lab ^ "\n"
              | Ne =>
                  "\t" ^ "jne \t" ^ lab2string lab ^ "\n"
              | Ge =>
                  "\t" ^ "jge \t" ^ lab2string lab ^ "\n"
              | Gt =>
                  "\t" ^ "jg \t" ^ lab2string lab ^ "\n"
              | Le =>
                  "\t" ^ "jle \t" ^ lab2string lab ^ "\n"
          in
            jmpinst
          end

          
    end


  fun code2string cL = 
    List.foldl (fn ((lab, instrL),s) 
		  =>  s ^ (List.foldl (fn (x,y) => y^x ) ((lab2string lab)^":"^"\n") 
				(List.map instr2string instrL)) 
        )
		    "" cL 

  fun printAssem ( os, (strblockL, funL))  =
	  let val s = "\t.section\t.rodata \n"^(data2string strblockL)^"\t.text\n\t.globl main \n"^ (code2string (List.concat funL) )
	  in
	   TextIO.output(os, s)
	  end

  type def_use = {def: RegSet.set, use: RegSet.set}
  fun list2set l = RegSet.addList(RegSet.empty,l)

  val instr_def_use : instruction -> def_use =
  fn
    Arith1 (_, rd) => {def=list2set[rd], use=list2set[rd]}
   | Arith2(_,rd,rs) => {def=list2set[rd], use=list2set[rs]}
   | Arith3(_,rd,rs,rt) => {def=list2set[rd], use=list2set[rs,rt]}
   | Arithi(_,rt,rs,_) => {def=list2set[rt], use=list2set[rs]}
   | Li(r,_) => {def=list2set[r], use=RegSet.empty}
   | La(r,_) => {def=list2set[r], use=RegSet.empty}
   | Lw(r,(_,ra)) => {def=list2set[r], use=list2set[ra]}
   | Sw(r,(_,ra)) => {def=RegSet.empty, use=list2set[r,ra]}
   | Move(rd,rs) => {def=list2set[rd], use=list2set[rs]}
   | Branchz(_,r,_) => {def=RegSet.empty, use=list2set[r]}
   | Branch(_,r1,r2,_) => {def=RegSet.empty, use=list2set[r1,r2]}
   | Jal(_) => {def=list2set[reg "%eip", reg "%eax"], use=RegSet.empty}
   | Jr(r,also) => {def=RegSet.empty, use=list2set(r::also)}
   | Jalr(r1,r2,use,def) => {def=list2set(r1 :: def), use=list2set(r2::use)}
   | Syscall => ErrorMsg.impossible "Syscall"
   | Nop => {def=RegSet.empty, use=RegSet.empty}
   | J _ => {def=RegSet.empty, use=RegSet.empty}
   | Leave => {def=list2set[reg "%esp", reg "%ebp"], use=list2set[reg "%ebp"]}
   | Ret => {def=RegSet.empty, use=list2set(reg "%eax"::calleeSaved)}
   | Push(r) => {def=RegSet.empty, use=list2set[r]}
   | Pop(r) => {def=list2set[r], use=RegSet.empty}
   | Branch2(c, l) => {def=RegSet.empty, use=RegSet.empty}

  fun syscall_def_use (num: int) : def_use option =
    case num of
      4 => SOME{def=list2set[reg "%eax"], use=list2set[reg "%eax", reg "%ebx", reg "%ecx", reg "%edx"]}
    | 45 => SOME{def=list2set[reg "%eax"], use=list2set[reg "%eax", reg "%ebx"]}
    | 125 => SOME{def=list2set[reg "%eax"], use=list2set[reg "%eax", reg "%ebx", reg "%ecx", reg "%edx"]}
    | _ => NONE

 type allocation = reg RegTb.table

 fun rename_regs (table: allocation) =
 let fun f r = if isvirtual r
	       then case RegTb.look(table,r) of SOME x => (x) 
                  (*  | NONE => ErrorMsg.impossible ("rename_regs: "^
	                              reg2name r)*)
                    | NONE => (r)
	       else r
 in
  fn 
    Arith1 (i, r) => Arith1(i, f r)
   | Arith2(i,rd,rs) => Arith2(i, f rd, f rs)
   | Arith3(i,rd,rs,rt) => Arith3(i, f rd, f rs, f rt)
   | Arithi(i,rt,rs,n) => Arithi(i, f rt, f rs, n)
   | Li(r,n) => Li(f r, n)
   | La(r,lab) => La(f r, lab)
   | Lw(r,(lab,ra)) => Lw(f r, (lab, f ra))
   | Sw(r,(lab,ra)) => Sw(f r, (lab, f ra))
   | Move(rd,rs) => Move(f rd, f rs)
   | Branchz(i,r,lab) => Branchz(i, f r, lab)
   | Branch(i,r1,r2,lab) => Branch(i, f r1, f r2, lab)
   | J(lab) => J lab
   | Jal(lab) => Jal lab
   | Jr(r,also) => Jr(f r, map f also)
   | Jalr(r1,r2,use,def) => Jalr(f r1, f r2, map f use, map f def)
   | Syscall => Syscall
   | Nop => Nop
   | Leave => Leave
   | Ret => Ret
   | Push(r) => Push(f r)
   | Pop(r) => Pop(f r)
   | Branch2(c, l) => Branch2(c, l)
 end

end
