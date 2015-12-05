signature MIPS = sig

  eqtype reg
  val reg : string -> reg (* convert a string such as "$sp" to a register*)
  val reg2name : reg -> string (* convert a register to its name*)
  val reg2int : reg -> int (* convert a register to its number*)
  val isvirtual : reg -> bool (*return true if the argument is a virtual reg*)
  val comparereg : (reg * reg) -> order (* compare two registers*)
 
  val newReg : unit -> reg (* generate a new virtual register *)
  val registers : reg list (* the list of all Mips registers*)

  val callerSaved : reg list (* the list of caller saved regs*)
  val calleeSaved : reg list (* the list of callee saved regs*)
 
  structure RegSet : ORD_SET where type Key.ord_key = reg 
      (* a set structure whose elements are registers*)
  structure RegTb : TABLE where type key = reg
       (*a table structure whose key is register*)

  type lab = Symbol.symbol
  val freshlab : unit -> lab (*generate a new label*)
  val thislab : string -> lab (*generate a label whose name 
                                is the given string*)
  val lab2string : lab -> string (* convert a label its name *)

  type immed 
  val immed : int -> immed (*convert an int to an immediate*)
  val immed2int : immed -> int (* convert an immediate to an int*)
  val compareimmed : (immed * immed) -> order (*compare two immediates*)

  val wordSize : int 
  val wordSizeImmed : immed

  type address = immed * reg 
  val compareaddress : (address * address) -> order

(* reset the internal states of Mips structure*)
  val reset: unit -> unit (* *)


(* Instructions *)

  datatype instruction = 
      Arith2 of aop2 * reg * reg
    | Arith3 of aop3 * reg * reg * reg
    | Arithi of aopi * reg * reg * immed
    | Li of reg * immed
    | La of  reg * lab
    | Lw of reg * address
    | Sw of reg * address
    | Move of reg * reg
    | Branchz of comparecode1 * reg * lab
    | Branchu of comparecode2 * reg * reg * lab
    | Branch of comparecode1 * reg * reg * lab
    | J of lab
    | Jal of lab
    | Jr of reg * reg list
	(* Jr(r1,others) means, jump to the location in r1, at which place
               all the 'others' registers are also live *)
    | Jalr of reg * reg * reg list * reg list
	(* Jalr(r1,r2,use,def) means, jump to the location in r2, saving
            a return address in r1.  At r2, at which place
               all the 'use' registers are also live; 
               and all the 'def' registers
               may be defined before returning to r1 *)
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
  type program = stringblock list *funcode list

(*print a mips program to an output stream*)
  val printAssem: (TextIO.outstream * program) -> unit

  type def_use = {def: RegSet.set, use: RegSet.set}
  val list2set : reg list -> RegSet.set
  val instr_def_use: instruction -> def_use
  val syscall_def_use: int -> def_use option  (* SPIM syscalls *)

  type allocation = reg RegTb.table
  val rename_regs : allocation -> instruction -> instruction

end