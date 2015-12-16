structure Compile : sig val compile: string -> unit end =
struct 
  exception Stop
  fun still_ok () = if (!ErrorMsg.anyErrors) then raise Stop else ()

  fun compile filename = 
    let
	val _ = X86.reset()
	val absyn = Parse.parse (filename, TextIO.openIn filename)
        val () = still_ok()
        val () = TypeCheck.tc absyn
        val () = still_ok()
        (* val _ = print "Program successfully typechecked\n" *)
        (* val _ = FunPP.print_prog absyn *)
	val (strBL, funCodeL1) = Codegen.codegen absyn
	val out' = TextIO.openOut (filename^".noregalloc.s")

  val funCodeL = List.map LoopOpt.optimize funCodeL1
	val _ = X86.printAssem(out', (strBL, funCodeL)) 

                before TextIO.closeOut out'
         	handle e => (TextIO.closeOut out'; raise e)

        (* val igraph = Liveness.liveness (strBL, funCodeL) *)

	val funCodeL' = List.map RegAlloc.alloc funCodeL
  val funCodeL' = List.map Deadcode.deadcode_elimination funCodeL'

	val out = TextIO.openOut (filename^".s") 

    in 
	X86.printAssem(out, (strBL, funCodeL')) before TextIO.closeOut out
	handle e => (TextIO.closeOut out; raise e)

    end
 handle ErrorMsg.Error => print "\nCompiler bug.\n\n" 
      | Stop => print "\nCompilation Failed.\n\n" 
end
