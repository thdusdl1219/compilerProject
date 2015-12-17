structure Compile : sig val compile: string -> unit end =
struct 
  exception Stop
  fun still_ok () = if (!ErrorMsg.anyErrors) then raise Stop else ()


  fun compile filename = 
    let
      fun intermidiate( strBL, funcode, str ) =
        let val outfile = TextIO.openOut (filename^str)
        in
          X86.printAssem(outfile, (strBL, funcode)) before TextIO.closeOut outfile
          handle e => (TextIO.closeOut outfile; raise e)
        end

    	val _ = X86.reset()
    	val absyn = Parse.parse (filename, TextIO.openIn filename)
            val () = still_ok()
            val () = TypeCheck.tc absyn
            val () = still_ok()
            (* val _ = print "Program successfully typechecked\n" *)
            (* val _ = FunPP.print_prog absyn *)
    	val (strBL, funCodeL) = Codegen.codegen absyn
    	val _ = intermidiate( strBL, funCodeL, ".noloopopt.1.s")

        (*Loop Optimization*)

      val funCodeL' = List.map LoopOpt.optimize funCodeL
      val _ = intermidiate( strBL, funCodeL', ".noregalloc.s")
           
      (* val _ = intermidiate( strBL, funCodeL', ".noregalloc.3.s") *)


        (* Register Allocation *)

    	val funCodeL' = List.map RegAlloc.alloc funCodeL'
      val funCodeL' = List.map Deadcode.deadcode_elimination funCodeL'
      
        (* Dead Code Elimination *)
      
      (*val funCodeL' = List.map Deadcode.deadcode_elimination funCodeL'
      val _ = intermidiate( strBL, funCodeL', ".noregalloc.3.s")*)

	   
     val out = TextIO.openOut (filename^".s") 
    in 
    	X86.printAssem(out, (strBL, funCodeL')) before TextIO.closeOut out
    	handle e => (TextIO.closeOut out; raise e)
    end
 handle ErrorMsg.Error => print "\nCompiler bug.\n\n" 
      | Stop => print "\nCompilation Failed.\n\n" 
end
