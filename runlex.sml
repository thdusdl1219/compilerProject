structure RunLex =
struct 
  structure FunLex = FunLexFun(structure Tokens = Tokens)

  fun runlex filename =
      let val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
          val () = ErrorMsg.reset filename
	  val lexer = FunLex.makeLexer get
	  fun do_it() =
	      let val t = lexer()
	       in if t="EOF" then () else do_it()
	      end
       in do_it() handle exn => (TextIO.closeIn file; raise exn);
	  TextIO.closeIn file
      end

end

