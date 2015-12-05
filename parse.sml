structure Parse : sig 
    val parse : (string * TextIO.instream) -> Absyn.prog  
end =
struct 
  structure FunLrVals = FunLrValsFun(structure Token = LrParser.Token)
  structure FunLex = FunLexFun(structure Tokens = FunLrVals.Tokens)
  structure FunParser = Join(structure ParserData = FunLrVals.ParserData
			structure Lex=FunLex
			structure LrParser = LrParser)

  fun parse (filename, file) =
      let val _ = (ErrorMsg.reset filename)
	  fun get _ = TextIO.input file
	  fun parseerror(s,p1,p2) = ErrorMsg.error ((p1,p2), s)
	  val lexer = LrParser.Stream.streamify (FunLex.makeLexer get)
	  val (absyn, _) = FunParser.parse(30,lexer,parseerror,())
       in TextIO.closeIn file;
	   absyn
      end handle LrParser.ParseError => raise ErrorMsg.Error

end

