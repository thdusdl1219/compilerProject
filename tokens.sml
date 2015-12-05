structure Tokens : Fun_TOKENS =
struct
  (* A "scaffold" structure for debugging lexers. *)
type ('svalue,'pos) token = string

type svalue = unit

fun print_token (pos,s) = 
 let val save = !ErrorMsg.anyErrors
  in ErrorMsg.error(pos,s); ErrorMsg.anyErrors := save; s
 end
 
fun ARROW(p) = print_token(p,"ARROW")
fun FUN(p) = print_token(p,"FUN")
fun IN(p) = print_token(p,"IN")
fun LET(p) = print_token(p,"LET")
fun ELSE(p) = print_token(p,"ELSE")
fun THEN(p) = print_token(p,"THEN")
fun IF(p) = print_token(p,"IF")
fun ASSIGN(p) = print_token(p,"ASSIGN")
fun BANG(p) = print_token(p,"BANG")
fun REF(p) = print_token(p,"REF")
fun DO(p) = print_token(p,"DO")
fun WHILE(p) = print_token(p,"WHILE")
fun OR(p) = print_token(p,"OR")
fun NOT(p) = print_token(p,"NOT")
fun AND(p) = print_token(p,"AND")
fun GT(p) = print_token(p,"GT")
fun EQ(p) = print_token(p,"EQ")
fun LT(p) = print_token(p,"LT")
fun PROJ(c,p1,p2) = print_token((p1,p2),"PROJ("^Int.toString(c)^")") 
fun TIMES(p) = print_token(p,"TIMES")
fun MINUS(p) = print_token(p,"MINUS")
fun PLUS(p) = print_token(p,"PLUS")
fun RPAREN(p) = print_token(p,"RPAREN")
fun LPAREN(p) = print_token(p,"LPAREN")
fun COLON(p) = print_token(p,"COLON")
fun SEMICOLON(p) = print_token(p,"SEMICOLON")
fun COMMA(p) = print_token(p,"COMMA")
fun ID(s,p1,p2) = print_token((p1,p2),"ID("^s^")")
fun INT(c,p1,p2) = print_token((p1,p2),"INT("^Int.toString(c)^")")
fun EOF(p) = print_token(p,"EOF")

end
