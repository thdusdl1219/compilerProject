signature Fun_TOKENS =
sig
type ('svalue,'pos) token
type svalue

val ARROW:  ErrorMsg.pos * ErrorMsg.pos -> ('svalue,'pos) token
val FUN:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val IN:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val LET:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val ELSE:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val THEN:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val IF:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val ASSIGN:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val BANG:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val REF:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val DO:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val WHILE:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val OR:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val NOT:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val AND:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val GT:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val EQ:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val LT:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val PROJ: int * ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val TIMES:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val MINUS:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val PLUS:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val RPAREN:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val LPAREN:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val COLON:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val SEMICOLON:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val COMMA:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val ID: string * ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val INT: int * ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
val EOF:  ErrorMsg.pos * ErrorMsg.pos ->  ('svalue,'pos) token
end







