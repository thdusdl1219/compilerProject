type pos = ErrorMsg.pos
type svalue = Tokens.svalue
type ('svalue,'pos) token = ('svalue,'pos) Tokens.token
type lexresult  = (svalue,ErrorMsg.pos) token

val newLine = ErrorMsg.newLine
val save = ref 0
val com = ref 0
fun make_pos (yypos,yytext) : ErrorMsg.pos2
    = (yypos, yypos + String.size(yytext) - 1)

(* Handling EOF.  Note that this function reports the wrong file-position for
   end-of-file.  Because of a design infelicity of ML-Lex, it's possible but
   not easy to get access to the correct file position.  There is a way to 
   do it using the %arg feature of ML-Lex, but you don't need to bother 
   with it for this exercise. 
*)
fun eof () = 
     let val () = () in if (!com = 1) then raise Fail "comment is not matched"
     else Tokens.EOF(0,0) end

%%

%s COMMENT;
%header (functor FunLexFun(structure Tokens: Fun_TOKENS));

alpha = [A-Za-z];
proj = 0 | [1-9][0-9]*;
id = [A-Za-z][A-Za-z0-9_]*;
num = [0-9]+;
%%
<INITIAL>"/*"  => (YYBEGIN COMMENT;  continue());
<INITIAL>"*/"  => (raise Fail "comment is not matched");
<COMMENT>"/*"  => (let val () = save := (!save + 1); val () = com := 1 in continue() end); 
<COMMENT>"*/"  => (let val r = !save in if r = 0 then let val () = save := 0 in com := 0; YYBEGIN INITIAL; continue() end else let val () = save := (!save - 1) in YYBEGIN COMMENT; continue() end end);
<COMMENT>\n    => (newLine yypos; continue ());
<COMMENT>.     => (continue());
<INITIAL>\-\>  => (Tokens.ARROW(make_pos(yypos,yytext)));
<INITIAL>fun   => (Tokens.FUN(make_pos(yypos,yytext)));
<INITIAL>in    => (Tokens.IN(make_pos(yypos,yytext)));
<INITIAL>let   => (Tokens.LET(make_pos(yypos,yytext)));
<INITIAL>else  => (Tokens.ELSE(make_pos(yypos,yytext)));
<INITIAL>then  => (Tokens.THEN(make_pos(yypos,yytext)));
<INITIAL>if    => (Tokens.IF(make_pos(yypos,yytext)));
<INITIAL>\:\=  => (Tokens.ASSIGN(make_pos(yypos,yytext)));
<INITIAL>\!    => (Tokens.BANG(make_pos(yypos,yytext)));
<INITIAL>ref   => (Tokens.REF(make_pos(yypos,yytext)));
<INITIAL>do    => (Tokens.DO(make_pos(yypos,yytext)));
<INITIAL>while => (Tokens.WHILE(make_pos(yypos,yytext)));
<INITIAL>\|\|  => (Tokens.OR(make_pos(yypos,yytext)));
<INITIAL>not   => (Tokens.NOT(make_pos(yypos,yytext)));
<INITIAL>\&    => (Tokens.AND(make_pos(yypos,yytext)));
<INITIAL>\>    => (Tokens.GT(make_pos(yypos,yytext)));
<INITIAL>\=    => (Tokens.EQ(make_pos(yypos,yytext)));
<INITIAL>\<    => (Tokens.LT(make_pos(yypos,yytext)));
<INITIAL>\#{proj}=> (let val (yypos, yypos2) = make_pos(yypos, yytext) in let val num =(foldr (fn(a,r)=>(ord(a)-ord(#"0")+10*r)) 0 (rev(tl (explode yytext)))) in Tokens.PROJ(num,yypos,yypos2) end end);
<INITIAL>\*    => (Tokens.TIMES(make_pos(yypos,yytext)));
<INITIAL>\-    => (Tokens.MINUS(make_pos(yypos,yytext)));
<INITIAL>\+    => (Tokens.PLUS(make_pos(yypos,yytext)));
<INITIAL>\)    => (Tokens.RPAREN(make_pos(yypos,yytext)));
<INITIAL>\(    => (Tokens.LPAREN(make_pos(yypos,yytext)));
<INITIAL>\:    => (Tokens.COLON(make_pos(yypos,yytext)));
<INITIAL>\;    => (Tokens.SEMICOLON(make_pos(yypos,yytext)));
<INITIAL>\,    => (Tokens.COMMA(make_pos(yypos,yytext)));
<INITIAL>{id}  => (let val (yypos, yypos2) = make_pos(yypos, yytext) in (Tokens.ID(yytext,yypos,yypos2)) end);
<INITIAL>{num} => (let val (yypos, yypos2) = make_pos(yypos, yytext) in let val num =(foldr (fn(a,r)=>(ord(a)-ord(#"0")+10*r)) 0 (rev(explode yytext))) in Tokens.INT(num,yypos,yypos2) end end);
<INITIAL>\n    => (newLine yypos; continue ());
<INITIAL>(" "|\t|\r)   => (continue ());
