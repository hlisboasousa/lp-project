(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

digit=[0-9];
ws = [\ \t];
alpha=[A-Za-z][A-Za-z0-9]*;
%%
\n          => (pos := (!pos) + 1; Tokens.EOF(!pos, !pos));
{ws}+       => (lex());
{digit}+    => (Tokens.NAT (valOf (Int.fromString yytext), !pos, !pos));
"+"         => (Tokens.PLUS(!pos,!pos));
"*"         => (Tokens.TIMES(!pos,!pos));
";"         => (Tokens.SEMICOLON(!pos, !pos));
","         => (Tokens.COMMA(!pos, !pos));
"-"         => (Tokens.MINUS(!pos,!pos));
"/"         => (Tokens.DIV(!pos,!pos));
"("         => (Tokens.LPAREN(!pos,!pos));
")"         => (Tokens.RPAREN(!pos,!pos));
"!"         => (Tokens.EXCLAMATION(!pos,!pos));
"hd"        => (Tokens.HEAD(!pos,!pos));
"tl"        => (Tokens.TAIL(!pos,!pos));
"ise"       => (Tokens.ISEMPTY(!pos,!pos));
"print"     => (Tokens.PRINT(!pos,!pos));
"AND"       => (Tokens.AND(!pos,!pos));
"OR"        => (Tokens.OR(!pos,!pos));
"NOT"       => (Tokens.NOT(!pos,!pos));
"="         => (Tokens.EQ(!pos,!pos));
"!="        => (Tokens.NE(!pos,!pos));
"<"         => (Tokens.LT(!pos,!pos));
"<="        => (Tokens.LE(!pos,!pos));
"["         => (Tokens.LBRACKET(!pos,!pos));
"]"         => (Tokens.RBRACKET(!pos,!pos));
"{"         => (Tokens.LBRACE(!pos,!pos));
"}"         => (Tokens.RBRACE(!pos,!pos));
"::"        => (Tokens.TWOCOLON(!pos,!pos));
"if"        => (Tokens.IF(!pos,!pos));
"else"      => (Tokens.ELSE(!pos,!pos));
"then"      => (Tokens.THEN(!pos,!pos));
"match"     => (Tokens.MATCH(!pos,!pos));
"with"      => (Tokens.WITH(!pos,!pos));
"var"       => (Tokens.VAR(!pos,!pos));
"fun"       => (Tokens.FUN(!pos,!pos));
"fun rec"   => (Tokens.FUNREC(!pos,!pos));
"fn"        => (Tokens.ANON(!pos, !pos));
"=>"        => (Tokens.ARROW(!pos,!pos));
"print"     => (Tokens.PRINT(!pos, !pos));
"end"       => (Tokens.END(!pos, !pos));
"false"     => (Tokens.FALSE(valOf (Bool.fromString yytext), !pos, !pos));
"true"      => (Tokens.TRUE(valOf (Bool.fromString yytext), !pos, !pos));
"nil"       => (Tokens.NIL(!pos, !pos));
"|"         => (Tokens.PIPE(!pos, !pos));
"_"         => (Tokens.UNDERSCORE(!pos, !pos));
"->"        => (Tokens.FUNT(!pos, !pos));
{alpha}+    => (Tokens.NAME(yytext, !pos, !pos));
