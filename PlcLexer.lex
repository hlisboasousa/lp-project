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
%%
\n       => (pos := (!pos) + 1; Tokens.EOF(!pos, !pos));
";"       => (Tokens.SEMICOLON(!pos, !pos));
{ws}+    => (lex());
{digit}+ => (Tokens.NAT (valOf (Int.fromString yytext), !pos, !pos));

"+"      => (Tokens.PLUS(!pos,!pos));
"*"       => (Tokens.TIMES(!pos,!pos));

"-"      => (Tokens.MINUS(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));

"print" => (Tokens.PRINT(!pos, !pos));