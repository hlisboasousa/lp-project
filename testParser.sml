(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fromString "fun f (Int -> Bool g) = if g(1) then 10 else 11; f()";

(* fromString "15";
fromString "true";
fromString "()";
fromString "(6,false)[1]";
fromString "([Bool] [])";
fromString "print x; true";
fromString "3::7::t";
fromString "match x with | 0 -> 1| _ -> -1 end";
fromString "fn (Int x) => -x end";
fromString "var x = 9; x + 3";
fromString "fun f(Int x) = x; f(1)";
fromFile ("example.plc"); *)

(* use "testParserCases.sml"; *)

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)
(* Test cases of testParserCases  *)
(* fun testCases(c) =
   case c of
      (s, e)::[] => fromString s
    | (s, e)::t => testCases(t);

testCases(cases); *)