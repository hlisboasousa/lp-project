(* Plc Parser Aux *)

(* Convert a list to string *)
fun listString (conv, l) =
  case l of
      [] => ""
    | h::[] => conv(h)
    | h::ts =>
        conv(h) ^ "; " ^ listString (conv, ts);

(* Convert a match to string *)
fun matchString (conv, (option,exp)::t) =
    case option of
      SOME option => "(SOME (" ^ conv(option) ^ "), " ^ conv(exp)  ^ "), " ^ matchString(conv, t)
    | NONE => "(NONE, " ^ conv(exp) ^ ")"

fun reverse nil = nil | reverse (x::xs) = (reverse xs) @ [x];

(* Creat the body of a function expression. *)
fun makeFunAux (n: int, xs: (plcType * string) list, e: expr): expr =
    case xs of
       (_, s)::[]  => Let(s, Item(n, Var "$list"), e)
     | (_, s)::t   => Let(s, Item(n, Var "$list"), makeFunAux(n+1, t, e)); 

(* Create the list of arguments of a function. *)
fun makeType (args: (plcType * string) list, acc: plcType list): plcType =
    case args of
        []        => ListT(reverse acc)
      | (i, _)::t => makeType(t, i::acc);

(* Create a function expression. *)
fun makeFun (f: string, xs: (plcType * string) list, rt: plcType, e1: expr, e2: expr): expr =
  case xs of
      [] => Letrec(f, ListT [], "()", rt, e1, e2)
    | (t,x)::[] => Letrec(f, t, x, rt, e1, e2)
    | _ =>
      let
        val t = makeType (xs, [])
        val e1' = makeFunAux (1, xs, e1)
      in
        Letrec(f, t, "$list", rt, e1', e2)
      end;

(* Create a Anonymus function expression. *)
fun makeAnon (xs:(plcType * string) list, e:expr):expr =
  case xs of
      [] => Anon(ListT [], "()", e)
    | (t,x)::[] => Anon(t,x,e)
    | _ =>
      let
        val t = makeType(xs, [])
      in
        let
          val e' = makeFunAux (1, xs, e)
        in
          Anon(t,"$list",e')
        end
      end;