(* Plc Parser Aux *)

(* Parse a list to string *)
fun listString (conv, l) =
  case l of
      [] => ""
    | h::[] => conv(h)
    | h::ts =>
        conv(h) ^ "; " ^ listString (conv, ts);

(* Parse a match to string *)
fun matchString (conv, (option,exp)::t) =
    case option of
      SOME option => "(Some (" ^ conv(option) ^ "), " ^ conv(exp)  ^ ");" ^ matchString(conv, t)
    | NONE => "(None, " ^ conv(exp) ^ ")"

(* Creat the body of a function expression. *)
fun makeFunAux (n: int, xs: (plcType * string) list, e: expr): expr =
    e (* TODO *)

(* Create the list of arguments of a function. *)
fun makeType (args: (plcType * string) list): plcType =
    ListT [];
    (* case args of
      IntT i          => i
      | BoolT b       => b
      | FunT (a, b)   => FunT(makeType a, makeType b)
      | ListT h::t    => ListT([makeType h, makeType t])
      | SeqT s        => SeqT(makeType s); *)

(* Create a function expression. *)
fun makeFun (f: string, xs: (plcType * string) list, rt: plcType, e1: expr, e2: expr): expr =
  case xs of
      [] => Letrec(f, ListT [], "()", rt, e1, e2)
    | (t,x)::[] => Letrec(f, t, x, rt, e1, e2)
    | _ =>
      let
        val t = makeType xs
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
        val t = makeType xs
      in
        let
          val e' = makeFunAux (1, xs, e)
        in
          Anon(t,"$list",e')
        end
      end;