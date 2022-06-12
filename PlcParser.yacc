(* Função auxiliar para imprimir Lista *)
fun list2expr (ConI x) = "ConI " ^ Int.toString(x)
  | list2expr (ConB x) = "ConB " ^ Bool.toString(x)
  | list2expr (Prim1(f, x)) = "Prim1(\"" ^ f ^ "\"" ^ ", " ^ list2expr(x) ^ ")"
  | list2expr (List(l)) = listString(list2expr, l)

fun exp2string (ConI x) = "ConI " ^ Int.toString(x)
  | exp2string (ConB x) = "ConB " ^ Bool.toString(x)
  | exp2string (Prim1(f, x)) = "Prim1(\"" ^ f ^ "\"" ^ ", " ^ exp2string(x) ^ ")"
  | exp2string (Prim2(f, x, y)) = "Prim2(\"" ^ f ^ "\"" ^ ", " ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ")"
  | exp2string (Item(i, x)) = "Item(" ^ Int.toString(i) ^ ", " ^ exp2string(x) ^ ")"
  | exp2string (If(e1, e2, e3)) = "If(" ^ exp2string(e2) ^ ", then " ^ exp2string(e2) ^ ", else " ^ exp2string(e3) ^ ")"
  | exp2string (List(l)) = "List[" ^ listString(list2expr, l) ^ "]"
  | exp2string (Var x) = "Var \""^ x ^ "\""
  | exp2string (Match(x, (option,exp)::t)) = "Match(" ^ exp2string(x) ^ ")"
  | exp2string (Call(a1, a2)) = "Call(" ^ exp2string(a1) ^ ", " ^ exp2string(a2)^ ")"

fun decl2string (Let(x, expr1, prog)) = "Let(\"" ^ x ^ "\", " ^ exp2string(expr1) ^ " " ^ exp2string(prog) ")"

       

%%

%name PlcParser

%pos int

%right SEMICOLON FUNT
%nonassoc IF
%left ELSE
%left AND
%left EQ NE
%left LT LE
%right TWOCOLON
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT HEAD TAIL ISEMPTY PRINT F
%left LBRACKET

%term   VAR | FUN | FUNREC | IF | THEN | ELSE | MATCH | EXCLAMATION | MINUS
        | HEAD | TAIL | ISEMPTY | PRINT | AND | OR | NOT | PLUS | TIMES | DIV | EQ
        | NE | LT | LE | TWOCOLON | SEMICOLON | COMMA | NAT of int | LBRACKET | RBRACKET
        | NAME of string | LBRACE | RBRACE | LPAREN | RPAREN | ANON | ARROW | END | TRUE of bool
        | FALSE of bool | NIL | BOOL | INT | WITH | EOF | FUNT | F | UNDERSCORE | PIPE


%nonterm  Prog | Decl | Expr of expr | AtomicExpr of expr | AppExpr of expr | Const of expr | Comps of expr | MatchExpr of (expr option * expr) list
        | CondExpr of expr option | Args | Params | TypedVar | Type | AtomicType | Types 
        
%eop EOF

%noshift EOF

%start Prog

%%

Prog    :   Expr                 (print (exp2string(Expr)^"\n" ))
        |   Decl SEMICOLON Prog  (print (decl2string(Decl, Prog)^"\n" ))

Expr    :   AtomicExpr                          (AtomicExpr)
        |   AppExpr                             (AppExpr)
        |   IF Expr THEN Expr ELSE Expr         (If(Expr1, Expr2, Expr3))
        |   MATCH Expr WITH MatchExpr           (Match(Expr, MatchExpr))
        |   EXCLAMATION Expr                    (Prim1("!", Expr))
        |   MINUS Expr                          (Prim1("-", Expr))
        |   HEAD Expr                           (Prim1("hd", Expr))
        |   TAIL Expr                           (Prim1("tl", Expr))
        |   ISEMPTY Expr                        (Prim1("ise", Expr))
        |   PRINT Expr                          (Prim1("print", Expr))
        |   Expr NOT Expr                       (Prim1("NOT", Expr))
        |   Expr AND Expr                       (Prim2("AND", Expr1, Expr2))
        |   Expr OR Expr                        (Prim2("OR", Expr1, Expr2))
        |   Expr AND Expr                       (Prim2("AND", Expr1, Expr2))
        |   Expr PLUS Expr                      (Prim2("+", Expr1, Expr2))
        |   Expr MINUS Expr                     (Prim2("-", Expr1, Expr2))
        |   Expr TIMES Expr                     (Prim2("*", Expr1, Expr2))
        |   Expr DIV Expr                       (Prim2("div", Expr1, Expr2))
        |   Expr EQ Expr                        (Prim2("=", Expr1, Expr2))
        |   Expr NE Expr                        (Prim2("!=", Expr1, Expr2))
        |   Expr LT Expr                        (Prim2("<", Expr1, Expr2))
        |   Expr LE Expr                        (Prim2("<=", Expr1, Expr2))
        |   Expr TWOCOLON Expr                  (Prim2("::", Expr1, Expr2))
        |   Expr SEMICOLON Expr                 (Prim2(";", Expr1, Expr2))
        |   Expr LBRACKET NAT RBRACKET          (Item(NAT, Expr))

AtomicExpr : Const                              (Const)
        |    NAME                               (Var(NAME))
        (* |    LBRACE Prog RBRACE                 (Prog) *)
        |    LPAREN Expr RPAREN                 (Expr)
        |    LPAREN Comps RPAREN                (List([Comps]))
        (* |    FUN Args ARROW Expr END            (makeAnon(Args, Expr)) *)

Const:     NAT     (ConI(NAT))
        |  TRUE    (ConB(TRUE))
        |  FALSE   (ConB(FALSE))

Comps:      Expr COMMA Expr             (List([Expr1, Expr2]))
       |    Expr COMMA Comps            (List([Expr, Comps]))

MatchExpr: END                               ([])
       |   PIPE CondExpr FUNT Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr: Expr          (SOME Expr)
       | UNDERSCORE     (NONE)

AppExpr :   AtomicExpr AtomicExpr  (Call(AtomicExpr1, AtomicExpr2))
        |   AppExpr AtomicExpr     (Call(AppExpr, AtomicExpr))

Decl    :   VAR NAME EQ Expr       (Let(NAME, Expr, Expr))


(* Args:      LPAREN RPAREN        ()
       |   LPAREN Params RPAREN (Params)

Params:    TypedVar (TypedVar)
       |   TypedVar COMMA Params (ListT(TypedVar, Params))

TypedVar:  Type NAME ()

Type:     AtomicType             (AtomicType)
       |  LPAREN Types RPAREN    (ListT(Types))
       |  LBRACKET Type RBRACKET (SeqT(Type))
       |  Type FTYPE Type        (FunT(Type1, Type2))

AtomicType: NIL                 ()
       |    BOOL                (BoolT)
       |    INT                 (IntT)
       |    LPAREN Type RPAREN  (Type)

Types:   Type COMMA Type        (ListT(Type1, Type2))
       | Type COMMA Types       (ListT(Type, Types)) *)

        (* |   Expr LBRACKET NAT RBRACKET          ()
 *)

