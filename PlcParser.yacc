(* Função auxiliar para imprimir Lista e Match *)
fun list2expr (ConI x) = "ConI " ^ Int.toString(x)
  | list2expr (ConB x) = "ConB " ^ Bool.toString(x)
  | list2expr (Prim1(f, x)) = "Prim1(\"" ^ f ^ "\"" ^ ", " ^ list2expr(x) ^ ")"
  | list2expr (Var x) = "Var \""^ x ^ "\""
  | list2expr (List(l)) = listString(list2expr, l)

fun exp2string (ConI x) = "ConI " ^ Int.toString(x)
  | exp2string (ConB x) = "ConB " ^ Bool.toString(x)
  | exp2string (Prim1(f, x)) = "Prim1 (\"" ^ f ^ "\"" ^ ", " ^ exp2string(x) ^ ")"
  | exp2string (Prim2(f, x, y)) = "Prim2 (\"" ^ f ^ "\"" ^ ", " ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ")"
  | exp2string (Item(i, x)) = "Item (" ^ Int.toString(i) ^ ", " ^ exp2string(x) ^ ")"
  | exp2string (If(e1, e2, e3)) = "If (" ^ exp2string(e1) ^ ", " ^ exp2string(e2) ^ ", " ^ exp2string(e3) ^ ")"
  | exp2string (List(l)) = "List [" ^ listString(list2expr, l) ^ "]"
  | exp2string (ESeq(t)) = "ESeq (" ^ type2string(t) ^ ")"
  | exp2string (Var x) = "Var \""^ x ^ "\""
  | exp2string (Match(x, l:(expr option * expr) list)) = "Match (" ^ exp2string(x) ^ ", [" ^ matchString(list2expr, l) ^ "])"
  | exp2string (Call(a1, a2)) = "Call (" ^ exp2string(a1) ^ ", " ^ exp2string(a2)^ ")"
  | exp2string (Let(s, e1, e2)) = "Let (\"" ^ s ^ "\", " ^ exp2string(e1) ^ ", " ^ exp2string(e2) ^ ")"
  | exp2string (Anon(p, s, e)) = "Anon (" ^ type2string(p) ^ ", \"" ^ s ^ "\", " ^ exp2string(e) ^ ")"
  | exp2string (Letrec(f, t, a, r, e1, e2)) = "Letrec (\"" ^ f ^ "\", " ^ type2string(t) ^ ", \"" ^ a ^ "\", " ^ type2string(r)
       ^ ", \n\t" ^ exp2string(e1)
       ^ ", \n\t" ^ exp2string(e2) ^ ")"
       
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
        | NE | LT | LE | TWOCOLON | SEMICOLON | COLON | COMMA | NAT of int | LBRACKET | RBRACKET
        | NAME of string | LBRACE | RBRACE | LPAREN | RPAREN | ANON | ARROW | END | TRUE of bool
        | FALSE of bool | NIL | BOOL | INT | WITH | EOF | FUNT | F | UNDERSCORE | PIPE


%nonterm  Init | Prog of expr | Decl of expr | Expr of expr | AtomicExpr of expr | AppExpr of expr | Const of expr | Comps of expr | MatchExpr of (expr option * expr) list
        | CondExpr of expr option | Args of (plcType * string) list | Params of (plcType * string) list
        | TypedVar of (plcType * string) | Type of plcType | AtomicType of plcType | Types of plcType list 
        
%eop EOF

%noshift EOF

%start Init

%%
Init   :    Prog                 (print(exp2string(Prog)^"\n"))

Prog    :   Expr                 (Expr)
        |   Decl                 (Decl)

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
        |    LBRACE Prog RBRACE                 (Prog)
        |    LPAREN Expr RPAREN                 (Expr)
        |    LPAREN Comps RPAREN                (List([Comps]))
        |    ANON Args ARROW Expr END           (makeAnon(Args, Expr))

Const:     NAT                                   (ConI(NAT))
        |  TRUE                                  (ConB(TRUE))
        |  FALSE                                 (ConB(FALSE))
        | LPAREN RPAREN                          (List([]))
        | LPAREN Type LBRACKET RBRACKET RPAREN   (ESeq(Type))

Comps:      Expr COMMA Expr             (List([Expr1, Expr2]))
       |    Expr COMMA Comps            (List([Expr, Comps]))

MatchExpr: END                               ([])
       |   PIPE CondExpr FUNT Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr: Expr          (SOME Expr)
       | UNDERSCORE     (NONE)

AppExpr :   AtomicExpr AtomicExpr  (Call(AtomicExpr1, AtomicExpr2))
        |   AppExpr AtomicExpr     (Call(AppExpr, AtomicExpr))

Decl    :   VAR NAME EQ Expr SEMICOLON Expr                    (Let(NAME, Expr1, Expr2))
        |   FUN NAME Args EQ Expr SEMICOLON Expr               (Let(NAME, makeAnon(Args, Expr1), Expr2))
        |   FUNREC NAME Args COLON Type EQ Expr SEMICOLON Expr (makeFun(NAME, Args, Type, Expr1, Expr2))


Args:      LPAREN RPAREN        ([])
       |   LPAREN Params RPAREN (Params)

Params:    TypedVar ([TypedVar])
       |   TypedVar COMMA Params (TypedVar::Params)

TypedVar:  Type NAME ((Type, NAME))

Type:     AtomicType             (AtomicType)
       |  LPAREN Types RPAREN    (ListT(Types))
       |  LBRACKET Type RBRACKET (SeqT(Type))
       |  Type FUNT Type         (FunT(Type1, Type2))

AtomicType: NIL                 (ListT[])
       |    BOOL                (BoolT)
       |    INT                 (IntT)
       |    LPAREN Type RPAREN  (Type)

Types:   Type COMMA Type        ([Type1, Type2])
       | Type COMMA Types       (Type::Types)

