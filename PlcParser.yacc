
fun exp2string (ConI x) = "ConI " ^ Int.toString(x)
  | exp2string (Prim1(f, x)) = "Prim1(\"" ^ f ^ "\"" ^ ", " ^ exp2string(x) ^ ")"
  | exp2string (Prim2(f, x, y)) = "Prim2(\"" ^ f ^ "\"" ^ ", " ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ")"
  
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
        | NAME of string | LBRACE | RBRACE | LPAREN | RPAREN | ANON | ARROW | END | TRUE
        | FALSE | NIL | BOOL | INT | WITH | EOF | FUNT | F


%nonterm  Prog | decl | Expr of expr | AtomicExpr of expr | AppExpr | Const of expr | Comps | MatchExpr
        | CondExpr | Args | Params | TypedVar | Type | AtomicType | Types 
        
%eop EOF

%noshift EOF

%start Prog

%%

Prog    :   Expr        (print (exp2string(Expr)^"\n" ))

Expr    :   AtomicExpr                          (AtomicExpr)
        (* |   AppExpr                             (AppExpr) *)
        |   IF Expr THEN Expr ELSE Expr         (If(Expr1, Expr2, Expr3))
        (* |   MATCH Expr WITH MatchExpr           (Match(Expr, MatchExpr)) *)
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

AtomicExpr : Const              (Const)
        |    NAME               (Var NAME)

Const:  NAT     (ConI(NAT))

        (* |   Expr LBRACKET NAT RBRACKET          ()

AtomicExpr    :   const         (const)
              |   name          (Var name)

AppExpr

Const

Comps

MatchExpr

CondExpr

Args

Params

TypeVar

Type

AtomicType

Types *)

