
fun exp2string (atomicExpr) = "atomicExpr ^ "
  | exp2string (Add(x, y)) = "Add(" ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ")"
  | exp2string (Sub(x, y)) = "Sub(" ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ")"
  | exp2string (Mul(x, y)) = "Mul(" ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ")"
  | exp2string (Div(x, y)) = "Div(" ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ")"
  
fun decl2string (Var(name)) = "IConst(" ^ Int.toString(i) ^ ")"
  | decl2string (Add(x, y)) = "Add(" ^ decl2string(x) ^ ", " ^ decl2string(y) ^ ")"
  | decl2string (Sub(x, y)) = "Sub(" ^ decl2string(x) ^ ", " ^ decl2string(y) ^ ")"
  | decl2string (Mul(x, y)) = "Mul(" ^ decl2string(x) ^ ", " ^ decl2string(y) ^ ")"
  | decl2string (Div(x, y)) = "Div(" ^ decl2string(x) ^ ", " ^ decl2string(y) ^ ")"

%%

%name PlcParser

%pos int

%term   VAR | FUN | FUNREC | IF | THEN | ELSE | MATCH | EXCLAMATION | MINUS
        | HEAD | TAIL | ISEMPTY | PRINT | AND | OR | PLUS | TIMES | DIV | EQ
        | NE | LT | LE | TWOCOLON | SEMICOLON | COMMA | NAT | LBRACKET | RBRACKET
        | NAME of string | LBRACE | RBRACE | LPAREN | RPAREN | FUNANON | ARROW | END | TRUE
        | FALSE | NIL | BOOL | INT | WITH


%nonterm  prog | decl | Expr | atomicExpr | appExpr | const | comps | matchExpr
        | condExpr | args | params | typedVar | type | atomicType | types 
        


Prog    :   Expr        (print (Expr2string(Expr)^"\n" ))
        |   decl        (print (decl2string(decl)^"\n" ))
        ;

Decl    :   VAR NAME EQ Expr                    (Let(NAME, Expr SEMICOLON)

Expr    :   atomicExpr                          (atomicExpr)
        |   appExpr                             (appExpr)
        |   IF Expr THEN Expr ELSE Expr         (If(Expr, Expr, Expr))
        |   MATCH Expr WITH matchExpr           (Match(Expr, matchExpr))
        |   EXCLAMATION Expr                    (Prim1("!", Expr))
        |   MINUS Expr                          (Prim1("-", Expr))
        |   HEAD Expr                           (Prim1("hd", Expr))
        |   TAIL Expr                           (Prim1("tl", Expr))
        |   ISEMPTY Expr                        (Prim1("ise", Expr))
        |   PRINT Expr                          (Prim1("print", Expr))
        |   Expr AND Expr                       (Prim2("AND", Expr, Expr))
        |   Expr PLUS Expr                      (Prim2("+", Expr, Expr))
        |   Expr MINUS Expr                     (Prim2("-", Expr, Expr))
        |   Expr TIMES Expr                     (Prim2("*", Expr, Expr))
        |   Expr DIV Expr                       (Prim2("div", Expr, Expr))
        |   Expr EQ Expr                        (Prim2("=", Expr, Expr))
        |   Expr NE Expr                        (Prim2("!=", Expr, Expr))
        |   Expr LT Expr                        (Prim2("<", Expr, Expr))
        |   Expr LE Expr                        (Prim2("<=", Expr, Expr))
        |   Expr TWOCOLON Expr                  (Prim2("::", Expr, Expr))
        |   Expr SEMICOLON Expr                 (Prim2(";", Expr, Expr))
        |   Expr LBRACKET NAT RBRACKET          ()
        ;

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

Types



%eop EOF

%noshift EOF

%start Prog

%%
