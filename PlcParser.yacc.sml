functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
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

       


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\021\000\004\000\020\000\007\000\019\000\008\000\018\000\
\\009\000\017\000\010\000\016\000\011\000\015\000\012\000\014\000\
\\013\000\013\000\027\000\012\000\030\000\011\000\033\000\010\000\
\\038\000\009\000\039\000\008\000\000\000\
\\001\000\004\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\027\000\012\000\030\000\011\000\033\000\010\000\038\000\009\000\
\\039\000\008\000\000\000\
\\001\000\004\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\027\000\012\000\030\000\011\000\033\000\010\000\038\000\009\000\
\\039\000\008\000\047\000\081\000\000\000\
\\001\000\005\000\096\000\006\000\096\000\009\000\096\000\014\000\096\000\
\\015\000\035\000\017\000\096\000\018\000\096\000\019\000\096\000\
\\020\000\096\000\021\000\096\000\022\000\096\000\023\000\096\000\
\\024\000\096\000\025\000\096\000\026\000\096\000\028\000\024\000\
\\034\000\096\000\037\000\096\000\043\000\096\000\044\000\096\000\
\\045\000\096\000\048\000\096\000\000\000\
\\001\000\005\000\097\000\006\000\097\000\009\000\097\000\014\000\097\000\
\\015\000\035\000\017\000\097\000\018\000\097\000\019\000\097\000\
\\020\000\097\000\021\000\097\000\022\000\097\000\023\000\097\000\
\\024\000\097\000\025\000\097\000\026\000\097\000\028\000\024\000\
\\034\000\097\000\037\000\097\000\043\000\097\000\044\000\097\000\
\\045\000\097\000\048\000\097\000\000\000\
\\001\000\005\000\098\000\006\000\098\000\009\000\098\000\014\000\098\000\
\\015\000\035\000\017\000\098\000\018\000\098\000\019\000\098\000\
\\020\000\098\000\021\000\098\000\022\000\098\000\023\000\098\000\
\\024\000\098\000\025\000\098\000\026\000\098\000\028\000\024\000\
\\034\000\098\000\037\000\098\000\043\000\098\000\044\000\098\000\
\\045\000\098\000\048\000\098\000\000\000\
\\001\000\005\000\099\000\006\000\099\000\009\000\099\000\014\000\099\000\
\\015\000\035\000\017\000\099\000\018\000\099\000\019\000\099\000\
\\020\000\099\000\021\000\099\000\022\000\099\000\023\000\099\000\
\\024\000\099\000\025\000\099\000\026\000\099\000\028\000\024\000\
\\034\000\099\000\037\000\099\000\043\000\099\000\044\000\099\000\
\\045\000\099\000\048\000\099\000\000\000\
\\001\000\005\000\100\000\006\000\100\000\009\000\100\000\014\000\100\000\
\\015\000\035\000\017\000\100\000\018\000\100\000\019\000\100\000\
\\020\000\100\000\021\000\100\000\022\000\100\000\023\000\100\000\
\\024\000\100\000\025\000\100\000\026\000\100\000\028\000\024\000\
\\034\000\100\000\037\000\100\000\043\000\100\000\044\000\100\000\
\\045\000\100\000\048\000\100\000\000\000\
\\001\000\005\000\101\000\006\000\101\000\009\000\037\000\014\000\101\000\
\\015\000\035\000\016\000\034\000\017\000\033\000\018\000\032\000\
\\019\000\031\000\020\000\030\000\021\000\029\000\022\000\028\000\
\\023\000\027\000\024\000\026\000\025\000\101\000\026\000\101\000\
\\028\000\024\000\034\000\101\000\037\000\101\000\043\000\101\000\
\\044\000\101\000\045\000\101\000\048\000\101\000\000\000\
\\001\000\005\000\069\000\009\000\037\000\014\000\036\000\015\000\035\000\
\\016\000\034\000\017\000\033\000\018\000\032\000\019\000\031\000\
\\020\000\030\000\021\000\029\000\022\000\028\000\023\000\027\000\
\\024\000\026\000\025\000\025\000\028\000\024\000\000\000\
\\001\000\006\000\082\000\009\000\037\000\014\000\036\000\015\000\035\000\
\\016\000\034\000\017\000\033\000\018\000\032\000\019\000\031\000\
\\020\000\030\000\021\000\029\000\022\000\028\000\023\000\027\000\
\\024\000\026\000\025\000\025\000\028\000\024\000\000\000\
\\001\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\026\000\067\000\028\000\024\000\034\000\066\000\000\000\
\\001\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\028\000\024\000\037\000\076\000\048\000\075\000\000\000\
\\001\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\028\000\024\000\043\000\068\000\000\000\
\\001\000\020\000\070\000\000\000\
\\001\000\025\000\038\000\000\000\
\\001\000\027\000\050\000\000\000\
\\001\000\029\000\071\000\000\000\
\\001\000\030\000\049\000\000\000\
\\001\000\034\000\065\000\000\000\
\\001\000\037\000\076\000\048\000\075\000\000\000\
\\001\000\044\000\000\000\000\000\
\\001\000\045\000\083\000\000\000\
\\088\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\028\000\024\000\000\000\
\\089\000\000\000\
\\090\000\027\000\012\000\030\000\011\000\033\000\010\000\038\000\009\000\
\\039\000\008\000\000\000\
\\091\000\027\000\012\000\030\000\011\000\033\000\010\000\038\000\009\000\
\\039\000\008\000\000\000\
\\092\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\028\000\024\000\000\000\
\\093\000\000\000\
\\094\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\028\000\024\000\000\000\
\\095\000\015\000\035\000\016\000\034\000\018\000\032\000\019\000\031\000\
\\028\000\024\000\000\000\
\\102\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\028\000\024\000\000\000\
\\104\000\015\000\035\000\016\000\034\000\018\000\032\000\019\000\031\000\
\\028\000\024\000\000\000\
\\105\000\015\000\035\000\016\000\034\000\018\000\032\000\019\000\031\000\
\\028\000\024\000\000\000\
\\106\000\015\000\035\000\016\000\034\000\028\000\024\000\000\000\
\\107\000\015\000\035\000\016\000\034\000\028\000\024\000\000\000\
\\108\000\009\000\037\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\019\000\031\000\022\000\028\000\023\000\027\000\
\\024\000\026\000\028\000\024\000\000\000\
\\109\000\009\000\037\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\019\000\031\000\022\000\028\000\023\000\027\000\
\\024\000\026\000\028\000\024\000\000\000\
\\110\000\009\000\037\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\019\000\031\000\024\000\026\000\028\000\024\000\000\000\
\\111\000\009\000\037\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\019\000\031\000\024\000\026\000\028\000\024\000\000\000\
\\112\000\009\000\037\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\019\000\031\000\024\000\026\000\028\000\024\000\000\000\
\\113\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\028\000\024\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\026\000\067\000\028\000\024\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\025\000\025\000\028\000\024\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\009\000\037\000\014\000\036\000\015\000\035\000\016\000\034\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\020\000\030\000\
\\021\000\029\000\022\000\028\000\023\000\027\000\024\000\026\000\
\\028\000\024\000\000\000\
\"
val actionRowNumbers =
"\000\000\043\000\026\000\025\000\
\\023\000\015\000\049\000\048\000\
\\001\000\044\000\047\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\018\000\
\\057\000\056\000\016\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\000\000\019\000\011\000\006\000\
\\005\000\004\000\003\000\030\000\
\\029\000\013\000\009\000\014\000\
\\017\000\041\000\040\000\039\000\
\\038\000\037\000\036\000\035\000\
\\034\000\032\000\007\000\031\000\
\\008\000\033\000\024\000\046\000\
\\045\000\001\000\020\000\001\000\
\\001\000\042\000\051\000\050\000\
\\028\000\002\000\052\000\010\000\
\\058\000\022\000\054\000\055\000\
\\001\000\001\000\027\000\012\000\
\\053\000\021\000"
val gotoT =
"\
\\001\000\085\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\004\000\020\000\006\000\001\000\000\000\
\\004\000\021\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\038\000\004\000\003\000\005\000\002\000\006\000\001\000\
\\007\000\037\000\000\000\
\\000\000\
\\000\000\
\\003\000\039\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\040\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\041\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\042\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\043\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\044\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\045\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\046\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\049\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\050\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\051\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\052\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\053\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\054\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\055\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\056\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\057\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\058\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\059\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\060\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\061\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\001\000\062\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\071\000\004\000\003\000\005\000\002\000\006\000\001\000\
\\007\000\070\000\000\000\
\\008\000\072\000\000\000\
\\003\000\075\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\076\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\078\000\004\000\003\000\005\000\002\000\006\000\001\000\
\\009\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\082\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\083\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\008\000\084\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 86
val numrules = 43
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | FALSE of unit ->  (bool) | TRUE of unit ->  (bool)
 | NAME of unit ->  (string) | NAT of unit ->  (int)
 | CondExpr of unit ->  (expr option)
 | MatchExpr of unit ->  ( ( expr option * expr )  list)
 | Comps of unit ->  (expr) | Const of unit ->  (expr)
 | AppExpr of unit ->  (expr) | AtomicExpr of unit ->  (expr)
 | Expr of unit ->  (expr) | Prog of unit ->  (unit)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 43) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "FUN"
  | (T 2) => "FUNREC"
  | (T 3) => "IF"
  | (T 4) => "THEN"
  | (T 5) => "ELSE"
  | (T 6) => "MATCH"
  | (T 7) => "EXCLAMATION"
  | (T 8) => "MINUS"
  | (T 9) => "HEAD"
  | (T 10) => "TAIL"
  | (T 11) => "ISEMPTY"
  | (T 12) => "PRINT"
  | (T 13) => "AND"
  | (T 14) => "OR"
  | (T 15) => "NOT"
  | (T 16) => "PLUS"
  | (T 17) => "TIMES"
  | (T 18) => "DIV"
  | (T 19) => "EQ"
  | (T 20) => "NE"
  | (T 21) => "LT"
  | (T 22) => "LE"
  | (T 23) => "TWOCOLON"
  | (T 24) => "SEMICOLON"
  | (T 25) => "COMMA"
  | (T 26) => "NAT"
  | (T 27) => "LBRACKET"
  | (T 28) => "RBRACKET"
  | (T 29) => "NAME"
  | (T 30) => "LBRACE"
  | (T 31) => "RBRACE"
  | (T 32) => "LPAREN"
  | (T 33) => "RPAREN"
  | (T 34) => "ANON"
  | (T 35) => "ARROW"
  | (T 36) => "END"
  | (T 37) => "TRUE"
  | (T 38) => "FALSE"
  | (T 39) => "NIL"
  | (T 40) => "BOOL"
  | (T 41) => "INT"
  | (T 42) => "WITH"
  | (T 43) => "EOF"
  | (T 44) => "FUNT"
  | (T 45) => "F"
  | (T 46) => "UNDERSCORE"
  | (T 47) => "PIPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41)
 $$ (T 40) $$ (T 39) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 28) $$ (T 27) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (print (exp2string(Expr)^"\n" ))
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.ntVOID Decl1, Decl1left, _)) :: rest671)) => let val  result
 = MlyValue.Prog (fn _ => let val  (Decl as Decl1) = Decl1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (print (decl2string(Decl, Prog)^"\n" ))
end)
 in ( LrTable.NT 0, ( result, Decl1left, Prog1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.AtomicExpr AtomicExpr1, AtomicExpr1left, 
AtomicExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn
 _ => let val  (AtomicExpr as AtomicExpr1) = AtomicExpr1 ()
 in (AtomicExpr)
end)
 in ( LrTable.NT 2, ( result, AtomicExpr1left, AtomicExpr1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, 
AppExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AppExpr as AppExpr1) = AppExpr1 ()
 in (AppExpr)
end)
 in ( LrTable.NT 2, ( result, AppExpr1left, AppExpr1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.Expr Expr3, _, Expr3right)) :: _ :: ( _, ( 
MlyValue.Expr Expr2, _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 val  Expr3 = Expr3 ()
 in (If(Expr1, Expr2, Expr3))
end)
 in ( LrTable.NT 2, ( result, IF1left, Expr3right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: _ :: ( _, ( MlyValue.Expr Expr1, _, _)) :: ( _, ( _, MATCH1left, _
)) :: rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in (Match(Expr, MatchExpr))
end)
 in ( LrTable.NT 2, ( result, MATCH1left, MatchExpr1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
EXCLAMATION1left, _)) :: rest671)) => let val  result = MlyValue.Expr
 (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("!", Expr))
end)
 in ( LrTable.NT 2, ( result, EXCLAMATION1left, Expr1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("-", Expr))
end)
 in ( LrTable.NT 2, ( result, MINUS1left, Expr1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
HEAD1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("hd", Expr))
end)
 in ( LrTable.NT 2, ( result, HEAD1left, Expr1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
TAIL1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("tl", Expr))
end)
 in ( LrTable.NT 2, ( result, TAIL1left, Expr1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
ISEMPTY1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn
 _ => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("ise", Expr))
end)
 in ( LrTable.NT 2, ( result, ISEMPTY1left, Expr1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("print", Expr))
end)
 in ( LrTable.NT 2, ( result, PRINT1left, Expr1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim1("NOT", Expr))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("AND", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("OR", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("AND", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("-", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("*", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("div", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("=", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("!=", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<=", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("::", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2(";", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 26, ( ( _, ( _, _, RBRACKET1right)) :: ( _, ( MlyValue.NAT NAT1,
 _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)
) => let val  result = MlyValue.Expr (fn _ => let val  (Expr as Expr1)
 = Expr1 ()
 val  (NAT as NAT1) = NAT1 ()
 in (Item(NAT, Expr))
end)
 in ( LrTable.NT 2, ( result, Expr1left, RBRACKET1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.Const Const1, Const1left, Const1right)) :: 
rest671)) => let val  result = MlyValue.AtomicExpr (fn _ => let val  (
Const as Const1) = Const1 ()
 in (Const)
end)
 in ( LrTable.NT 3, ( result, Const1left, Const1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.AtomicExpr (fn _ => let val  (
NAME as NAME1) = NAME1 ()
 in (Var(NAME))
end)
 in ( LrTable.NT 3, ( result, NAME1left, NAME1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Expr Expr1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.AtomicExpr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Comps Comps1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.AtomicExpr (fn _ => let val  (Comps as Comps1) = Comps1 ()
 in (List([Comps]))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.NAT NAT1, NAT1left, NAT1right)) :: rest671)
) => let val  result = MlyValue.Const (fn _ => let val  (NAT as NAT1)
 = NAT1 ()
 in (ConI(NAT))
end)
 in ( LrTable.NT 5, ( result, NAT1left, NAT1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.TRUE TRUE1, TRUE1left, TRUE1right)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => let val  (TRUE
 as TRUE1) = TRUE1 ()
 in (ConB(TRUE))
end)
 in ( LrTable.NT 5, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.FALSE FALSE1, FALSE1left, FALSE1right)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => let val  (FALSE
 as FALSE1) = FALSE1 ()
 in (ConB(FALSE))
end)
 in ( LrTable.NT 5, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Comps (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (List([Expr1, Expr2]))
end)
 in ( LrTable.NT 6, ( result, Expr1left, Expr2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.Comps Comps1, _, Comps1right)) :: _ :: ( _,
 ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result
 = MlyValue.Comps (fn _ => let val  (Expr as Expr1) = Expr1 ()
 val  (Comps as Comps1) = Comps1 ()
 in (List([Expr, Comps]))
end)
 in ( LrTable.NT 6, ( result, Expr1left, Comps1right), rest671)
end
|  ( 36, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.MatchExpr (fn _ => ([]))
 in ( LrTable.NT 7, ( result, END1left, END1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: ( _, ( MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( 
MlyValue.CondExpr CondExpr1, _, _)) :: ( _, ( _, PIPE1left, _)) :: 
rest671)) => let val  result = MlyValue.MatchExpr (fn _ => let val  (
CondExpr as CondExpr1) = CondExpr1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in ((CondExpr, Expr)::MatchExpr)
end)
 in ( LrTable.NT 7, ( result, PIPE1left, MatchExpr1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.CondExpr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 in (SOME Expr)
end)
 in ( LrTable.NT 8, ( result, Expr1left, Expr1right), rest671)
end
|  ( 39, ( ( _, ( _, UNDERSCORE1left, UNDERSCORE1right)) :: rest671))
 => let val  result = MlyValue.CondExpr (fn _ => (NONE))
 in ( LrTable.NT 8, ( result, UNDERSCORE1left, UNDERSCORE1right), 
rest671)
end
|  ( 40, ( ( _, ( MlyValue.AtomicExpr AtomicExpr2, _, AtomicExpr2right
)) :: ( _, ( MlyValue.AtomicExpr AtomicExpr1, AtomicExpr1left, _)) :: 
rest671)) => let val  result = MlyValue.AppExpr (fn _ => let val  
AtomicExpr1 = AtomicExpr1 ()
 val  AtomicExpr2 = AtomicExpr2 ()
 in (Call(AtomicExpr1, AtomicExpr2))
end)
 in ( LrTable.NT 4, ( result, AtomicExpr1left, AtomicExpr2right), 
rest671)
end
|  ( 41, ( ( _, ( MlyValue.AtomicExpr AtomicExpr1, _, AtomicExpr1right
)) :: ( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, _)) :: rest671))
 => let val  result = MlyValue.AppExpr (fn _ => let val  (AppExpr as 
AppExpr1) = AppExpr1 ()
 val  (AtomicExpr as AtomicExpr1) = AtomicExpr1 ()
 in (Call(AppExpr, AtomicExpr))
end)
 in ( LrTable.NT 4, ( result, AppExpr1left, AtomicExpr1right), rest671
)
end
|  ( 42, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: _ :: ( _, ( 
MlyValue.NAME NAME1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (NAME as NAME1)
 = NAME1 ()
 val  (Expr as Expr1) = Expr1 ()
 in (Let(NAME, Expr, Expr))
end; ()))
 in ( LrTable.NT 1, ( result, VAR1left, Expr1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNREC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCLAMATION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun HEAD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TAIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ISEMPTY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun TWOCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NAT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.NAT (fn () => i),p1,p2))
fun LBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun ANON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.TRUE (fn () => i),p1,p2))
fun FALSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.FALSE (fn () => i),p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun F (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERSCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
end
end
