(* Plc interpreter main file *)

fun run(e : expr) : string =
 let
  val eType = teval e []
  val eVal = eval e []
 in
  val2string eVal ^ " : " ^ type2string eType
 end;