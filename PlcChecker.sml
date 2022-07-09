(* PlcChecker *)
exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

(* fun checkRes (e2:expr) (xs : (expr option * expr) list) (env) : bool =
	case xs of
			[] => true
		|	(cond, res)::t =>
			if e2 <> (teval res env) then false
			else checkRes e2 t env *)

fun getElement index [] it = raise ListOutOfRange
	| getElement index (h::t) it =
			if it = index then h
			else if it > index then raise ListOutOfRange
			else getElement index t (it+1)

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
      ConI _ => IntT
    | ConB _ => BoolT
    | ESeq x => x
		| Var x => lookup env x
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr, t1) of
							("-", IntT) => IntT
						|	("!", BoolT) => BoolT
						|	("ise", _) => BoolT
						|	("hd", _) => t1
						|	("tl", _) => t1
						| ("print", _) => ListT []
						| _ => raise UnknownType
				end
		| Prim2(opr, e1, e2) =>
				let
					val t1 = teval e1 env
					val t2 = teval e2 env
				in
					case (opr, t1, t2) of
                      ("&&", BoolT, BoolT) => BoolT
                    | ("*" , IntT, IntT) => IntT
					| ("div" , IntT, IntT) => IntT
					| ("+" , IntT, IntT) => IntT
					| ("-" , IntT, IntT) => IntT
					| ("=" , _, _) => 
                        if t1 = t2 then BoolT
                        else
                            raise NotEqTypes
					| ("!=" , _, _) => 
                        if t1 = t2 then BoolT
                        else
                            raise NotEqTypes
					| ("<" , IntT, IntT) => BoolT
					| ("<=" , IntT, IntT) => BoolT
					| ("::" , _, _) => t2
					| (";" , _ , _)    => t2
					| _   =>  raise UnknownType
				end
		| Let(x, e1, e2) =>
				let
					val t = teval e1 env
					val env' = (x,t)::env
				in
					teval e2 env'
				end
		| If(cond, e1, e2) =>
            let
                val condType = teval cond env
                val e1Type = teval e1 env
                val e2Type = teval e2 env
            in
                if condType = BoolT then
                    if e1Type = e2Type then
                        e1Type
                    else
                        raise DiffBrTypes
                else 
                    raise IfCondNotBool
            end
		| Match(e, []) => raise NoMatchResults
		| Match(e, (SOME e1, e2) :: xs) =>
				let
						val tEval = teval e env
						val tE1val = teval e1 env
						val tE2val = teval e2 env
						val matchE = Match(e, xs)
				in
						if tEval <> tE1val then raise MatchCondTypesDiff
						else tE2val
				end
		| Match(e, (None, e2) :: xs) => teval e2 env
		| List([]) => ListT []
		| List(h::t) =>
				let
						val headType = teval h env
						val tail = List(t)
						val tailType = teval tail env
				in
						ListT ([headType, tailType])
				end
		| Item(i, e) =>
				let
						val eType = teval e env
				in
						case eType of
								ListT(l) => getElement i l 1
							|	_ => raise OpNonList
				end 
		| _   =>  raise UnknownType