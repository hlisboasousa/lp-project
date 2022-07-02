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

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
          ConI i => IntT
        | ConB b => BoolT
        | ESeq x => x
		| Var x => lookup env x
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr, t1) of
                         ("-", IntT) => IntT
                        |("!", BoolT) => BoolT
                        |("ise", ListT l) => BoolT
                        |("hd", ListT l) => t1
                        |("tl", ListT l) => t1
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
		| _   =>  raise UnknownType