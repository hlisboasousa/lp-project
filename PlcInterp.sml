(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc
			 
fun eval (e:expr) (env:plcVal env) : plcVal =
	case e of
		  ConI i => IntV i
		| ConB b => BoolV b
		| ESeq _ =>	SeqV []
		| Var x => lookup env x
		| Let(x, e1, e2) =>
				let
					val v = eval e1 env
					val env2 = (x,v) :: env
				in
					eval e2 env2
				end
		| Prim1(opr, e1) =>
				let
					val v1 = eval e1 env
				in
					case (opr, v1) of
						  ("-", IntV i) => IntV (~i)
						| ("!", BoolV b) => BoolV (not b)
						| ("ise", SeqV []) => BoolV true 
						| ("ise", SeqV _) => BoolV false 
						| ("hd", SeqV []) => raise HDEmptySeq
						| ("hd", SeqV l) => hd l
						| ("tl", SeqV []) => raise TLEmptySeq
						| ("tl", SeqV l) => SeqV (tl l)
						| ("print", _) =>
										let
											val s = val2string v1
										in
											print(s^"\n"); ListV []
										end
						| _   => raise Impossible
						end
		| Prim2(opr, e1, e2) =>
				let
					val v1 = eval e1 env
					val v2 = eval e2 env
				in
					case (opr, v1, v2) of
						  ("&&" , BoolV b1, BoolV b2) => BoolV (b1 andalso b2)
						| ("*" , IntV i1, IntV i2) => IntV (i1 * i2)
						| ("div" , IntV i1, IntV i2) => IntV (i1 div i2)
						| ("+" , IntV i1, IntV i2) => IntV (i1 + i2)
						| ("-" , IntV i1, IntV i2) => IntV (i1 - i2)
						| ("=" , _, _) => 
							if v1 = v2 then BoolV true
							else BoolV false
						| ("!=" , _, _) => 
							if v1 <> v2 then BoolV true
							else BoolV false
						| ("<" , IntV i1, IntV i2) => 
							if i1 < i2 then BoolV true
							else BoolV false
						| ("<=" , IntV i1, IntV i2) => 
							if i1 <= i2 then BoolV true
							else BoolV false
						| ("::" , _ , _) => 
								let
										val rec buildList = fn 	(Prim2(expr, h, t)) => (eval h env)::buildList(t)
																					|	(h) => [eval h env]
								in
									SeqV(v1::buildList(e2))
								end
						| (";" , _ , _) => v2
						| _ => raise Impossible
						end
		| If (cond, e1, e2) =>
			let
				val condVal = eval cond env
				val e1Val = eval e1 env
				val e2Val = eval e2 env
			in
				if condVal = BoolV true then e1Val
				else e2Val
			end
		| Match(e, []) => raise ValueNotFoundInMatch
		| Match(e, (SOME e1, e2) :: xs) =>
				let
						val eVal = eval e env
						val e1Val = eval e1 env
						val matchE = Match(e,xs)
				in
						if eVal = e1Val then eval e2 env
						else eval matchE env
				end
		| Match(e, (None, e2) :: xs) => eval e2 env
		| Call(e1, e2) =>
				let
						val evalE1 = eval e1 env
						val rec insertVar = fn 	(List((h::[]))) => [eval h env]
																	| (List(h::t)) => (eval h env)::insertVar(List t)
																	| (exp) => [eval exp env]
				in
					case evalE1 of
							Clos(funName, argName, bodyExpr, funEnv) =>
									let
											val evalE2 = eval e2 (("$list", ListV (insertVar(e2)))::env)
											val env' = (argName, evalE2)::(funName, evalE1)::funEnv
									in
											eval bodyExpr env'
									end
							| _ => raise NotAFunc
				end
		| List(l) =>
				let
						val rec buildList = fn	([]) => []	
																	|	(h::[]) => [eval h env]
																	|	(h::t) => (eval h env)::buildList(t)
				in
						ListV(buildList(l))
				end
		| Item(i, e) => 
				let
						val eVal = eval e env
				in
						case eVal of
									ListV l => getElement i l 1
								|	_ => raise OpNonList
				end
		| Anon(_, varName, expr) => Clos("", varName, expr, env)
		| Letrec(funName, _, argName, _, bodyExpr, callExpr) =>
					let
							val env' = (funName, Clos(funName, argName, bodyExpr, env))::env
					in
							eval callExpr env'
					end