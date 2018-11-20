#use "sintaxe.ml"
#use "resultados.ml"

exception TypeNotFound of string

type tipo = TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo | TyId of string | TyPair of tipo * tipo

let rec get_constraints (envmnt : env) (e : expr) = (
	match(e) with
		  Ncte(n) -> (TyInt, [])
		| Bcte(b) -> (TyBool, [])
		| Binop(op, e1, e2) -> ( match(op) with
			  Sum -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt), (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Sub -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt), (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Mult -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt), (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Div -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt), (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
		)
		| Unop(op, e1) -> ( match(op) with
			  Not -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let newConstraint = [(typeE1, TyBool)] in
							(TyBool, List.concat[newConstraint; constraintE1])
		)
		| Pair(e1, e2) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
							let (typeE2, constraintE2) = get_constraints envmnt e2 in
								(TyPair(typeE1, typeE2), List.concat[constraintE1, constraintE2])
		| If(e1, e2, e3) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
								let (typeE2, constraintE2) = get_constraints envmnt e2 in
									let (typeE3, constraintE3) = get_constraints envmnt e3 in
										let newConstraint = [(typeE1, TyBool), (typeE2, typeE3)] in
											(typeE2, List.concat[newConstraint; constraintE1; constraintE2; constraintE3])
		| Try(e1, e2) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
							let (typeE2, constraintE2) = get_constraints envmnt e2 in
								let newConstraint = [(typeE1, typeE2)] in
									(typeE1, List.concat[newConstraint; constraintE1; constraintE2])
)