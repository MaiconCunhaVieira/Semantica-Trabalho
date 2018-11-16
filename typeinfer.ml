#use "sintaxe.ml"
#use "resultados.ml"

type tipo = TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo | TyId of string | TyPair of tipo * tipo

let rec get_constraints (envmnt : env) (e : expr) = (
	match(e) with
		  Ncte(n) -> (TyInt, [])
		| Bcte(b) -> (TyBool, [])
		| Binop(op, e1, e2) -> match(op) with (
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
)