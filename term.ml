type exp =
  C0 
| C1 
| Var of ident 
| If0 of exp * exp * exp 
| Fold of exp * exp * (ident * ident * exp) 
| Op1 of op1 * exp 
| Op2 of op2 * exp * exp
and op1 = | Not | Shl1 | Shr1 | Shr4 | Shr16
and op2 = | And | Or | Xor | Plus
and ident = string
  
(* open Int64 *)

(* let eval = function *)
(*   | C0 -> 0L   *)
(*   | C1 -> 1L *)
(*   | Var id -> ;; *)

(*   | Var of ident  *)
(* | If0 of exp * exp * exp  *)
(* | Fold of exp * exp * (ident * ident * exp)  *)
(* | Op1 of op1 * exp  *)
(* | Op2 of op2 * exp * exp *)

	     
