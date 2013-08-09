type exp =
  C0 
| C1 
| Var of ident 
| If0 of exp * exp * exp 
| Fold of exp * exp * exp 
| Op1 of op1 * exp 
| Op2 of op2 * exp * exp
and op1 = | Not | Shl1 | Shr1 | Shr4 | Shr16
and op2 = | And | Or | Xor | Plus
and ident = int

let rec size = function
  | C0 | C1 | Var _ -> 1
  | If0 (e,f,g) -> 1 + size e + size f + size g
  | Fold (e,f,g) -> 2 + size e + size f + size g
  | Op1 (_, e) -> 1 + size e 
  | Op2 (_, e, f) -> 1 + size e + size f 

(* There is at most three variables in the terms, hence, we can define them statically *)
module Constants = struct 
  let arg = 0
  let fold_acc = 1
  let fold_arg = 2
end

open Int64

let eval =
  let env = Array.create 3 0L in 
  let rec eval = function
    | C0 -> 0L  
    | C1 -> 1L
    | Var id -> env.(id)
    | If0 (e1,e2,e3) -> if eval  e1 = 0L then eval  e2 else eval  e3
    | Op1 (op,e) -> let e = eval  e in 
		    begin match op with 
		    | Not -> lognot e
		    | Shl1 -> shift_left e 1
		    | Shr1 -> shift_right_logical e 1
		    | Shr4 -> shift_right_logical e 4
		    | Shr16 -> shift_right_logical e 16
		    end
    | Op2 (op,e,f) -> let e = eval  e in
		      let f = eval  f in 
		      begin match op with
		      | And -> logand e f
		      | Or -> logor e f
		      | Xor -> logxor e f
		      | Plus -> add e f
		      end
    | Fold (e0,e1,e2) -> 
      let e0 = ref (eval  e0) in 
      let acc =  ref (eval e1) in
      for i = 0 to 7 do
      (* set the arguments for the fold *)
	let byte = logand !e0 0xFFL in 
	env.(Constants.fold_arg) <- byte;
	env.(Constants.fold_acc) <- !acc;
      (* compute the new value of acc *)
	acc := eval e2;
      (* shift e0 *)
	e0 := shift_right_logical !e0 2;
      done;
      !acc
  in
  fun p x -> env.(Constants.arg) <- x; eval p
;;

module Notations = struct
    
  let mk_arg = Var (Constants.arg)
  let mk_facc = Var (Constants.fold_acc)
  let mk_farg = Var (Constants.fold_arg) 

  (* binop *)
  let (&&) x y = Op2 (And, x, y)
  let (||) x y = Op2 (Or, x, y)
  let ( ** ) x y = Op2 (Xor, x, y)
  let (++) x y = Op2 (Plus, x, y)
    
  (* unop *)
  let (~~)  x = Op1 (Not, x)
  let shl1 x = Op1 (Shl1, x)
  let shr1 x = Op1 (Shr1, x)
  let shr4 x = Op1 (Shr4, x)
  let shr16 x = Op1 (Shr16, x)

end 

let p = let open Notations in Fold (mk_arg, C0, mk_farg || mk_facc)
(* let _ = Printf.printf "%Ld\n" (eval p (0x1122334455667788L)) *)
