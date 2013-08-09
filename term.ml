type exp =
  C0
| C1
| Var of ident
| If0 of exp * exp * exp * tag
| Fold of exp * exp * exp * tag
| Op1 of op1 * exp * tag
| Op2 of op2 * exp * exp * tag
and tag = int
and op1 = | Not | Shl1 | Shr1 | Shr4 | Shr16
and op2 = | And | Or | Xor | Plus
and ident = int

let get_exp_id = function
  | C0 -> -1
  | C1 -> -2
  | Var x -> -x-3
  | If0 (_, _, _, x) -> x
  | Fold (_, _, _, x) -> x
  | Op1 (_, _, x) -> x
  | Op2 (_, _, _, x) -> x

module HC = Hashcons.Make(struct
  type t = exp

  let equal x y =
    match x, y with
    | C0, C0 -> true
    | C1, C1 -> true
    | Var id1, Var id2 -> id1 = id2
    | If0 (a1, b1, c1, _), If0 (a2, b2, c2, _) ->
	a1 == a2 && b1 == b2 && c1 == c2
    | Fold (a1, b1, c1, _), Fold (a2, b2, c2, _) ->
	a1 == a2 && b1 == b2 && c1 == c2
    | Op1 (a1, b1, _), Op1 (a2, b2, _) ->
	a1 = a2 && b1 == b2
    | Op2 (a1, b1, c1, _), Op2 (a2, b2, c2, _) ->
	a1 = a2 && b1 == b2 && c1 == c2
    | _ -> false

  let hash x =
    match x with
    | C0 -> 3
    | C1 -> 5
    | Var x -> x+13
    | If0 (a, b, c, _) ->
	Hashcons.combine_list get_exp_id 31 [a;b;c]
    | Fold (a, b, c, _) ->
	Hashcons.combine_list get_exp_id 37 [a;b;c]
    | Op1 (op, a, _) ->
	Hashcons.combine
	  (match op with Not -> 19 | Shl1 -> 23 | Shr1 -> 59 | Shr4 -> 61 | Shr16 -> 67)
	  (get_exp_id a)
    | Op2 (op, a, b, _) ->
	Hashcons.combine2
	  (match op with And -> 71 | Or -> 73 | Xor -> 79 | Plus -> 83)
	  (get_exp_id a) (get_exp_id b)

  let tag n = function
    | C0 -> C0
    | C1 -> C1
    | Var x -> Var x
    | If0 (a, b, c, _) -> If0 (a, b, c, n)
    | Fold (a, b, c, _) -> Fold (a, b, c, n)
    | Op1 (a, b, _) -> Op1 (a, b, n)
    | Op2 (a, b, c, _) -> Op2 (a, b, c, n)

end)



let rec size = function
  | C0 | C1 | Var _ -> 1
  | If0 (e,f,g,_) -> 1 + size e + size f + size g
  | Fold (e,f,g, _) -> 2 + size e + size f + size g
  | Op1 (_, e, _) -> 1 + size e
  | Op2 (_, e, f, _) -> 1 + size e + size f

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
    | If0 (e1,e2,e3,_) -> if eval  e1 = 0L then eval  e2 else eval  e3
    | Op1 (op,e,_) -> let e = eval  e in 
      begin match op with 
      | Not -> lognot e
      | Shl1 -> shift_left e 1
      | Shr1 -> shift_right_logical e 1
      | Shr4 -> shift_right_logical e 4
      | Shr16 -> shift_right_logical e 16
      end
    | Op2 (op,e,f,_) -> let e = eval  e in
      let f = eval  f in 
      begin match op with
      | And -> logand e f
      | Or -> logor e f
      | Xor -> logxor e f
      | Plus -> add e f
      end
    | Fold (e0,e1,e2,_) -> 
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
    
  let mk_arg = HC.hashcons (Var (Constants.arg))
  let mk_facc = HC.hashcons (Var (Constants.fold_acc))
  let mk_farg = HC.hashcons (Var (Constants.fold_arg))
  let c0 = HC.hashcons C0
  let c1 = HC.hashcons C0

  (* binop *)
  let (&&) x y = HC.hashcons (Op2 (And, x, y, -1))
  let (||) x y = HC.hashcons (Op2 (Or, x, y, -1))
  let ( ** ) x y = HC.hashcons (Op2 (Xor, x, y, -1))
  let (++) x y = HC.hashcons (Op2 (Plus, x, y, -1))
  let op2 op x y = HC.hashcons (Op2 (op, x, y, -1))
    
  (* unop *)
  let (~~) x = HC.hashcons (Op1 (Not, x, -1))
  let shl1 x = HC.hashcons (Op1 (Shl1, x, -1))
  let shr1 x = HC.hashcons (Op1 (Shr1, x, -1))
  let shr4 x = HC.hashcons (Op1 (Shr4, x, -1))
  let shr16 x = HC.hashcons (Op1 (Shr16, x, -1))
  let op1 op x = HC.hashcons (Op1 (op, x, -1))

  let if0 c a b = HC.hashcons (If0 (c, a, b, -1))
  let fold c a b = HC.hashcons (Fold (c, a, b, -1))
end 

let p = let open Notations in fold mk_arg c0 (mk_farg || mk_facc)
(* let _ = Printf.printf "%Ld\n" (eval p (0x1122334455667788L)) *)
