open Term
open Int64

(* the most general evaluation function takes as arguments an
   environment for the bound varibles and a substituion for holes. *)

let eval sigma (env: Vect.t array) t =
  
  

let eval =
  let env = Array.create 3 0L in 
  let rec eval = function
    | C0 -> 0L
    | C1 -> 1L
    | Var id -> env.(id)
    | Hole _ -> assert false 
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
    | Cst (a, _, _) -> a
  in
  fun p x -> env.(Constants.arg) <- x; eval p
;;

let evalv p v = Array.map (eval p) v;;
