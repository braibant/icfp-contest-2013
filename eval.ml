open Term
open Int64

(* the most general evaluation function takes as arguments an
   environment for the bound varibles and a substituion for holes. *)
let map2 f a b = 
  Array.mapi (fun i a -> f a b.(i)) a
      
(* evalv + holes + open terms (for folds) *)
let  ho_evalv sigma args env =  
  let size = Array.length args in 
  let ffl = Vect.mk_size size 0xFFL in 
  let zero =  Vect.mk_size size 0L  in 
  let one = Vect.mk_size size 1L  in 
  let rec aux = 
    function
    | C0 -> zero
    | C1 -> one 
    | Cst (i,_,_) -> Vect.mk_size size i
    | Var id -> env.(id)
    | Op1 ([], _,_) -> assert false
    | Op1 (op::q,e,_) -> let e = aux (Term.__op1 q e) in 
		    begin match op with 
		    | Not -> Array.map lognot e
		    | Shl1 -> Array.map (fun e -> shift_left e 1) e
		    | Shr1 -> Array.map (fun e -> shift_right_logical e 1) e
		    | Shr4 -> Array.map (fun e -> shift_right_logical e 4) e
		    | Shr16 -> Array.map (fun e ->shift_right_logical e 16) e
		    end
    | Op2 (_, [], _) -> assert false
    | Op2 (op,e::q,_) -> let e = aux  e in
		      let f = aux  (Term.__op2 op q) in 
		      begin match op with
		      | Term.And -> map2 logand e f
		      | Term.Or -> map2 logor e f
		      | Term.Xor -> map2 logxor e f
		      | Term.Plus -> map2 add e f
		      end 
    | If0 (a,b,c,_) -> 
      let a = aux a in 
      let b = aux b in 
      let c = aux c in 
      Array.init (Array.length a) (fun i ->
	if a.(i) = 0L then b.(i) else c.(i)
      ) 
    | Hole (n,false) -> 
      (try sigma.(n) with _ -> failwith (Printf.sprintf "sigma %i %i" (Array.length sigma) n))
    | Hole (n,_) -> assert false 
    | Fold (e0,e1,e2,_) -> 
      let e0 = ref (aux e0) in 
      let acc = ref (aux e1) in
      for i = 0 to 7 do
	(* set the arguments for the fold *)
	let byte = map2 logand !e0 ffl in 
	env.(Constants.fold_arg) <- byte;
	env.(Constants.fold_acc) <- !acc;
      (* compute the new value of acc *)
	acc := aux e2;
      (* shift e0 *)
	e0 := Array.map (fun e -> shift_right_logical e 8) !e0;
      done;
      !acc
  in aux 

(* evalv + holes *)
let h_evalv p sigma args =
  let env = Array.create 3 (Vect.mk_size (Array.length args) 0L) in 
  env.(0) <- args;
  ho_evalv sigma args env p

let evalv p args =
  h_evalv p [||] args

    

let eval =
  let env = Array.create 3 0L in 
  let rec eval = function
    | C0 -> 0L
    | C1 -> 1L
    | Var id -> env.(id)
    | Hole _ -> 0L
    | If0 (e1,e2,e3,_) -> if eval  e1 = 0L then eval  e2 else eval  e3
    | Op1 ([], _,_) -> assert false
    | Op1 (op::ops,e,_) -> let e = eval (Term.__op1 ops e) in 
		      begin match op with 
		      | Not -> lognot e
		      | Shl1 -> shift_left e 1
		      | Shr1 -> shift_right_logical e 1
		      | Shr4 -> shift_right_logical e 4
		      | Shr16 -> shift_right_logical e 16
		      end
    | Op2 (_, [], _) -> assert false
    | Op2 (op,e::q,_) -> let e = eval  e in
		      let f = eval  (Term.__op2 op q) in 
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
	e0 := shift_right_logical !e0 8;
      done;
      !acc
    | Cst (a, _, _) -> a
  in
  fun p x -> env.(Constants.arg) <- x; eval p
;;

(* let evalv p v = Array.map (eval p) v;; *)
