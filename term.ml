type exp =
  C0
| C1
| Var of ident
| Hole of ident * bool 
| If0 of exp * exp * exp * tag
| Fold of exp * exp * exp * tag
| Op1 of op1 list * exp * tag
| Op2 of op2 * exp list * tag
| Cst of int64 (* Value *) * exp (* example of realisation *) * tag
and tag = int
and op1 = | Not | Shl1 | Shr1 | Shr4 | Shr16
and op2 = | And | Or | Xor | Plus
and ident = int

let get_exp_id = function
  | C0 -> -1
  | C1 -> -2
  | Var x -> (-x -3)*2+1
  | Hole (x,_) -> (-x-3)*2 
  | If0 (_, _, _, x) -> x
  | Fold (_, _, _, x) -> x
  | Op1 (_, _, x) -> x
  | Op2 (_, _, x) -> x
  | Cst (_, _, x) -> x

module HC = Hashcons.Make(struct
  type t = exp

  let equal x y =
    match x, y with
    | C0, C0 -> true
    | C1, C1 -> true
    | Var id1, Var id2 -> id1 = id2
    | Hole (id1,b1), Hole (id2,b2) -> id1 = id2 && b1 = b2
    | If0 (a1, b1, c1, _), If0 (a2, b2, c2, _) ->
	a1 == a2 && b1 == b2 && c1 == c2
    | Fold (a1, b1, c1, _), Fold (a2, b2, c2, _) ->
	a1 == a2 && b1 == b2 && c1 == c2
    | Op1 (a1, b1, _), Op1 (a2, b2, _) ->
	a1 = a2 && b1 == b2
    | Op2 (a1, b1, _), Op2 (a2, b2, _) ->
	let rec eq a b =
	  match a, b with
	  | [], [] -> true
	  | t1::q1, t2::q2 when t1 == t2 -> eq q1 q2
	  | _, _ -> false
	in
	a1 = a2 && eq b1 b2
    | Cst (a1, _, _), Cst (a2, _, _) ->
	a1 = a2
    | _ -> false

  let hash_op1 op =
    match op with Not -> 19 | Shl1 -> 23 | Shr1 -> 59 | Shr4 -> 61 | Shr16 -> 67
      
  let hash x =
    match x with
    | C0 -> 13
    | C1 -> 17
    | Var x -> x
    | Hole (x,_) -> x+5   
    | If0 (a, b, c, _) ->
	Hashcons.combine_list get_exp_id 31 [a;b;c]
    | Fold (a, b, c, _) ->
	Hashcons.combine_list get_exp_id 37 [a;b;c]
    | Op1 (ops, a, _) ->
	Hashcons.combine
	  101
	  (Hashcons.combine_list
	     hash_op1
	     (get_exp_id a)
	     ops)
    | Op2 (op, l, _) ->
	Hashcons.combine_list
	  get_exp_id
	  (match op with And -> 71 | Or -> 73 | Xor -> 79 | Plus -> 83)
	  l
    | Cst (a, _, _) ->
	Hashcons.combine 91
	  (Int64.to_int (Int64.mul a 103L))

  let tag n = function
    | C0 -> C0
    | C1 -> C1
    | Var x -> Var x
    | Hole (x,y) -> Hole (x,y)
    | If0 (a, b, c, _) -> If0 (a, b, c, n)
    | Fold (a, b, c, _) -> Fold (a, b, c, n)
    | Op1 (a, b, _) -> Op1 (a, b, n)
    | Op2 (a, b, _) -> Op2 (a, b, n)
    | Cst (a, b, _) -> Cst (a, b, n)

end)

module H = Hashtbl.Make(struct
  type t = exp
  let equal = (==)
  let hash = get_exp_id
end)

let size x =
  let rec aux = function
    | C0 | C1 | Var _ | Hole _ -> 1
    | If0 (e,f,g,_) -> 1 + aux e + aux f + aux g
    | Fold (e,f,g, _) -> 2 + aux e + aux f + aux g
    | Op1 (l, e, _) -> List.length l + aux e
    | Op2 (_, l, _) ->
	List.length l - 1 +
	  List.fold_left (fun acc e -> aux e + acc) 0 l
    | Cst (_, _, _) -> 1
  in aux x + 1

let max_hole x =
  let (+) = max in 
  let rec aux = function
    | C0 | C1 | Var _ -> min_int 
    | Hole (n,_) -> n
    | If0 (e,f,g,_) -> aux e + aux f + aux g
    | Fold (e,f,g, _) -> aux e + aux f + aux g
    | Op1 (_, e, _) -> aux e
    | Op2 (_, l, _) -> List.fold_left (fun acc e -> acc + aux e) min_int l
    | Cst (_, _, _) -> min_int
  in max 0 (succ (aux x))

let rec holes =function
  | C0 | C1 | Var _ -> 0
  | Hole (n,_) -> 1
  | If0 (e,f,g,_) -> holes e + holes f + holes g
  | Fold (e,f,g, _) -> holes e + holes f + holes g
  | Op1 (_, e, _) -> holes e
  | Op2 (_, l, _) -> List.fold_left (fun acc e -> acc + holes e) 0 l
  | Cst (_, _, _) -> 0

  
(* There is at most three variables in the terms, hence, we can define them statically *)
module Constants = struct 
  let arg = 0
  let fold_acc = 1
  let fold_arg = 2
end

open Int64
let rnd64 () =
  if Random.bool ()
  then Random.int64 Int64.max_int
  else Int64.neg (Random.int64 Int64.max_int)

module Notations = struct
    
  let mk_arg = HC.hashcons (Var (Constants.arg))
  let mk_facc = HC.hashcons (Var (Constants.fold_acc))
  let mk_farg = HC.hashcons (Var (Constants.fold_arg))
  let c0 = HC.hashcons C0
  let c1 = HC.hashcons C1
  let cst v e =
    match v with
    | 0L -> c0
    | 1L -> c1
    | _ -> HC.hashcons (Cst (v, e, -1))

  let rec if0 c a b =
    match c with
    | C0 | Cst (0L, _, _) -> a
    | C1 | Cst (_, _, _) -> b
    | _ ->
	if a == b then a
	else if c == a then if0 c c0 b
	else if Pervasives.(&&) (c == b) (a == c0) then c
	else
	  HC.hashcons (If0 (c, a, b, -1))

  (* unop *)
  let rec compose_op1 op ops =
    match op, ops with
    | Not, Not::q -> q
    | Shr1, Shr1 :: Shr1 :: Shr1 :: q -> Shr4 ::q
    | Shr4, Shr4 :: Shr4 :: Shr4 :: q -> Shr16 ::q
    | Shr4, Shr1 :: q -> compose_op1 Shr1 (compose_op1 Shr4 q)
    | Shr16, Shr1 :: q -> compose_op1 Shr1 (compose_op1 Shr16 q)
    | Shr16, Shr4 :: q -> compose_op1 Shr4 (compose_op1 Shr16 q)
    | _, _ -> op :: ops

  let rec op1 op x =
    match op,x with
    | _, Op1 (ops,t,_) ->
      let ops = compose_op1 op ops in
      if ops = [] then t else
      HC.hashcons (Op1 (ops, t, -1))
    | Shr1, C0 | Shr1, C1 -> c0
    | Shr4, C0 | Shr4, C1 -> c0
    | Shr16, C0 | Shr16, C1 -> c0
    | Shl1, C0 -> c0
    | _, If0 (a, b, c, _) -> if0 a (op1 op b) (op1 op c)
    | ((Shr1 | Shr4 | Shr16 | Shl1), Op2 ((And|Or|Xor) as op', l, _)) ->
	List.fold_right (fun e acc -> op2 op' (op1 op e) acc) l
	  (unit_of_op2 op')
    | (Not, Op2 ((And|Or) as op', l, _)) ->
	let op' = match op' with And -> Or | Or -> And | _ -> assert false in
	List.fold_right (fun e acc -> op2 op' (op1 op e) acc) l
	  (unit_of_op2 op')
    | _, x -> HC.hashcons (Op1 ([op],x, -1))

  and op2 op x y =
    match x, op with
    | Op2 (op', l, _), _ when op = op' ->
	List.fold_right (fun e acc -> op2 op e acc) l y
    | C0, (Or | Xor | Plus) -> y
    | Op1 ([Not], C0, _), And -> y
    | _ ->
	let rec insert e l =
	  match l with
	  | [] -> [e]
	  | t::q when t == e ->
	      begin match op with
	      | And | Or -> l
	      | Plus -> insert (op1 Shl1 t) q
	      | Xor -> q
	      end
	  | t::q when get_exp_id t < get_exp_id e ->
	      insert t (insert e q)
	  | l -> e::l
	in
	let ly =
	  match y, op with
	  | Op2 (op', l, _), _ when op = op'-> l
	  | C0, (Or | Xor | Plus) -> []
	  | Op1 ([Not], C0, _), And -> []
	  | t, _ -> [t]
	in
	match insert x ly with
	| [] -> unit_of_op2 op
	| [e] -> e
	| l -> HC.hashcons (Op2 (op, l, -1))
  and  unit_of_op2 = function
    | And -> op1 Not c0
    | Or | Plus | Xor -> c0

  let (~~) x = op1 Not x 
  let shl1 x = op1 Shl1 x
  let shr1 x = op1 Shr1 x
  let shr4 x = op1 Shr4 x
  let shr16 x = op1 Shr16 x

  (* binop *)


  let (&&) = op2 And
  let (||) = op2 Or
  let ( ** ) = op2 Xor
  let (++) = op2 Plus

  let fold c a b = HC.hashcons (Fold (c, a, b, -1))

  let hole n b = HC.hashcons (Hole (n,b))
end 

let __op1 ops x =
  match ops with
  | [] -> x
  | _ ->  HC.hashcons (Op1 (ops,x,-1))

let __op2 op l =
  match l with
  | [] -> Notations.unit_of_op2 op
  | [e] -> e
  | _ -> HC.hashcons (Op2 (op,l,-1))

let subst_holes sigma t =
  let rec aux = function
    | C0 | C1 | Var _ as e-> e
    | Hole (n,_) -> (try sigma.(n) with _ -> Printf.printf "sigma : %i hole : %i" (Array.length sigma) n; assert false)
    | If0 (c,f,g,_) -> Notations.if0 (aux c) (aux f) (aux g)
    | Fold (c,f,g, _) -> Notations.fold (aux c) (aux f) (aux g)
    | Op1 (o , e, _) ->
	List.fold_right (fun o acc -> Notations.op1 o acc) o (aux e)
    | Op2 (o , l, _) ->
	List.fold_right (fun e acc -> Notations.op2 o (aux e) acc) l
	  (Notations.unit_of_op2 o)
    | Cst (_, _, _) as e -> e
  in aux t

let renumber_holes t =
  let next = ref 0 in 
  let rec aux = function
  | C0 | C1 | Var _ as e-> e
  | Hole (_,b) -> let n = !next in incr next; Notations.hole n b
  | If0 (c,f,g,_) -> Notations.if0 (aux c) (aux f) (aux g)
  | Fold (c,f,g, _) -> Notations.fold (aux c) (aux f) (aux g)
  | Op1 (o , e, _) ->
      List.fold_right (fun o acc -> Notations.op1 o acc) o (aux e)
  | Op2 (o , l, _) ->
      List.fold_right (fun e acc -> Notations.op2 o (aux e) acc) l
	(Notations.unit_of_op2 o)
  | Cst (_, _, _) as e -> e
  in 
aux t
    
(** Reified representation of operators *)
type op =
| If0o
| Foldo
| Op1o of op1
| Op2o of op2

let op_of_string = function
  | "not" -> Op1o Not
  | "shl1" -> Op1o Shl1
  | "shr1" -> Op1o Shr1
  | "shr4" -> Op1o Shr4
  | "shr16" -> Op1o Shr16
  | "and" -> Op2o And
  | "or" -> Op2o Or
  | "xor" -> Op2o Xor
  | "plus" -> Op2o Plus
  | "if0" -> If0o
  | "fold" -> Foldo
  | "tfold" -> Foldo
  | op -> failwith (Printf.sprintf "Parser.parse_op: unknown operator %S" op)

let string_of_op1 = function
  | Not -> "not"
  | Shl1 -> "shl1"
  | Shr1 -> "shr1"
  | Shr4 -> "shr4"
  | Shr16 -> "shr16"
let string_of_op2 = function
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Plus -> "plus"

let string_of_op = function
  | If0o -> "if0"
  | Foldo -> "fold"
  | Op1o op -> string_of_op1 op
  | Op2o op -> string_of_op2 op
