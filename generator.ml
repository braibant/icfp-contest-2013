open Term

module OSet = Set.Make(struct type t=op let compare=compare end)

let ops_from_list =
  List.fold_left (fun acc t -> OSet.add t acc) OSet.empty

let all_ops =
  ops_from_list
    [If0o;
     Op1o Not;Op1o Shl1;Op1o Shr1;Op1o Shr4;Op1o Shr16;
     Op2o And;Op2o Or;Op2o Xor;Op2o Plus]

let operators t =
  let rec operators t acc =
    match t with
    | C0 | C1 | Var _ | Hole (_,_) -> acc 
    | If0(a,b,c,_) ->
	operators a (operators b (operators c (OSet.add If0o acc)))
    | Fold(a,b,c,_) ->
	operators a (operators b (operators c (OSet.add Foldo acc)))
    | Op1(ops,a,_) ->
      let l = ops_from_list (List.map (fun x -> Op1o x) ops) in
	operators a (OSet.union l acc)
    | Op2(op,a,b,_) ->
	operators a (operators b (OSet.add (Op2o op) acc))
    | Cst(_, _, _) -> assert false
  in
  operators t OSet.empty

let rec min_free_var = function
  | C0 -> 3
  | C1 -> 3
  | Cst (_, _, _) -> 3
  | Hole (_,b) -> if b then 0 else 1 
  | Var x -> x
  | If0(a,b,c,_) ->
      begin match min_free_var a with
      | 3 ->
	  if Eval.eval a 0L = 0L then min_free_var a
	  else min_free_var b
      | n -> n
      end
  | Fold (a, b, c, _) ->
      if min_free_var a = 3 && min_free_var b = 3 && min_free_var c >= 1 then
	3
      else if min_free_var c = 3 then
	3
      else
	0
  | Op1 (_, a, _) ->
      min_free_var a
  | Op2 (_, a, b, _) ->
      min (min_free_var a) (min_free_var b)

let rec simpl t =
  if min_free_var t = 3 then Notations.cst (Eval.eval t 0L) t else
  match t with
  | If0 (C0, a, b, _) -> simpl a
  | If0 (C1, a, b, _) -> simpl b
  | If0 (Cst (v, _, _), a, b, _) -> if v = 0L then simpl a else simpl b
  | If0 (_, a, b, _) when a == b -> simpl a
  (* | Op1 (Not, Op1 (Not, a, _), _) -> simpl a *)
  (* | Op1 (Shr1, Op1 (Shr1, Op1 (Shr1, Op1 (Shr1, t, _), _), _), _) -> *)
  (*     simpl (Notations.shr4 t) *)
  (* | Op1 (Shr4, Op1 (Shr4, Op1 (Shr4, Op1 (Shr4, t, _), _), _), _) -> *)
  (*     simpl (Notations.shr16 t) *)
  | Op2 (And, C0, t, _) | Op2 (And, t, C0, _)
  | Op2 (And, Cst (0L, _, _), t, _) | Op2 (And, t, Cst (0L, _, _), _) ->
      Notations.(cst 0L c0)
  | Op2 (And, a, b, _) when a == b -> simpl a
  | Op2 (And, (Op2 (And, a, b, _) as r), c, _) when b == c -> simpl r
  | Op2 (Or, C0, t, _) | Op2 (Or, t, C0, _)
  | Op2 (Or, Cst (0L, _, _), t, _) | Op2 (Or, t, Cst (0L, _, _), _) ->
      simpl t
  | Op2 (Or, a, b, _) when a == b -> simpl a
  | Op2 (Or, (Op2 (Or, a, b, _) as r), c, _) when b == c -> simpl r
  | Op2 (Xor, C0, t, _) | Op2 (Xor, t, C0, _)
  | Op2 (Xor, Cst (0L, _, _), t, _) | Op2 (Xor, t, Cst (0L, _, _), _) ->
      simpl t
  | Op2 (Xor, a, b, _) when a == b -> Notations.(cst 0L c0)
  | Op2 (Xor, Op2 (Xor, a, b, _), c, _) when b == c -> simpl a
  | Op2 (Plus, C0, t, _) | Op2 (Plus, t, C0, _)
  | Op2 (Plus, Cst (0L, _, _), t, _) | Op2 (Plus, t, Cst (0L, _, _), _) ->
      simpl t
  | Op2 (Plus, a, b, _) when a == b -> simpl (Notations.shl1 a)
  | Op2 (Plus, Op2 (Plus, a, b, _), c, _) when b == c ->
      simpl Notations.(a ++ shl1 b)
  | t -> t



type fold_state =
  | Required
  | Inside
  | Forbidden

let generate, generate_tfold, generate_novar,generate_context =
  let generate force_fold size exact ops atoms =
    let has_fold = OSet.mem Foldo ops in
    let ops = OSet.remove Foldo ops in
    let memo = Array.make_matrix size 3 None in
    let () =
      memo.(0).(0) <- Some [];
      memo.(0).(1) <- Some (Notations.(mk_facc::mk_farg::atoms));
      memo.(0).(2) <- Some atoms
    in
    let rec aux size fold_state =
      let fold_state_int =
	match fold_state with Required -> 0 | Inside -> 1 | Forbidden -> 2
      in
      if memo.(size-1).(fold_state_int) = None then
	begin
	  assert (size > 1);
	  let res =
	    let acc =
	      if fold_state = Required then
		begin
		  let acc = ref [] in
		  for i = 1 to size-4 do
		    for j = 1 to size-3-i do
		      let gen1 = aux i Forbidden in
		      let gen2 = aux j Forbidden in
		      let gen3 = aux (size-2-i-j) Inside in
		      acc:=
			List.fold_left (fun acc x ->
			  List.fold_left (fun acc y ->
			    List.fold_left (fun acc z -> Notations.fold x y z::acc) acc gen3)
			    acc gen2)
			  !acc gen1
		    done;
		  done;
		  !acc
		end
	      else []
	    in

	    OSet.fold (fun op acc ->
	      match op with
	      | Op1o op ->
		  List.fold_left
		    (fun acc x -> Notations.op1 op x::acc)
		    acc
		    (aux (size-1) fold_state)
	      | Op2o op ->
		  if fold_state = Required then
		    begin
		      let acc = ref acc in
		      for i = 5 to size-2 do
			let genl = aux i Required in
			let genr = aux (size-1-i) Forbidden in
			acc:=
			  List.fold_left (fun acc x ->
			    List.fold_left (fun acc y -> Notations.op2  op x y::acc)
			      acc genr)
			    !acc genl
		      done;
		      !acc
		    end
		  else
		    begin
		      let acc = ref acc in
		      for i = 1 to (size-1)/2 do
			let genl = aux i fold_state in
			let genr = aux (size-1-i) fold_state in
			acc:=
			  List.fold_left (fun acc x ->
			    List.fold_left (fun acc y ->
			      if i = size-1-i && get_exp_id y < get_exp_id x then acc
			      else Notations.op2 op x y::acc)
			      acc genr)
			    !acc genl
		      done;
		      !acc
		    end
	      | If0o ->
		  if fold_state = Required then
		    begin
		      let acc = ref acc in
		      for i = 5 to size-3 do
			for j = 1 to size-2-i do
			  let gen1 = aux i Required in
			  let gen2 = aux j Forbidden in
			  let gen3 = aux (size-1-i-j) Forbidden in
			  acc:=
			    List.fold_left (fun acc x ->
			      List.fold_left (fun acc y ->
				List.fold_left (fun acc z -> Notations.if0 x y z::acc) acc gen3)
				acc gen2)
			      !acc gen1
			done;
		      done;
		      for i = 1 to size-7 do
			for j = 5 to size-2-i do
			  let gen1 = aux i Forbidden in
			  let gen2 = aux j Required in
			  let gen3 = aux (size-1-i-j) Forbidden in
			  acc:=
			    List.fold_left (fun acc x ->
			      List.fold_left (fun acc y ->
				List.fold_left (fun acc z -> Notations.if0 x y z::acc) acc gen3)
				acc gen2)
			      !acc gen1
			done;
		      done;
		      for i = 1 to size-7 do
			for j = 1 to size-6-i do
			  let gen1 = aux i Forbidden in
			  let gen2 = aux j Forbidden in
			  let gen3 = aux (size-1-i-j) Required in
			  acc:=
			    List.fold_left (fun acc x ->
			      List.fold_left (fun acc y ->
				List.fold_left (fun acc z -> Notations.if0 x y z::acc) acc gen3)
				acc gen2)
			      !acc gen1
			done;
		      done;
		      !acc
		    end
		  else
		    let acc = ref acc in
		    for i = 1 to size-3 do
		      for j = 1 to size-2-i do
			let gen1 = aux i fold_state in
			let gen2 = aux j fold_state in
			let gen3 = aux (size-1-i-j) fold_state in
			acc:=
			  List.fold_left (fun acc x ->
			    List.fold_left (fun acc y ->
			      List.fold_left (fun acc z -> Notations.if0 x y z::acc) acc gen3)
			      acc gen2)
			    !acc gen1
		      done;
		    done;
		    !acc
	      | Foldo -> assert false)
	      ops acc
	  in
	  let htbl = Term.H.create 13 in
	  List.iter (fun x -> Term.H.replace htbl (simpl x) ()) res;
	  memo.(size-1).(fold_state_int) <-
	    Some
	      (Term.H.fold (fun e _ acc -> e::acc) htbl [])
	end;
      match memo.(size-1).(fold_state_int) with
      | Some x -> x
      | None -> assert false
    in
    let rec aux2 size fold_state =
      if exact then aux size fold_state
      else
	match size with
	| 0 -> []
	| _ -> List.rev_append (aux size fold_state) (aux2 (size-1) fold_state)
    in
    if force_fold then
      aux2 size (if has_fold then Required else Forbidden)
    else
      let lst = aux2 size Forbidden in
      if has_fold then List.rev_append (aux size Required) lst else lst
  in
  (fun ?(force_fold=true) size ?(exact=true) ops ->
    generate force_fold (size-1) exact ops Notations.([c0;c1;mk_arg])
  ),
  (fun size ?(exact=true) ops ->
    let ops = OSet.remove Foldo ops in
    let size = size-5 in
    let lst =
      generate false size exact ops Notations.([c0;c1;mk_arg;mk_facc;mk_farg])
    in
    List.rev_map (fun t -> Notations.(fold mk_arg c0 t)) lst
  ),
  (fun ?(force_fold=true) size ?(exact=true) ops ->
    generate force_fold (size-1) exact ops Notations.([c0;c1])),
  (fun size ops holes -> 
    generate false size false ops (Notations.([c0;c1;mk_arg]@holes))
  )

let generate_constants ?(force_fold=true) size ?(exact=true) ops =
  List.rev_map (fun t -> Eval.eval t 0L) (generate_novar ~force_fold ~exact size ops)

let generate_constants_witness size ops =
  List.rev_map (fun t -> t, Eval.eval t 0L) (generate_novar ~force_fold:false 
					       ~exact:false size ops)
