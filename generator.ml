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
    | Op2(op,l,_) ->
	List.fold_left (fun acc e -> operators e acc) (OSet.add (Op2o op) acc) l
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
  | Op2 (_, l, _) ->
      List.fold_left (fun acc e -> min acc (min_free_var e)) 3 l

let rec simpl t =
  if min_free_var t = 3 then Notations.cst (Eval.eval t 0L) t
  else t

type fold_state =
  | Required
  | Inside
  | Forbidden

let generate, generate_tfold, generate_novar,generate_context =
  let generate ~in_context force_fold size exact ops atoms =
    let has_fold = OSet.mem Foldo ops in
    let ops = OSet.remove Foldo ops in
    let memo = Array.make_matrix size 3 None in
    let () =
      memo.(0).(0) <- Some [| |];
      memo.(0).(1) <- Some (Array.of_list (Notations.(mk_facc::mk_farg::atoms)));
      memo.(0).(2) <- Some (Array.of_list atoms)
    in
    let rec aux size fold_state =
      let fold_state_int =
	match fold_state with Required -> 0 | Inside -> 1 | Forbidden -> 2
      in
      if memo.(size-1).(fold_state_int) = None then
	begin
	  assert (size > 1);
	  let htbl = Term.H.create 100_000 in
	  let add x = Term.H.replace htbl (simpl x) () in

	  if fold_state = Required then
	    for i = 1 to size-4 do
	      for j = 1 to size-3-i do
		let gen1 = aux i Forbidden in
		let gen2 = aux j Forbidden in
		let gen3 = aux (size-2-i-j) Inside in
		Array.iter (fun x ->
		  Array.iter (fun y ->
		    Array.iter (fun z -> add (Notations.fold x y z)) gen3)
		    gen2)
		  gen1
	      done;
	    done;

	  OSet.iter (fun op ->
	    match op with
	    | Op1o op ->
		Array.iter
		  (fun x -> add (Notations.op1 op x))
		  (aux (size-1) fold_state)
	    | Op2o op ->
		if fold_state = Required then
		  for i = 5 to size-2 do
		    let genl = aux i Required in
		    let genr = aux (size-1-i) Forbidden in
		    Array.iter (fun x ->
		      Array.iter (fun y -> add (Notations.op2  op x y))
			genr)
		      genl
		  done
		else
		  for i = 1 to (size-1)/2 do
		    let genl = aux i fold_state in
		    let genr = aux (size-1-i) fold_state in
		    Array.iter (fun x ->
		      Array.iter (fun y ->
			if i < size-1-i || get_exp_id x <= get_exp_id y then
			  add (Notations.op2 op x y))
			genr)
		      genl
		  done
	    | If0o ->
		if fold_state = Required then
		  begin
		    for i = 5 to size-3 do
		      for j = 1 to size-2-i do
			let gen1 = aux i Required in
			let gen2 = aux j Forbidden in
			let gen3 = aux (size-1-i-j) Forbidden in
			Array.iter (fun x ->
			  Array.iter (fun y ->
			    Array.iter (fun z -> add (Notations.if0 x y z)) gen3)
			    gen2)
			  gen1
		      done;
		    done;
		    for i = 1 to size-7 do
		      for j = 5 to size-2-i do
			let gen1 = aux i Forbidden in
			let gen2 = aux j Required in
			let gen3 = aux (size-1-i-j) Forbidden in
			Array.iter (fun x ->
			  Array.iter (fun y ->
			    Array.iter (fun z -> add (Notations.if0 x y z)) gen3)
			    gen2)
			  gen1
		      done;
		    done;
		    for i = 1 to size-7 do
		      for j = 1 to size-6-i do
			let gen1 = aux i Forbidden in
			let gen2 = aux j Forbidden in
			let gen3 = aux (size-1-i-j) Required in
			Array.iter (fun x ->
			  Array.iter (fun y ->
			    Array.iter (fun z -> add (Notations.if0 x y z)) gen3)
			    gen2)
			  gen1
		      done;
		    done
		  end
		else
		  for i = 1 to size-3 do
		    for j = 1 to size-2-i do
		      let gen1 = aux i fold_state in
		      let gen2 = aux j fold_state in
		      let gen3 = aux (size-1-i-j) fold_state in
		      Array.iter (fun x ->
			Array.iter (fun y ->
			  Array.iter (fun z -> add (Notations.if0 x y z)) gen3)
			  gen2)
			gen1
		    done;
		  done
	    | Foldo -> assert false)
	    ops;
	  let res = Array.make (Term.H.length htbl) Notations.mk_arg in
	  ignore (Term.H.fold (fun e _ acc -> res.(acc) <- e; acc+1) htbl 0);
          let res_len = Array.length res in
          let res =
            if res_len > 100_000 || in_context (* unplug quotient here! *)
            then res
            else begin
              let quotient =
                Array.of_list (Quotient.quotient (Array.to_list res))
              in
              Printf.printf "Quotienting level %d helped memo from %d to %d\n%!"
                size res_len (Array.length quotient);
              quotient
            end
          in
	  memo.(size-1).(fold_state_int) <- Some res
	end;
      match memo.(size-1).(fold_state_int) with
      | Some x -> x
      | None -> assert false
    in
    if exact && (not has_fold || force_fold) then
      aux size (if has_fold then Required else Forbidden)
    else
      begin
	let htbl = Term.H.create 100_000 in
	for i = if exact then size else 1 to size do
	  Array.iter (fun e -> Term.H.replace htbl e ())
	    (aux i (if has_fold then Required else Forbidden));
	  if has_fold && not force_fold then
	    Array.iter (fun e -> Term.H.replace htbl e ()) (aux i Forbidden)
	done;
	let res = Array.make (Term.H.length htbl) Notations.mk_arg in
	ignore (Term.H.fold (fun e _ acc -> res.(acc) <- e; acc+1) htbl 0);
	res
      end
  in
  (fun ?(force_fold=true) size ?(exact=true) ops ->
    generate ~in_context:false force_fold (size-1) exact ops Notations.([c0;c1;mk_arg])
  ),
  (fun size ?(exact=true) ops ->
    let ops = OSet.remove Foldo ops in
    let size = size-5 in
    let res =
      generate ~in_context:false false size exact ops Notations.([c0;c1;mk_arg;mk_facc;mk_farg])
    in
    for i = 0 to Array.length res - 1 do
      res.(i) <- Notations.(fold mk_arg c0 res.(i))
    done;
    res
  ),
  (fun ?(force_fold=true) size ?(exact=true) ops ->
    generate ~in_context:false force_fold (size-1) exact ops Notations.([c0;c1])),
  (fun size ops holes ->
    let res =
      generate ~in_context:true false size false ops Notations.([c0;c1;mk_arg; hole 0 false])
    in
    let terms,contexts1,contexts2 =
      Array.fold_right (fun c ((ts,cs1,cs2) as acc)->
	match Term.holes c with
	| 0 -> (c::ts, cs1, cs2)
	| 1 -> (ts, c::cs1, cs2)
	| 2 -> (ts, cs1, Term.renumber_holes c::cs2)
	| _ -> acc
      ) res ([],[],[])
    in
    let n1 = List.length terms in
    let n2 = List.length contexts1 in
    let n3 = List.length contexts2 in
    let n = n1 + n2 + n3 in 
    let t = Array.create n Term.Notations.c0 in
    List.iteri (fun i c -> t.(i) <- c) terms;
    List.iteri (fun i c -> t.(i + n1) <- c) contexts1;
    List.iteri (fun i c -> t.(i+n1 +n2) <- c) contexts2;
    t
  )

let generate_constants ?(force_fold=true) size ?(exact=true) ops =
  Array.map (fun t -> Eval.eval t 0L) (generate_novar ~force_fold ~exact size ops)
