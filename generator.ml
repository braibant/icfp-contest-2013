open Term

type op =
| If0o
| Foldo
| Op1o of op1
| Op2o of op2

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
    | C0 | C1 | Var _ -> acc
    | If0(a,b,c,_) ->
	operators a (operators b (operators c (OSet.add If0o acc)))
    | Fold(a,b,c,_) ->
	operators a (operators b (operators c (OSet.add Foldo acc)))
    | Op1(op,a,_) ->
	operators a (OSet.add (Op1o op) acc)
    | Op2(op,a,b,_) ->
	operators a (operators b (OSet.add (Op2o op) acc))
  in
  operators t OSet.empty

type fold_state =
  | Required
  | Inside
  | Forbidden

let generate, generate_tfold, generate_novar =
  let generate filter size exact ops atoms =
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
	memo.(size-1).(fold_state_int) <- Some
	begin
	  assert (size > 1);
	  let acc =
	    if fold_state = Required then
	      begin
		let acc = ref [] in
		for i = 1 to size-4 do
		  for j = 1 to size-3-i do
		    let gen1 = aux i Forbidden in
		    let gen2 = aux j Inside in
		    let gen3 = aux (size-2-i-j) Forbidden in
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
		      let genl = aux i Forbidden in
		      let genr = aux (size-1-i) Forbidden in
		      acc:=
			List.fold_left (fun acc x ->
			  List.fold_left (fun acc y ->
			    if i = size-1-i && y < x then acc
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
		      let gen1 = aux i Forbidden in
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
		  !acc
	    | Foldo -> assert false)
	    ops acc
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
    if filter then
      let ops' = if has_fold then OSet.add Foldo ops else ops in
      let lst = aux2 size (if has_fold then Required else Forbidden) in
      List.filter (fun t -> OSet.equal (operators t) ops') lst
    else
      let lst = aux2 size Forbidden in
      if has_fold then List.rev_append (aux size Required) lst else lst
  in
  (fun ?(filter=true) size ?(exact=true) ops ->
    generate filter (size-1) exact ops Notations.([c0;c1;mk_arg])),
  (fun ?(filter=true) size ?(exact=true) ops ->
    let ops = OSet.remove Foldo ops in
    let size = size-5 in
    let lst =
      generate filter size exact ops Notations.([c0;c1;mk_facc;mk_farg])
    in
    List.rev_map (fun t -> Notations.(fold mk_arg c0 t)) lst),
  (fun ?(filter=true) size ?(exact=true) ops ->
    generate filter (size-1) exact ops Notations.([c0;c1]))

let generate_constants ?(filter=true) size ?(exact=true) ops =
  List.rev_map (fun t -> eval t 0L) (generate_novar ~filter ~exact size ops)
