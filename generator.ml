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
    | If0(a,b,c) ->
	operators a (operators b (operators c (OSet.add If0o acc)))
    | Fold(a,b,c) ->
	operators a (operators b (operators c (OSet.add Foldo acc)))
    | Op1(op,a) ->
	operators a (OSet.add (Op1o op) acc)
    | Op2(op,a,b) ->
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
      memo.(0).(1) <- Some (Var Constants.fold_acc::Var Constants.fold_arg::atoms);
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
		for i = 1 to size-3 do
		  for j = 1 to size-2-i do
		    let gen1 = aux i Forbidden in
		    let gen2 = aux j Inside in
		    let gen3 = aux (size-1-i-j) Forbidden in
		    acc:=
		      List.fold_left (fun acc x ->
			List.fold_left (fun acc y ->
			  List.fold_left (fun acc z -> Fold (x,y,z)::acc) acc gen3)
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
		  (fun acc x -> Op1 (op,x)::acc)
		  acc
		  (aux (size-1) fold_state)
	    | Op2o op ->
		if fold_state = Required then
		  begin
		    let acc = ref acc in
		    for i = 1 to size-5 do
		      let genl = aux i Required in
		      let genr = aux (size-1-i) Forbidden in
		      acc:=
			List.fold_left (fun acc x ->
			  List.fold_left (fun acc y -> Op2 (op,x,y)::acc)
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
			    else Op2 (op,x,y)::acc)
			    acc genr)
			  !acc genl
		    done;
		    !acc
		  end
	    | If0o ->
		if fold_state = Required then
		  begin
		    let acc = ref acc in
		    for i = 4 to size-3 do
		      for j = 1 to size-2-i do
			let gen1 = aux i Required in
			let gen2 = aux j Forbidden in
			let gen3 = aux (size-1-i-j) Forbidden in
			acc:=
			  List.fold_left (fun acc x ->
			    List.fold_left (fun acc y ->
			      List.fold_left (fun acc z -> If0 (x,y,z)::acc) acc gen3)
			      acc gen2)
			    !acc gen1
		      done;
		    done;
		    for i = 1 to size-6 do
		      for j = 4 to size-2-i do
			let gen1 = aux i Forbidden in
			let gen2 = aux j Required in
			let gen3 = aux (size-1-i-j) Forbidden in
			acc:=
			  List.fold_left (fun acc x ->
			    List.fold_left (fun acc y ->
			      List.fold_left (fun acc z -> If0 (x,y,z)::acc) acc gen3)
			      acc gen2)
			    !acc gen1
		      done;
		    done;
		    for i = 1 to size-6 do
		      for j = 1 to size-5-i do
			let gen1 = aux i Forbidden in
			let gen2 = aux j Forbidden in
			let gen3 = aux (size-1-i-j) Required in
			acc:=
			  List.fold_left (fun acc x ->
			    List.fold_left (fun acc y ->
			      List.fold_left (fun acc z -> If0 (x,y,z)::acc) acc gen3)
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
			    List.fold_left (fun acc z -> If0 (x,y,z)::acc) acc gen3)
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
    (generate filter size exact ops [C0;C1;Var Constants.arg])),
  (fun ?(filter=true) size ?(exact=true) ops ->
    let ops = OSet.remove Foldo ops in
    let size = size-3 in
    let lst =
      generate filter size exact ops
	[Var Constants.fold_acc;Var Constants.fold_arg;C0;C1]
    in
    List.rev_map (fun t -> Fold (Var Constants.arg, C0, t)) lst),
  (fun ?(filter=true) size ?(exact=true) ops -> generate filter size exact ops [C0;C1])

let generate_constants ?(filter=true) size ?(exact=true) ops =
  List.rev_map (fun t -> eval t 0L) (generate_novar ~filter ~exact size ops)
