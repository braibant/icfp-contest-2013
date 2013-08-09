open Term

type op =
| If0o
| Foldo
| Op1o of op1
| Op2o of op2

module OSet = Set.Make(struct type t=op let compare=compare end)

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

let generate size ops =
  let rec generate size ops atoms =
    match size with
    | 1 -> if OSet.mem Foldo ops then [] else atoms
    | n ->
	assert (n > 1);
	OSet.fold (fun op acc ->
	  match op with
	  | Op1o op ->
	      List.fold_left
		(fun acc x -> Op1 (op,x)::acc)
		acc
		(generate (size-1) ops atoms)
	  | Op2o op ->
	      if OSet.mem Foldo ops then
		begin
		  let acc = ref [] in
		  for i = 1 to size-5 do
		    let genl = generate i ops atoms in
		    let genr = generate (size-1-i) (OSet.remove Foldo ops) atoms in
		    acc:=
		      List.fold_left (fun acc x ->
			List.fold_left (fun acc y -> Op2 (op,x,y)::acc)
			  acc genr)
			!acc genl
		  done;
		  for i = 4 to size-2 do
		    let genl = generate i (OSet.remove Foldo ops) atoms in
		    let genr = generate (size-1-i) ops atoms in
		    acc:=
		      List.fold_left (fun acc x ->
			List.fold_left (fun acc y -> Op2 (op,x,y)::acc)
			  acc genr)
			!acc genl
		  done;
		  !acc
		end
	      else
		let acc = ref [] in
		for i = 1 to size-2 do
		  let genl = generate i ops atoms in
		  let genr = generate (size-1-i) ops atoms in
		  acc:=
		    List.fold_left (fun acc x ->
		      List.fold_left (fun acc y -> Op2 (op,x,y)::acc)
			acc genr)
		      !acc genl
		done;
		!acc
	  | If0o ->
	      if OSet.mem Foldo ops then
		begin
		  let acc = ref acc in
		  for i = 1 to size-3 do
		    for j = 1 to size-2-i do
		      let gen1 = generate i (OSet.remove Foldo ops) atoms in
		      let gen2 = generate j ops atoms in
		      let gen3 = generate (size-1-i-j) ops atoms in
		      acc:=
			List.fold_left (fun acc x ->
			  List.fold_left (fun acc y ->
			    List.fold_left (fun acc z -> If0 (x,y,z)::acc) acc gen3)
			    acc gen2)
			  !acc gen1
		    done;
		  done;
		  for i = 1 to size-3 do
		    for j = 1 to size-2-i do
		      let gen1 = generate i ops atoms in
		      let gen2 = generate j (OSet.remove Foldo ops) atoms in
		      let gen3 = generate (size-1-i-j) ops atoms in
		      acc:=
			List.fold_left (fun acc x ->
			  List.fold_left (fun acc y ->
			    List.fold_left (fun acc z -> If0 (x,y,z)::acc) acc gen3)
			    acc gen2)
			  !acc gen1
		    done;
		  done;
		  for i = 1 to size-3 do
		    for j = 1 to size-2-i do
		      let gen1 = generate i ops atoms in
		      let gen2 = generate j ops atoms in
		      let gen3 = generate (size-1-i-j) (OSet.remove Foldo ops) atoms in
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
		  let gen1 = generate i ops atoms in
		  let gen2 = generate j ops atoms in
		  let gen3 = generate (size-1-i-j) ops atoms in
		  acc:=
		    List.fold_left (fun acc x ->
		      List.fold_left (fun acc y ->
			List.fold_left (fun acc z -> If0 (x,y,z)::acc) acc gen3)
			acc gen2)
		      !acc gen1
		done;
	      done;
	      !acc
	  | Foldo ->
	      let acc = ref acc in
	      for i = 1 to size-3 do
		for j = 1 to size-2-i do
		  let gen1 = generate i (OSet.remove Foldo ops) atoms in
		  let gen2 = generate j (OSet.remove Foldo ops)
		      (Var Constants.fold_acc::Var Constants.fold_arg::atoms)
		  in
		  let gen3 = generate (size-1-i-j) ops atoms in
		  acc:=
		    List.fold_left (fun acc x ->
		      List.fold_left (fun acc y ->
			List.fold_left (fun acc z -> Fold (x,y,z)::acc) acc gen3)
			acc gen2)
		      !acc gen1
		done;
	      done;
	      !acc)
	  ops atoms
  in
  generate size ops [C0;C1]
