open Term

let zero_var =
  try int_of_string "10000000000000"
  with _ -> 10000000

type sat_state =
    { mutable next_var : int;
      mutable clauses : int list list }

let init_state () = { next_var = 1; clauses = [] }

let new_var state =
  let res = state.next_var in
  state.next_var <- res + 1;
  res

let add_clause state clause =
  if not (List.mem (-zero_var) clause) then
    let clause = List.filter ((<>) zero_var) clause in
    state.clauses <- clause :: state.clauses

type 'a sat_result =
  | Sat of 'a
  | Unsat
  | Unknown

let output_int chan n = output_string chan (string_of_int n)

let run_minisat problems =
  let datas =
    List.map (fun problem ->
      let (in_file_name, chan) = Filename.open_temp_file "minisat_input" ".cnf" in
      output_string chan "c blah\n";
      (* Printf.fprintf chan "p cnf %d %d\n" () (List.length problem.clauses); *)
      output_string chan "p cnf ";
      output_int chan (problem.next_var-1); output_char chan ' ';
      output_int chan (List.length problem.clauses); output_char chan '\n';
      List.iter (fun cl ->
	List.iter (fun i -> output_int chan i; output_char chan ' ') cl;
	output_string chan "0\n")
	problem.clauses;
      close_out chan;
      let out_file_name = Filename.temp_file "minisat" ".out" in
      let cmd =
	Printf.sprintf "./minisat_static -rnd-seed=%d -verb=1 -mem-lim=%d %s %s"
	  (Random.bits ()) (if !Config.teraram then 10000 else 1000) in_file_name out_file_name
      in
      (* Printf.printf "%s\n" cmd; *)
      (Unix.open_process_out cmd, in_file_name, out_file_name, problem))
      problems
  in
  List.map (fun (chan, in_file_name, out_file_name, problem) ->
    ignore (Unix.close_process_out chan);
    let res =
      try
	let chan = open_in out_file_name in
	try
	  match input_line chan with
	  | "UNSAT" -> close_in chan; Unsat
	  | "SAT" ->
	      let res = Array.make problem.next_var false in
	      for i = 1 to problem.next_var -1 do
		res.(i) <-
		  Scanf.fscanf chan " %d" (fun x -> assert (abs x = i); x > 0);
	      done;
	      close_in chan;
	      Sat res
	  | _ -> close_in chan; Unknown
	with
	| End_of_file -> close_in chan; Unknown
      with Sys_error _ -> Unknown
    in
    (try Unix.unlink in_file_name with Unix.Unix_error (_, _, _) -> ());
    (try Unix.unlink out_file_name with Unix.Unix_error (_, _, _) -> ());
    res)
    datas

let new_64var state = Array.init 64 (fun _ -> new_var state)

let encode_formula state env hole_env t =
  let rec encode env = function
    | C0 -> Array.make 64 zero_var
    | C1 ->
      let res = Array.make 64 zero_var in
      res.(0) <- -zero_var;
      res
    | Var x -> env.(x)
    | If0 (a, b, c, _) ->
      let a = encode env a in
      let b = encode env b in
      let c = encode env c in
      let res = new_64var state in
      let is_0 = new_var state in
      add_clause state (is_0::Array.to_list a);
      for i = 0 to 63 do
	add_clause state [-is_0;-a.(i)];
	add_clause state [-is_0;-b.(i);res.(i)];
	add_clause state [-is_0;b.(i);-res.(i)];
	add_clause state [is_0;-c.(i);res.(i)];
	add_clause state [is_0;c.(i);-res.(i)]
      done;
      res
    | Fold(a, b, c, _) ->
      let a = encode env a in
      let acc = ref (encode env b) in
      for i = 0 to 7 do
	let env = Array.make 3 env.(Constants.arg) in
	env.(Constants.fold_acc) <- !acc;
	env.(Constants.fold_arg) <- Array.make 64 zero_var;
	Array.blit a (i*8) env.(Constants.fold_arg) 0 8;
	acc := encode env c
      done;
      !acc
    | Op1 ([], _, _) -> assert false
    | Op1 (Bonus :: _, _, _) -> assert false
    | Op1(Not :: q, a, _) ->
      let a = encode env (Term.__op1 q a) in
      Array.map (fun x -> -x) a
    | Op1(Shl1::q , a, _) ->
      let a = encode env (Term.__op1 q a) in
      let res = Array.make 64 zero_var in
      Array.blit a 0 res 1 63;
      res
    | Op1(Shr1::q, a, _) ->
      let a = encode env (Term.__op1 q a) in
      let res = Array.make 64 zero_var in
      Array.blit a 1 res 0 63;
      res
    | Op1(Shr4::q, a, _) ->
      let a = encode env (Term.__op1 q a) in
      let res = Array.make 64 zero_var in
      Array.blit a 4 res 0 60;
      res
    | Op1(Shr16::q, a, _) ->
      let a = encode env (Term.__op1 q a) in
      let res = Array.make 64 zero_var in
      Array.blit a 16 res 0 48;
      res
    | Op2(_, [], _) -> assert false
    | Op2(And, t::q, _) ->
      let a = encode env t in
      let b = encode env (Term.__op2 And q) in
      let res = new_64var state in
      for i = 0 to 63 do
	add_clause state [-a.(i);-b.(i);res.(i)];
	add_clause state [a.(i);-res.(i)];
	add_clause state [b.(i);-res.(i)];
      done;
      res
    | Op2(Or, t::q, _) ->
      let a = encode env t in
      let b = encode env (Term.__op2 Or q) in
      let res = new_64var state in
      for i = 0 to 63 do
	add_clause state [a.(i);b.(i);-res.(i)];
	add_clause state [-a.(i);res.(i)];
	add_clause state [-b.(i);res.(i)];
      done;
      res
    | Op2(Xor, t::q, _) ->
      let a = encode env t in
      let b = encode env (Term.__op2 Xor q) in
      let res = new_64var state in
      for i = 0 to 63 do
	add_clause state [-a.(i);b.(i);res.(i)];
	add_clause state [a.(i);-b.(i);res.(i)];
	add_clause state [a.(i);b.(i);-res.(i)];
	add_clause state [-a.(i);-b.(i);-res.(i)];
      done;
      res
    | Op2(Plus, t::q, _) ->
      let a = encode env t in
      let b = encode env (Term.__op2 Plus q) in
      let res = new_64var state in
      let carry = Array.init 64 (fun i ->
	if i = 0 then zero_var else new_var state) in
      for i = 0 to 63 do
	if i < 63 then
	  begin
	    add_clause state [-a.(i); -b.(i); carry.(i+1)];
	    add_clause state [-a.(i); -carry.(i); carry.(i+1)];
	    add_clause state [-b.(i); -carry.(i); carry.(i+1)];
	    add_clause state [a.(i); b.(i); -carry.(i+1)];
	    add_clause state [a.(i); carry.(i); -carry.(i+1)];
	    add_clause state [b.(i); carry.(i); -carry.(i+1)];
	  end;
	add_clause state [-a.(i); -b.(i); -carry.(i); res.(i)];
	add_clause state [-a.(i); -b.(i); carry.(i); -res.(i)];
	add_clause state [-a.(i); b.(i); -carry.(i); -res.(i)];
	add_clause state [-a.(i); b.(i); carry.(i); res.(i)];
	add_clause state [a.(i); -b.(i); -carry.(i); -res.(i)];
	add_clause state [a.(i); -b.(i); carry.(i); res.(i)];
	add_clause state [a.(i); b.(i); -carry.(i); res.(i)];
	add_clause state [a.(i); b.(i); carry.(i); -res.(i)]
      done;
      res
    | Cst (v, _, _) ->
      Array.init 64 (fun i ->
	if Int64.logand 1L (Int64.shift_right_logical v i) = 1L then
	  -zero_var
	else zero_var)
    | Hole (n, _b) ->
      if Array.length hole_env < n then
        failwith (Printf.sprintf "Sat.encode: hole %d too large" n);
      hole_env.(n)
  in
  let formula = encode env t in
  formula

let add_diff_clause state enc1 enc2 =
  let diff = new_64var state in
  for i = 0 to 63 do
    add_clause state [enc1.(i); enc2.(i); -diff.(i)];
    add_clause state [-enc1.(i); -enc2.(i); -diff.(i)]
  done;
  add_clause state (Array.to_list diff)

let int64_of_var64 data var64 =
  let res = ref 0L in
  for i = 0 to 63 do
    if data.(var64.(i)) then res := Int64.logor !res (Int64.shift_left 1L i)
  done;
  !res

(* this function handles single pairs to discriminate, and accept
   contexts with holes; when Sat, it will return a valuation for the input,
   and for each hole *)
let discriminate_with_holes pairs = 
  let sat_problem (t1,t2) =
    let state = init_state () in
    let env = Array.init 3 (fun _ -> new_64var state) in
    let nb_holes = max (Term.max_hole t1) (Term.max_hole t2) in
    let holes = Array.init nb_holes (fun _ -> new_64var state) in
    let enc1 = encode_formula state env holes t1 in
    let enc2 = encode_formula state env holes t2 in
    add_diff_clause state enc1 enc2;
    (state, (t1, t2, env, holes)) in
  let problem_states, problem_vars = List.split (List.map sat_problem pairs) in
  let minisat_results = run_minisat problem_states in
  let handle_result result (t1, t2, env, holes) =
    match result with
      | Sat data ->
        let discr_input = Array.map (int64_of_var64 data) env in
        let discr_holes = Array.map (int64_of_var64 data) holes in
        (* Here I don't know how to use Eval.foo to verify that
           the discriminating value (and hole values) are indeed correct *)
        ignore discr_input; ignore discr_holes;
        Sat (discr_input, discr_holes)
      | Unsat -> Unsat
      | Unknown -> Unknown
  in
  List.map2 handle_result minisat_results problem_vars

(* this function takes a (term*term) list as input,
   but expects only terms, no contexts with holes *)
let discriminate l =
  let pbs =
    List.map (fun (t1, t2) ->
      let state = init_state () in
      let env = [|new_64var state|] in
      let no_holes = [||] in
      let enc1 = encode_formula state env no_holes t1 in
      let enc2 = encode_formula state env no_holes t2 in
      add_diff_clause state enc1 enc2;
      (state, env.(0), t1, t2))
      l
  in
  let res = run_minisat (List.map (fun (x, _, _, _) -> x) pbs) in
  List.map2 (fun (_, input, t1, t2) res ->
    match res with
    | Sat data ->
        let res = int64_of_var64 data input in
	if Eval.eval t1 res = Eval.eval t2 res then
	  begin
	    Printf.printf "===== WARNING =====\n";
	    Printf.printf "Problem in SAT encoding : wrong discriminator\n";
	    Unknown
	  end
	else Sat res
    | Unsat -> Unsat
    | Unknown -> Unknown)
    pbs res

let synthesis n_if0 n_binop n_unop ops keys values =
  let n_atoms = 2*n_if0 + n_binop + 1 in
  let state = init_state () in
  let n_connect = 1 + 3*n_if0 + 2*n_binop + n_unop in

  (* matrix is down,top vars *)
  (* down vars are root if0 binop unop *)
  (* top vars are if0 binop unop atoms *)
  let connect =
    Array.init n_connect (fun _ -> Array.init n_connect (fun _ -> new_var state))
  in
  (* Each down is connected to (at least) one top *)
  for i = 0 to n_connect-1 do
    add_clause state (Array.to_list connect.(i))
  done;
  (* Each top is connected to (at most) one bottom *)
  for i = 0 to n_connect-1 do
    for j = 0 to n_connect-1 do
      for k = j+1 to n_connect-1 do
	add_clause state [-connect.(j).(i);-connect.(k).(i)]
      done
    done
  done;
  let binop_op2 op =
    if OSet.mem (Op2o op) ops then
      Array.init n_binop (fun _ -> new_var state)
    else
      Array.make n_binop zero_var
  in

  (* Binop *)
  let binop_and = binop_op2 And in
  let binop_or = binop_op2 Or in
  let binop_xor = binop_op2 Xor in
  let binop_plus = binop_op2 Plus in
  (* Each binop is and, or, xor or plus *)
  for i = 0 to n_binop-1 do
    add_clause state [binop_and.(i);binop_or.(i);
		      binop_xor.(i);binop_plus.(i)]
  done;
  let unop_op1 op =
    if OSet.mem (Op1o op) ops then
      Array.init n_unop (fun _ -> new_var state)
    else
      Array.make n_unop zero_var
  in

  (* Unop *)
  let unop_not = unop_op1 Not in
  let unop_shl1 = unop_op1 Shl1 in
  let unop_shr1 = unop_op1 Shr1 in
  let unop_shr4 = unop_op1 Shr4 in
  let unop_shr16 = unop_op1 Shr16 in
  (* Each unop is not, shl1, shr1, shr4 or shr16 *)
  for i = 0 to n_unop-1 do
    add_clause state [unop_not.(i);unop_shl1.(i);
		      unop_shr1.(i);unop_shr4.(i);unop_shr16.(i)]
  done;

  (* Atoms *)
  let atom_c0 = Array.init n_atoms (fun _ -> new_var state) in
  let atom_c1 = Array.init n_atoms (fun _ -> new_var state) in
  let atom_arg = Array.init n_atoms (fun _ -> new_var state) in
  (* Each atom is c0 c1 or arg *)
  for i = 0 to n_atoms-1 do
    add_clause state [atom_c0.(i); atom_c1.(i); atom_arg.(i)]
  done;

  List.iter2 (fun key value ->
    let top =
      Array.init n_connect (fun _ -> Array.init 64 (fun _ -> new_var state))
    in
    let down =
      Array.init n_connect (fun _ -> Array.init 64 (fun _ -> new_var state))
    in

    (* When top and down are connected, they have the same value *)
    for i = 0 to n_connect-1 do
      for j = 0 to n_connect-1 do
	for k = 0 to 63 do
	  add_clause state [-connect.(i).(j); -top.(j).(k); down.(i).(k)];
	  add_clause state [-connect.(i).(j); top.(j).(k); -down.(i).(k)]
	done
      done
    done;

    (* Root encoding *)
    for i = 0 to 63 do
      if Int64.logand 1L (Int64.shift_right_logical value i) = 1L then
	add_clause state [down.(0).(i)]
      else
	add_clause state [-down.(0).(i)]
    done;

    (* if0 encoding *)
    for i = 0 to n_if0-1 do
      let top = top.(i) in
      let downc = down.(i+1) in
      let down1 = down.(n_if0+i+1) in
      let down2 = down.(2*n_if0+i+1) in
      let is_0 = new_var state in
      add_clause state (is_0::Array.to_list downc);
      for i = 0 to 63 do
	add_clause state [-is_0;-downc.(i)];
	add_clause state [-is_0;-down1.(i);top.(i)];
	add_clause state [-is_0;down1.(i);-top.(i)];
	add_clause state [is_0;-down2.(i);top.(i)];
	add_clause state [is_0;down2.(i);-top.(i)]
      done;
    done;

    (* binop encoding *)
    for i = 0 to n_binop-1 do
      let top = top.(n_if0+i) in
      let down1 = down.(3*n_if0+i+1) in
      let down2 = down.(3*n_if0+n_binop+i+1) in
      let carry =
	if OSet.mem (Op2o Plus) ops then
	  Array.init 64 (fun i -> if i = 0 then zero_var else new_var state)
	else
	  Array.make 64 zero_var
      in
      for j = 0 to 63 do
	add_clause state [-binop_and.(i); down1.(j); -top.(j)];
	add_clause state [-binop_and.(i); down2.(j); -top.(j)];
	add_clause state [-binop_and.(i); -down1.(j); -down2.(j); top.(j)];
	add_clause state [-binop_or.(i); -down1.(j); top.(j)];
	add_clause state [-binop_or.(i); -down2.(j); top.(j)];
	add_clause state [-binop_or.(i); down1.(j); down2.(j); -top.(j)];
	add_clause state [-binop_xor.(i); -down1.(j); -down2.(j); -top.(j)];
	add_clause state [-binop_xor.(i); -down1.(j); down2.(j); top.(j)];
	add_clause state [-binop_xor.(i); down1.(j); -down2.(j); top.(j)];
	add_clause state [-binop_xor.(i); down1.(j); down2.(j); -top.(j)];
	if j < 63 && OSet.mem (Op2o Plus) ops then
	  begin
	    add_clause state [-down1.(j); -down2.(j); carry.(j+1)];
	    add_clause state [-down1.(j); -carry.(j); carry.(j+1)];
	    add_clause state [-down2.(j); -carry.(j); carry.(j+1)];
	    add_clause state [down1.(j); down2.(j); -carry.(j+1)];
	    add_clause state [down1.(j); carry.(j); -carry.(j+1)];
	    add_clause state [down2.(j); carry.(j); -carry.(j+1)]
	  end;
	add_clause state [-binop_plus.(i); -down1.(j); -down2.(j); -carry.(j); top.(j)];
	add_clause state [-binop_plus.(i); -down1.(j); -down2.(j); carry.(j); -top.(j)];
	add_clause state [-binop_plus.(i); -down1.(j); down2.(j); -carry.(j); -top.(j)];
	add_clause state [-binop_plus.(i); -down1.(j); down2.(j); carry.(j); top.(j)];
	add_clause state [-binop_plus.(i); down1.(j); -down2.(j); -carry.(j); -top.(j)];
	add_clause state [-binop_plus.(i); down1.(j); -down2.(j); carry.(j); top.(j)];
	add_clause state [-binop_plus.(i); down1.(j); down2.(j); -carry.(j); top.(j)];
	add_clause state [-binop_plus.(i); down1.(j); down2.(j); carry.(j); -top.(j)]
      done;
    done;

    (* unop encoding *)
    for i = 0 to n_unop-1 do
      let top = top.(n_if0+n_binop+i) in
      let down = down.(3*n_if0+2*n_binop+i+1) in
      for j = 0 to 63 do
	add_clause state [-unop_not.(i); -down.(j); -top.(j)];
	add_clause state [-unop_not.(i); down.(j); top.(j)];
	if j > 0 then
	  begin
	    add_clause state [-unop_shl1.(i); -down.(j-1); top.(j)];
	    add_clause state [-unop_shl1.(i); down.(j-1); -top.(j)]
	  end
	else
	  add_clause state [-unop_shl1.(i); -top.(j)];
	if j < 63 then
	  begin
	    add_clause state [-unop_shr1.(i); -down.(j+1); top.(j)];
	    add_clause state [-unop_shr1.(i); down.(j+1); -top.(j)]
	  end
	else
	  add_clause state [-unop_shr1.(i); -top.(j)];
	if j < 60 then
	  begin
	    add_clause state [-unop_shr4.(i); -down.(j+4); top.(j)];
	    add_clause state [-unop_shr4.(i); down.(j+4); -top.(j)]
	  end
	else
	  add_clause state [-unop_shr4.(i); -top.(j)];
	if j < 48 then
	  begin
	    add_clause state [-unop_shr16.(i); -down.(j+16); top.(j)];
	    add_clause state [-unop_shr16.(i); down.(j+16); -top.(j)]
	  end
	else
	  add_clause state [-unop_shr16.(i); -top.(j)]
      done;
    done;

    (* atoms encoding *)
    for i = 0 to n_atoms-1 do
      let top = top.(n_if0+n_binop+n_unop+i) in
      for j = 0 to 63 do
	add_clause state [-atom_c0.(i);-top.(j)];
	add_clause state [-atom_c1.(i); if j = 0 then top.(j) else -top.(j)];
	add_clause state [-atom_arg.(i);
			  if Int64.logand 1L (Int64.shift_right_logical key j) = 1L
			  then top.(j) else -top.(j)];
      done
    done)
    keys values;

  state,
  fun data ->
    let data x = if x = zero_var then false else data.(x) in
    let rec exp_of_top x =
      if x < n_if0 then
	Notations.if0 (exp_of_down (1+x)) (exp_of_down (1+n_if0+x))
	    (exp_of_down (1+2*n_if0+x))
      else let x = x-n_if0 in
      if x < n_binop then
	let op =
	  if data (binop_and.(x)) then And
	  else if data (binop_or.(x)) then Or
	  else if data (binop_xor.(x)) then Xor
	  else if data (binop_plus.(x)) then Plus
	  else assert false
	in
	Notations.op2 op (exp_of_down (3*n_if0+x+1))
	  (exp_of_down (3*n_if0+n_binop+x+1))
      else let x = x-n_binop in
      if x < n_unop then
	let op =
	  if data (unop_not.(x)) then Not
	  else if data (unop_shl1.(x)) then Shl1
	  else if data (unop_shr1.(x)) then Shr1
	  else if data (unop_shr4.(x)) then Shr4
	  else if data (unop_shr16.(x)) then Shr16
	  else assert false
	in
	Notations.op1 op (exp_of_down (3*n_if0+2*n_binop+x+1))
      else let x = x-n_unop in
      assert (x<n_atoms);
      if data (atom_c0.(x)) then Notations.c0
      else if data (atom_c1.(x)) then Notations.c1
      else if data (atom_arg.(x)) then Notations.mk_arg
      else assert false
    and exp_of_down x =
      let rec aux n =
	assert (n < n_connect);
	if data (connect.(x).(n)) then exp_of_top n
	else aux (n+1)
      in aux 0
    in
    exp_of_down 0
