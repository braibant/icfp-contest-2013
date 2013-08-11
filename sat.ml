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

let run_minisat problems =
  let datas =
    List.map (fun problem ->
      let (in_file_name, chan) = Filename.open_temp_file "minisat_input" ".cnf" in
      Printf.fprintf chan "c blah\n";
      Printf.fprintf chan "p cnf %d %d\n" (problem.next_var-1) (List.length problem.clauses);
      List.iter (fun cl ->
	List.iter (Printf.fprintf chan "%d ") cl;
	Printf.fprintf chan "0\n")
	problem.clauses;
      close_out chan;
      let out_file_name = Filename.temp_file "minisat" ".out" in
      let cmd =
	Printf.sprintf "./minisat_static -rnd-seed=%d -verb=0 -cpu-lim=1 -mem-lim=100 %s %s > /dev/null"
	  (Random.bits ()) in_file_name out_file_name
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

let encode_formula state env t =
  let rec encode = function
    | C0 -> Array.make 64 zero_var
    | C1 ->
      let res = Array.make 64 zero_var in
      res.(0) <- -zero_var;
      res
    | Var x -> env.(x)
    | If0 (a, b, c, _) ->
      let a = encode a in
      let b = encode b in
      let c = encode c in
      let res = Array.init 64 (fun _ -> new_var state) in
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
      let a = encode a in
      let acc = ref (encode b) in
      for i = 0 to 7 do
	let env = Array.make 3 env.(Constants.arg) in
	env.(Constants.fold_acc) <- !acc;
	env.(Constants.fold_arg) <- Array.make 64 zero_var;
	Array.blit a (i*8) env.(Constants.fold_arg) 0 8;
	acc := encode c
      done;
      !acc
    | Op1 ([], _, _) -> assert false
    | Op1(Not :: q, a, _) ->
      let a = encode (Term.__op1 q a) in
      Array.map (fun x -> -x) a
    | Op1(Shl1::q , a, _) ->
      let a = encode (Term.__op1 q a) in
      let res = Array.make 64 zero_var in
      Array.blit a 0 res 1 63;
      res
    | Op1(Shr1::q, a, _) ->
      let a = encode (Term.__op1 q a) in
      let res = Array.make 64 zero_var in
      Array.blit a 1 res 0 63;
      res
    | Op1(Shr4::q, a, _) ->
      let a = encode (Term.__op1 q a) in
      let res = Array.make 64 zero_var in
      Array.blit a 4 res 0 60;
      res
    | Op1(Shr16::q, a, _) ->
      let a = encode (Term.__op1 q a) in
      let res = Array.make 64 zero_var in
      Array.blit a 16 res 0 48;
      res
    | Op2(_, [], _) -> assert false
    | Op2(And, t::q, _) ->
      let a = encode t in
      let b = encode (Term.__op2 And q) in
      let res = Array.init 64 (fun _ -> new_var state) in
      for i = 0 to 63 do
	add_clause state [-a.(i);-b.(i);res.(i)];
	add_clause state [a.(i);-res.(i)];
	add_clause state [b.(i);-res.(i)];
      done;
      res
    | Op2(Or, t::q, _) ->
      let a = encode t in
      let b = encode (Term.__op2 Or q) in
      let res = Array.init 64 (fun _ -> new_var state) in
      for i = 0 to 63 do
	add_clause state [a.(i);b.(i);-res.(i)];
	add_clause state [-a.(i);res.(i)];
	add_clause state [-b.(i);res.(i)];
      done;
      res
    | Op2(Xor, t::q, _) ->
      let a = encode t in
      let b = encode (Term.__op2 Xor q) in
      let res = Array.init 64 (fun _ -> new_var state) in
      for i = 0 to 63 do
	add_clause state [-a.(i);b.(i);res.(i)];
	add_clause state [a.(i);-b.(i);res.(i)];
	add_clause state [a.(i);b.(i);-res.(i)];
	add_clause state [-a.(i);-b.(i);-res.(i)];
      done;
      res
    | Op2(Plus, t::q, _) ->
      let a = encode t in
      let b = encode (Term.__op2 Plus q) in
      let res = Array.init 64 (fun _ -> new_var state) in
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
    | Hole _ -> assert false
  in encode t

let discriminate l =
  let pbs =
    List.map (fun (t1, t2) ->
      let state = init_state () in
      let env = [|Array.init 64 (fun _ -> new_var state)|] in
      let enc1 = encode_formula state env t1 in
      let enc2 = encode_formula state env t2 in
      let diff = Array.init 64 (fun _ -> new_var state) in
      for i = 0 to 63 do
	add_clause state [enc1.(i); enc2.(i); -diff.(i)];
	add_clause state [-enc1.(i); -enc2.(i); -diff.(i)]
      done;
      add_clause state (Array.to_list diff);
      (state, env.(0), t1, t2))
      l
  in
  let res = run_minisat (List.map (fun (x, _, _, _) -> x) pbs) in
  List.map2 (fun (_, env, t1, t2) res ->
    match res with
    | Sat data ->
	let res = ref 0L in
	for i = 0 to 63 do
	  if data.(env.(i)) then res := Int64.logor !res (Int64.shift_left 1L i)
	done;
	if Eval.eval t1 !res = Eval.eval t2 !res then
	  begin
	    Printf.printf "===== WARNING =====\n";
	    Printf.printf "Problem in SAT encoding : wrong discriminator\n";
	    Unknown
	  end
	else Sat !res
    | Unsat -> Unsat
    | Unknown -> Unknown)
    pbs res
