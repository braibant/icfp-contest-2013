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
	Printf.sprintf "./minisat_static -rnd-seed=%d -verb=0 -cpu-lim=1 -mem-lim=%d %s %s > /dev/null"
	  (Random.bits ()) (if !Config.teraram then 10000 else 100) in_file_name out_file_name
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
