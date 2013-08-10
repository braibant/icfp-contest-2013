open Term

let zero_var = 10000000000000

type sat_state =
    { mutable next_var : int;
      mutable clauses : int list list }

let new_var state =
  let res = state.next_var in
  state.next_var <- res + 1;
  res

let add_clause state clause =
  if not (List.mem (-zero_var) clause) then
    let clause = List.filter ((<>) zero_var) clause in
    state.clauses <- clause :: state.clauses

let rec encode_formula state env t =
  match t with
  | C0 -> Array.make 64 zero_var
  | C1 ->
      let res = Array.make 64 zero_var in
      res.(0) <- -zero_var;
      res
  | Var x -> env.(x)
  | If0 (a, b, c, _) ->
      let a = encode_formula state env a in
      let b = encode_formula state env b in
      let c = encode_formula state env c in
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
      let a = encode_formula state env a in
      let acc = ref (encode_formula state env b) in
      for i = 0 to 7 do
	let env = Array.make 3 env.(Constants.arg) in
	env.(Constants.fold_acc) <- !acc;
	env.(Constants.fold_arg) <- Array.make 64 zero_var;
	Array.blit a (i*8) env.(Constants.fold_arg) 0 8;
	acc := encode_formula state env c
      done;
      !acc
  | Op1(Not, a, _) ->
      let a = encode_formula state env a in
      Array.map (fun x -> -x) a
  | Op1(Shl1, a, _) ->
      let a = encode_formula state env a in
      let res = Array.make 64 zero_var in
      Array.blit a 0 res 1 63;
      res
  | Op1(Shr1, a, _) ->
      let a = encode_formula state env a in
      let res = Array.make 64 zero_var in
      Array.blit a 1 res 0 63;
      res
  | Op1(Shr4, a, _) ->
      let a = encode_formula state env a in
      let res = Array.make 64 zero_var in
      Array.blit a 4 res 0 60;
      res
  | Op1(Shr16, a, _) ->
      let a = encode_formula state env a in
      let res = Array.make 64 zero_var in
      Array.blit a 16 res 0 48;
      res
  | Op2(And, a, b, _) ->
      let a = encode_formula state env a in
      let b = encode_formula state env b in
      let res = Array.init 64 (fun _ -> new_var state) in
      for i = 0 to 63 do
	add_clause state [-a.(i);-b.(i);res.(i)];
	add_clause state [-a.(i);-res.(i)];
	add_clause state [-b.(i);-res.(i)];
      done;
      res
  | Op2(Or, a, b, _) ->
      let a = encode_formula state env a in
      let b = encode_formula state env b in
      let res = Array.init 64 (fun _ -> new_var state) in
      for i = 0 to 63 do
	add_clause state [a.(i);b.(i);-res.(i)];
	add_clause state [a.(i);res.(i)];
	add_clause state [b.(i);res.(i)];
      done;
      res
  | Op2(Xor, a, b, _) ->
      let a = encode_formula state env a in
      let b = encode_formula state env b in
      let res = Array.init 64 (fun _ -> new_var state) in
      for i = 0 to 63 do
	add_clause state [-a.(i);b.(i);res.(i)];
	add_clause state [a.(i);-b.(i);res.(i)];
	add_clause state [a.(i);b.(i);-res.(i)];
	add_clause state [-a.(i);-b.(i);-res.(i)];
      done;
      res
  | Op2(Plus, a, b, _) ->
      let a = encode_formula state env a in
      let b = encode_formula state env b in
      let res = Array.init 64 (fun _ -> new_var state) in
      let carry = Array.init 64 (fun _ -> new_var state) in
      add_clause state [-carry.(0)];
      for i = 0 to 63 do
	if i < 63 then
	  begin
	    add_clause state [-a.(i); -b.(i); carry.(i+1)];
	    add_clause state [-a.(i); -carry.(i); carry.(i+1)];
	    add_clause state [-b.(i); -carry.(i); carry.(i+1)]
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
