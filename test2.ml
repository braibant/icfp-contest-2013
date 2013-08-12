open Generator
open Term
open Notations 

(* let () = *)
(*   List.iter (fun x -> Printf.printf "%Lx\n" x) *)
(*     (List.sort *)
(*        compare *)
(*        (generate_constants ~filter:false ~exact:false 9 all_ops)) *)



(* let lst = *)
(*   generate 8 ~exact:false (ops_from_list [Op1o Shr1]) *)

(* let () = *)
(*   Array.iter (fun x -> Print.print_exp x; print_newline ()) *)
(*     lst *)


(* let t1 = *)
(*   Notations.(fold c1 c0 (c1 || (mk_arg ** (shl1 (shr1 mk_facc))))) *)
(* let t2 = *)
(*   Notations.(fold (mk_arg || c1) c0 (mk_arg || (mk_farg ** mk_facc))) *)


let rec loop t n_if0 n_op2 n_op1 ops keys values =
  Printf.printf "%!\n";
  let pb,decode = Sat.synthesis n_if0 n_op2 n_op1 ops keys values in
  let [Sat.Sat sol] = Sat.run_minisat [pb] in
  let t_sol = decode sol in
  let disc = Sat.discriminate [t,t_sol] in
  Print.print_exp t_sol; print_newline ();
  Print.print_exp t; print_newline ();
  match disc with
  | [Sat.Sat v] ->
      Printf.printf "Discriminator : %Lx %Lx %Lx\n" v (Eval.eval t v) (Eval.eval t_sol v);
      loop t n_if0 n_op2 n_op1 ops (v::keys) (Eval.eval t v::values)
  | [Sat.Unsat] ->
      Printf.printf "Found !\n"
  | [Sat.Unknown] -> Printf.printf "Unknown\n"

let _ =
  let open Protocol.Training in
  match Net.send_training ({
    Request.size = Some 17;
    Request.operators = Some []
  })
  with
  | `Training_body pb ->
    let t = Parser.prog_of_string (pb.Response.challenge) in
    let keys = Array.to_list (Array.init 2 (fun _ -> rnd64 ())) in
    let values = List.map (Eval.eval t) keys in
    loop
      t
      (Parser.if0_of_string (pb.Response.challenge))
      (Parser.ops2_of_string (pb.Response.challenge))
      (Parser.ops1_of_string (pb.Response.challenge))
      (Parser.ops_of_string (pb.Response.challenge))
      keys values

  | #Net.unexpected as other ->
      invalid_arg (Printf.sprintf "train: %s" (Net.str_of_return other))
