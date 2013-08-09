open Term
open Int64

let refinement_measure (terms: exp array) (selected_terms: Bitv.t) (disc_value: int64) =
  let answers = Hashtbl.create 1021 in
  Bitv.iteri_true
    (fun i ->
      let ans = Term.eval terms.(i) disc_value in
      try
	Hashtbl.replace answers ans (1 + Hashtbl.find answers ans)
      with
      | Not_found -> Hashtbl.add answers ans 1
    ) selected_terms;
  let max_count =
    Hashtbl.fold
      (fun key count max_count -> max count max_count)
      answers 0
  in
  Printf.printf "Max: %d\n" max_count;
  (* Printf.printf "Nombre: %d\n" (Bitv.length selected_terms); *)
  (float_of_int max_count) /. (float_of_int (Bitv.length selected_terms))

let terms = Array.of_list (Generator.generate ~filter:false 7 Generator.all_ops)
let _ = Printf.printf "Total number of terms: %d\n" (Array.length terms)
let selected = Bitv.create (Array.length terms) true
(* let disc_value = 0x0000000000000001L *)
(* let _ = Printf.printf "%f\n" (refinement_measure terms selected disc_value) *)
let _ =
  let r = ref 1L in
  for i = 1 to 64 do
    Printf.printf "Discrimination with %s:\n" (Int64.to_string !r);
    refinement_measure terms selected !r;
    r := shift_left !r 1
  done
