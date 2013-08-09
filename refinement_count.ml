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
  let average_count = 
    (Bitv.length selected_terms) / (Hashtbl.length answers)
  in
  Printf.printf "Max: %d\n" max_count;
  Printf.printf "Average: %d\n" average_count;
  (* (float_of_int max_count) /. (float_of_int (Bitv.length selected_terms)) *)
  ()

let not_so_random_bits p pmax =
  let s = ref 0L in
  for i = 0 to 63 do
    s := add !s (if (Random.int pmax > p) then (shift_left 1L i) else 0L)
  done;
  !s

let terms = Array.of_list (Generator.generate ~filter:false 7 Generator.all_ops)
let _ = Printf.printf "Total number of terms: %d\n" (Array.length terms)
let selected = Bitv.create (Array.length terms) true
(* let disc_value = 0x0000000000000001L *)
(* let _ = Printf.printf "%f\n" (refinement_measure terms selected disc_value) *)
let _ =
  (* let r = ref 1L in *)
  (* for i = 1 to 16 do *)
  (*   Printf.printf "Discrimination with power %d:\n" (4*i); *)
  (*   refinement_measure terms selected !r; *)
  (*   r := shift_left !r 4 *)
  (* done; *)
  Printf.printf "Discrimination with three random values\n";
  refinement_measure terms selected (Random.int64 Int64.max_int);
  refinement_measure terms selected (Random.int64 Int64.max_int);
  refinement_measure terms selected (Random.int64 Int64.max_int);
  Printf.printf "Discrimination with three sparse pseudo-clever values\n";
  refinement_measure terms selected (0x0001001001001000L);
  refinement_measure terms selected (0x0100001000010001L);
  refinement_measure terms selected (0x0010100001000100L);
  Printf.printf "Discrimination with three dense pseudo-clever values\n";
  refinement_measure terms selected (0xfffeffeffeffefffL);
  refinement_measure terms selected (0xfeffffeffffefffeL);
  refinement_measure terms selected (0xffefeffffefffeffL);
  Printf.printf "Discrimination with three not_so_random numbers\n";
  refinement_measure terms selected (not_so_random_bits 2 8);
  refinement_measure terms selected (not_so_random_bits 2 8);
  refinement_measure terms selected (not_so_random_bits 2 8)
