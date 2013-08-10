open Term

module IMap = Map.Make (struct type t = int64 let compare = Int64.compare end)


let main sizeC sizeT sizeE  =
  let ops = Generator.all_ops in 
  Printf.printf "synthesis: c %i t %i e %i\n%!" sizeC sizeT sizeE;
  let constants = Generator.generate_constants_witness sizeC ops in
  let constants = List.fold_right (fun (t,c) acc -> IMap.add c t acc) constants IMap.empty in
  Printf.printf "constants: %i\n%!" (IMap.cardinal constants);
  (* terms *)
  let terms = Array.of_list (Generator.generate ~force_fold:false sizeT ~exact:false ops) in
  Printf.printf "terms: %i\n%!" (Array.length terms);
  IMap.iter (fun k v -> 
    Printf.printf "%Lx %s\n" k (Print.(sprint (doc_exp  v)))
  ) constants;
  Printf.printf "=============================\n";
  Array.iter Print.print_exp_nl terms;;

 

  
let _ = main 6 6 0
