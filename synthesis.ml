module IMap = Map.Make (struct type t = int64 let compare = Int64.compare end)
module VMap = Map.Make(Vect)
module VSet = Set.Make(Vect)

module Context = struct
  open Term
  (* ensure that the free variables are normalized between 0 and the
     number of free variables... *)
  type t = exp
      
  let eval = Eval.h_evalv

  let multimap_fold (f: Vect.t -> 'a -> 'b -> 'b) 
      (map : 'a list VMap.t) (acc: 'b) : 'b  =
    VMap.fold 
      (fun k  ->
	List.fold_right (fun v acc -> f k v acc) 
      ) map acc
      
  let fit (space: Term.exp list VMap.t) src (tgt: Vect.t) c : Term.exp array list  =
    let n = max 0 (Term.holes c) in 
    let sigma = Array.create n (Vect.zero, Term.Notations.c0) in
    let rec aux i acc =
      if i = n then 
	let sigma1 = Array.map fst sigma in
	let sigma2 = Array.map snd sigma in 
	 (if Vect.equal (eval c sigma1 src) tgt
	 then sigma2 :: acc
	 else acc)
      else
	multimap_fold (fun v t acc ->
	  sigma.(i) <- v,t;
	  aux (i+1) acc
	)
	  space acc
    in 
    aux 0  []

end


let synthesis 
    (terms: Term.exp array) 
    (contexts: Context.t list)
    (keys: int64 array) (values: int64 array)
    : Term.exp list 
    =
  let map =
    let map = ref VMap.empty in 
    let add c t = 
      try map := VMap.add c (t :: VMap.find c !map) !map
      with Not_found -> map := VMap.add c [t] !map
    in
    for i = 0 to Array.length terms -1 do
      let img = Eval.evalv terms.(i) keys in 
      add img terms.(i)
    done; !map
  in
  List.fold_right (fun c acc ->
    match Context.fit map keys values c with
    | [] -> acc
    | pts -> 
      List.rev_append (List.map (fun (prg) -> Term.subst_holes prg c) pts)
	(acc)
  ) contexts []
  
  
let main sizeT sizeE ops  =
  Printf.printf "synthesis:  t %i e %i\n%!" sizeT sizeE;
  (* let constants = Generator.generate_constants_witness sizeC ops in *)
  (* let constants =  *)
  (*   List.fold_right (fun (t,c) acc -> IMap.add c t acc) constants IMap.empty in *)
  (* Printf.printf "constants: %i\n%!" (IMap.cardinal constants); *)
  (* terms *)
  let terms = Array.of_list (Generator.generate ~force_fold:false sizeT ~exact:false ops) in
  Printf.printf "terms: %i\n%!" (Array.length terms);
  let o = open_out "terms.debug" in 
  Array.iter (fun t -> Print.(fprint o (doc_exp t ^^ hardline)))  terms;
  close_out o;
  (* contexts *)
  let contexts = (Generator.generate_context sizeE ops ([Term.Notations.hole 0 false])) in 
  Printf.printf "contexts: %i\n%!" (List.length contexts);
  List.iter Print.print_exp_nl contexts;
  synthesis terms (contexts)  
 

