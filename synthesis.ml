module IMap = Map.Make (struct type t = int64 let compare = Int64.compare end)
module VMap = Map.Make(Vect)
module VSet = Set.Make(Vect)

module Context = struct
  open Term
  (* ensure that the free variables are normalized between 0 and the
     number of free variables... *)
  type t = exp
      
  let eval = Eval.h_evalv

  let fit space src (tgt: Vect.t) c : Vect.t array list  =
<<<<<<< HEAD
    let n = Term.holes c in 
=======
    let n = max 0 (Term.holes c) in 
>>>>>>> Synthesis of suitable terms, not tested
    let sigma = Array.create n Vect.zero in
    let rec aux i space acc =
      if i = n then 
	 (if Vect.equal (eval c sigma src) tgt
	 then sigma :: acc
	 else acc)
      else
	VSet.fold (fun v acc ->
	  sigma.(i) <- v;
	  aux (i+1) (VSet.remove v space) acc
	) space acc
    in 
    aux 0 space []

end


let synthesis 
    (constants: Term.exp IMap.t) (terms: Term.exp array) 
    (contexts: Context.t list)
    (keys: int64 array) (values: int64 array)
    : Term.exp list 
    =
  let images : Vect.t array = Array.map (fun t -> Eval.evalv t keys) terms in
  let space = Array.fold_right (VSet.add) images VSet.empty in 
  let space = IMap.fold (fun key _ acc -> VSet.add (Vect.mk key) acc) constants space in
  let l = List.fold_right (fun c acc -> 
    match Context.fit space keys values c with
    | [] -> acc
    | pts -> (c,pts)::acc
  ) contexts []
  in
  (* We use the information we collected when hashing terms to rebuild
     the seminal terms... *)
  let map =
    let map = ref VMap.empty in 
    let add c t = 
      try map := VMap.add c (t :: VMap.find c !map) !map
      with Not_found -> map := VMap.add c [t] !map
    in
    for i = 0 to Array.length images -1 do
      let img = images.(i) in 
      let t = terms.(i) in 
      add img t 
    done;
    IMap.iter (fun c t -> add (Vect.mk c) t) constants;
    !map
  in
  let invert (x: Vect.t array) : Term.exp array list = 
    let v = Array.map (fun x -> VMap.find x map) x in
    let rec aux deb i acc =
      if i = Array.length v then Array.of_list (List.rev deb) :: acc
      else 
	List.fold_right (fun t acc' -> 
	  aux (t::deb) (i+1) acc'
	) v.(i) acc
    in
    aux [] 0 []
  in
  let rec process l acc  =
    match l with
    | [] -> acc
    | (c,pts)::q  -> 
      let acc'  = 
	List.rev_map (fun p -> 
	let ps = invert p in
	List.map (fun p -> Term.subst_holes  p c) ps
      ) pts in 
      process q (List.rev_append (List.flatten acc') acc)
  in 
  process l []

<<<<<<< HEAD
let main sizeC sizeT sizeE ops keys values =
  let constants = Generator.generate_constants_witness sizeC ops in
  let constants = 
    List.fold_right (fun (t,c) acc -> IMap.add c t acc) constants IMap.empty in
  (* terms *)
  let terms = Array.of_list (Generator.generate ~force_fold:false sizeT ~exact:false ops) in
  (* contexts *)
  let contexts = (Generator.generate_context sizeE ops ([Term.Notations.hole 0 false])) in 
    synthesis constants terms (contexts) 
=======
let main sizeC sizeT sizeE ops  =
  Printf.printf "synthesis: c %i t %i e %i\n%!" sizeC sizeT sizeE;
  let constants = Generator.generate_constants_witness sizeC ops in
  let constants = 
    List.fold_right (fun (t,c) acc -> IMap.add c t acc) constants IMap.empty in
  Printf.printf "constants: %i\n%!" (IMap.cardinal constants);
  (* terms *)
  let terms = Array.of_list (Generator.generate ~force_fold:false sizeT ~exact:false ops) in
  Printf.printf "terms: %i\n%!" (Array.length terms);
  (* contexts *)
  let contexts = (Generator.generate_context sizeE ops ([Term.Notations.hole 0 false])) in 
  Printf.printf "contexts: %i\n%!" (List.length contexts);
  synthesis constants terms (contexts)  
>>>>>>> Synthesis of suitable terms, not tested
 

