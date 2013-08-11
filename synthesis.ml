module IMap = Map.Make (struct type t = int64 let compare = Int64.compare end)
module VMap = Map.Make(Vect)
module VSet = Set.Make(Vect)

(* ensure that the free variables are normalized between 0 and the
   number of free variables... *)

let eval a b c = 
  try Eval.h_evalv a b c
  with e -> 
    Print.print_exp_nl a;  
    raise e
      
   
   
let explode (v: 'a list array) acc : 'a array list =
  let rec aux deb i acc =
    if i = Array.length v 
    then Array.of_list (List.rev deb) :: acc
    else 
      List.fold_right 
	(fun t acc' -> 
	  aux (t::deb) (i+1) acc'
	) v.(i) acc
  in aux [] 0 acc 

(* in fact, all we need is n > 2 possible contexts !!! *)
exception Found of Term.exp array list
let fit (space: Term.exp list VMap.t) src (tgt: Vect.t) c : Term.exp array list  =
  let n = Term.holes c in 
  let sigma1 = Array.create n Vect.zero in 
  let sigma2 = Array.create n ([]) in

  let check l = if List.length l > 2 then raise (Found l) else l in 
  
  let rec aux i acc : Term.exp array list =
    if i = n then 
      (if Vect.equal (eval c sigma1 src) tgt
       then check (explode sigma2  acc)
       else acc)
    else
      VMap.fold 
	(fun k t acc ->
	  sigma1.(i) <- k;
	  sigma2.(i) <- t;
	  aux (i+1) acc
	) space acc
  in 
  try aux 0  []
  with Found l -> l

type t =
  {
    terms: Term.exp array;
    contexts: Term.exp Queue.t;
    map: (Term.exp list VMap.t) option ref ; 	(* this field is mutable, because it will be computed only on the first execution of synthesis... *)
  }

let synthesis 
    env
    (keys: int64 array) 
    (values: int64 array)
    : Term.exp list 
    =
  let terms = env.terms  in
  let contexts = env.contexts in 
  let map = match !(env.map) with
  | None -> 
    let map = ref VMap.empty in 
    let add c t = 
      try map := VMap.add c (t :: VMap.find c !map) !map
      with Not_found -> map := VMap.add c [t] !map
    in
    for i = 0 to Array.length terms -1 do
      let img = Eval.evalv terms.(i) keys in 
      add img terms.(i)
    done; 
    Printf.printf "synthesis discriminate map built (card:%i)\n%!" (VMap.cardinal !map);
    env.map := Some !map;
    !map
  | Some map -> map
  in
  let rec aux acc = 
    try 
      let c = Queue.pop contexts in 
      let res = fit map keys values c in 
      let res = List.map (fun prg -> Term.subst_holes prg c) res in 
      let res = List.rev_append res acc in
      if List.length res > 2 then res else  aux res

    with Queue.Empty -> failwith "contexts depleted"
  in
  aux [] 
  
let generate sizeT sizeE ops =
  Printf.printf "synthesis:  t %i e %i\n%!" sizeT sizeE;
  let terms = Generator.generate ~force_fold:false sizeT ~exact:false ops in
  Printf.printf "terms: %i\n%!" (Array.length terms);
  let contexts = Generator.generate_context sizeE ops ([Term.Notations.hole 0 false]) in
  Printf.printf "contexts: %i\n%!" (Array.length contexts);
  let queue = Queue.create () in
  Array.iter (fun elt -> Queue.add elt queue) contexts;
  {terms; contexts= queue; map = ref None}
  

(* let main sizeT sizeE ops  = *)
(*   Printf.printf "synthesis:  t %i e %i\n%!" sizeT sizeE; *)
(*   let terms = Array.of_list (Generator.generate ~force_fold:false sizeT ~exact:false ops) in *)
(*   Printf.printf "terms: %i\n%!" (Array.length terms); *)
(*   let contexts = (Generator.generate_context sizeE ops ([Term.Notations.hole 0 false])) in  *)
(*   Printf.printf "contexts: %i\n%!" (List.length contexts); *)
(*   (\* List.iter Print.print_exp_nl contexts; *\) *)
(*   synthesis terms (contexts)   *)
 

