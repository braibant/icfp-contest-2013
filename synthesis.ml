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
      
module PrioQueue = struct
  let n = 5 				(* virons les contextes a plus que 5 trou *)

  exception Empty

  type t =
    {queues : Term.exp Queue.t array;
     mutable current : int }

  let create () : t = 
    {queues = Array.init n (fun _ -> Queue.create ());
     current = 0}

  let rec pop (q:t) =
    try  Queue.pop q.queues.(q.current) 
    with Queue.Empty ->
      if  q.current < n
      then (q.current <- q.current + 1; pop q)
      else raise Empty

  let add elt (q:t) =
    let nholes = Term.holes elt in 
    if nholes < n then   Queue.add elt q.queues.(nholes) else ()
end





  
   
   
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

  (* early termination detection *)
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
    contexts: PrioQueue.t;
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
  
  let batch : unit -> Term.exp list =
    let rec aux acc =
      function
      | 0 -> acc
      | n -> 
	try  aux  (PrioQueue.pop contexts :: acc) (n - 1)
	with PrioQueue.Empty -> 
	  if acc = [] 
	  then failwith "contexts depleted"
	  else acc
    in
    fun () -> 
      aux [] !Config.batch_size
	   
  in

  let cmap (c: Term.exp) : Term.exp list  =
    let res = fit map keys values c in 
    let res = List.rev_map (fun prg -> Term.subst_holes prg c) res in 
    res
  in
  let rec aux acc =
    Printf.printf "-%!";
    let b = batch () in 
    let res = Functory.Cores.map_fold_ac 
      ~f:(cmap) ~fold:List.rev_append [] b in 
    let res = List.rev_append res acc in
    if List.length res > 1 then res else  aux res
  in 
  aux []
  
let generate sizeT sizeE ops =
  Printf.printf "synthesis:  t %i e %i\n%!" sizeT sizeE;
  let terms = Generator.generate ~force_fold:false sizeT ~exact:false ops in
  Printf.printf "terms: %i\n%!" (Array.length terms);
  let contexts = Generator.generate_context sizeE ops ([Term.Notations.hole 0 false]) in
  Printf.printf "contexts: %i\n%!" (Array.length contexts);
  let queue = PrioQueue.create () in
  Array.iter (fun elt -> PrioQueue.add elt queue) contexts;
  {terms; contexts= queue; map = ref None}
  
