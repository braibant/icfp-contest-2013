module IMap = Map.Make (struct type t = int64 let compare = Int64.compare end)
module VMap = Map.Make(Vect)
module VSet = Set.Make(Vect)

module Constraints = struct
  type t =   (int64 IMap.t) ref

  let map = ref IMap.empty 

  let is_empty () = IMap.is_empty !map
  let to_vect () = 
    let l = IMap.bindings !map in 
    let n = IMap.cardinal !map in
    let keys =  Array.create n 0L in
    let values =  Array.create n 0L in
    List.iteri (fun i (k,v) -> keys.(i) <- k; values.(i) <- v) l;
    keys, values

  let add k v = map := IMap.add k v !map
 
  let addv keys values =
    let n = Array.length  keys in
    for i = 0 to n-1 do
      add keys.(i) values.(i)
    done

end


(* ensure that the free variables are normalized between 0 and the
   number of free variables... *)

let eval p sigma args =  Eval.h_evalv p sigma args
      
module PrioQueue = struct

  (* type t = ref Term.exp list *)
  type t =
    {
      content : Term.exp array;
      mutable next: int
    }

  exception Empty

  let compare p q = 
    let x = compare (Term.holes p) (Term.holes q) in
    if  x == 0
    then compare (Term.size p) (Term.size q) 
    else x

     
  let create content =
    Array.stable_sort compare  content;
    {content; next = 0}

  let pop q =
    if q.next < Array.length q.content 
    then
      let res = q.content.(q.next) in 
      q.next <- q.next + 1;
      res
    else
      raise Empty
     
  (* let n = 5 				(\* virons les contextes a plus que 5 trou *\) *)

  (* type t = *)
  (*   {queues : Term.exp Queue.t array; *)
  (*    mutable current : int } *)

  (* let create () : t =  *)
  (*   {queues = Array.init n (fun _ -> Queue.create ()); *)
  (*    current = 0} *)

  (* let rec pop (q:t) = *)
  (*   try  Queue.pop q.queues.(q.current)  *)
  (*   with Queue.Empty -> *)
  (*     if  q.current < n *)
  (*     then (q.current <- q.current + 1; pop q) *)
  (*     else raise Empty *)

  (* let add elt (q:t) = *)
  (*   let nholes = Term.holes elt in  *)
  (*   if nholes < n then   Queue.add elt q.queues.(nholes) else () *)
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


(* in fact, all we need is n > 0 possible contexts !!! *)
exception Found of Term.exp array list
let fit ?(time_budget=10.) (space: Term.exp list VMap.t) src (tgt: Vect.t) c : Term.exp array list  =
  let n = Term.holes c in 
  let sigma1 = Array.create n src in 
  let sigma2 = Array.create n ([]) in

  (* early termination detection *)
  let check l = if List.length l > 2 then raise (Found l) else l in 
  let start_time = Unix.gettimeofday () in 
  let rec aux i acc : Term.exp array list =
    if Unix.gettimeofday () -. start_time > time_budget
    then raise (Found acc) 
    else
      if i = n then 
	(if try Vect.equal (eval c sigma1 src) tgt with _ -> true 
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

    (* this field is mutable, because it will be computed only on the
       first execution of synthesis... *)
    (* map: (Term.exp list VMap.t) ref ; 	 *)
  }

let synthesis 
    env
    : Term.exp list 
    =
  let terms = env.terms  in
  let contexts = env.contexts in 
  let keys, values = Constraints.to_vect () in 
  
  let map = 
    let map = ref VMap.empty in
    let add c t = 
      try map := VMap.add c (t :: VMap.find c !map) !map
      with Not_found -> map := VMap.add c [t] !map
    in
    for i = 0 to Array.length terms -1 do
      let img = Eval.evalv terms.(i) keys in 
      add img terms.(i)
    done; 
    Printf.printf "synthesis discriminate map built (card:%i)\n%!" 
      (VMap.cardinal !map);
    !map
  in

  let batch_size = 256 * !Config.jobs  in
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
      aux [] batch_size
	
  in

  let cmap (c: Term.exp) acc : Term.exp list  =
    let res = fit map keys values c in 
    let res = List.rev_map (fun prg -> Term.subst_holes prg c) res in 
    List.rev_append res acc
  in
  let rec aux acc =
    Printf.printf "-%!";
    let b = batch () in 
    (* let res = Functory.Cores.map_fold_ac  *)
    (*   ~f:(cmap) ~fold:List.rev_append [] b in  *)
    let res = Parmap.parfold ~chunksize:4 cmap  (Parmap.L b) [] List.rev_append in
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
  let queue = PrioQueue.create contexts in
  (* Array.iter (fun elt -> PrioQueue.add elt queue) contexts; *)
  {terms; contexts= queue}
    
