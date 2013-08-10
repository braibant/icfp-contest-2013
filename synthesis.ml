module IMap = Map.Make (struct type t = int64 let compare = Int64.compare end)

module Vect = struct
  type t = int64 array
  let compare = Pervasives.compare
  let size = 256
  let eval (t : Term.exp) (x: int64 array) = Term.evalv t x
  let mk c = Array.create size c 
  let zero = mk 0L
  let (equal) = (=)
end


module VMap = Map.Make(Vect)
module VSet = Set.Make(Vect)

module Context = struct
    
  (* ensure that the free variables are normalized between 0 and the
     number of free variables... *)
  type t = 
  | Var of int
  | If0 of t * t * t 
  | Fold of t * t * t
  | Op1 of Term.op1 * t 
  | Op2 of Term.op2 * t * t 

  let rec fvar = function Var v -> v
    | If0 (a,b,c) -> max (fvar a) (max (fvar b) (fvar c))
    | Fold (a,b,c) -> max (fvar a) (max (fvar b) (fvar c))
    | Op1 (_,a) ->fvar a
    | Op2 (_,a,b) -> max (fvar a) (fvar b) 
    
      
  
  let mk_term sigma t =
    let rec aux  = function     
      | Var i -> sigma.(i)
      | If0 (a,b,c) -> Term.Notations.if0  (aux a) (aux b) (aux c)
      | Fold (a,b,c) -> Term.Notations.fold  (aux a) (aux b) (aux c)
      | Op1  (op,t) -> Term.Notations.op1 op (aux t)
      | Op2  (op,s,t) -> Term.Notations.op2 op (aux s)(aux t)
    in 
    aux t

  let map2 f a b = 
    Array.mapi (fun i a -> f a b.(i)) a
      
  let eval sigma t =let open Int64 in 
    let rec aux = function
      | Var i -> sigma.(i)
      | If0 (a,b,c) -> 
	let a = aux a in let b = aux b in let c = aux c in 
	Array.init (Array.length a) (fun i ->
	  if a.(i) = 0L then b.(i) else c.(i)
	) 
      | Op1 (op,e) -> let e = aux  e in 
		      begin match op with 
		      | Term.Not -> Array.map lognot e
		      | Term.Shl1 -> Array.map (fun e -> shift_left e 1) e
		      | Term.Shr1 -> Array.map (fun e -> shift_right_logical e 1) e
		      | Term.Shr4 -> Array.map (fun e -> shift_right_logical e 4) e
		      | Term.Shr16 -> Array.map (fun e ->shift_right_logical e 16) e
		      end
      | Op2 (op,e,f) -> let e = aux  e in
			let f = aux  f in 
			begin match op with
			| Term.And -> map2 logand e f
			| Term.Or -> map2 logor e f
			| Term.Xor -> map2 logxor e f
			| Term.Plus -> map2 add e f
			end 
      | Fold (e0,e1,e2) -> assert false
    in aux t

  let fit space src (tgt: Vect.t) c : Vect.t array list  =
    let n = fvar c in 
    let sigma = Array.create n Vect.zero in
    let rec aux i space acc =
      if i = n then 
	 (if Vect.equal (eval sigma c) tgt
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
    (keys: int64 array) (values: int64 array)
    (contexts: Context.t list)
    : Term.exp list 
    =
  let images : Vect.t array = Array.map (fun t -> Vect.eval t keys) terms in
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
	List.map (fun p -> Context.mk_term p c) ps
      ) pts in 
      process q (List.rev_append (List.flatten acc') acc)
  in 
  process l []
