let rnd64 = Term.rnd64

(* discriminate evaluate loop *)
type guess_result = Discr of int64 * int64 | Equiv 

open Loop


exception Incomplete
module Constraints = struct
  type chunk = (int64 * int64) array
  type t = 
    {mutable chunks: chunk list;
     top : chunk;
     mutable pos : int}
    
  let size = 256

  let state = {chunks = []; top = Array.create size (0L, 0L); pos = 0}
   
  let oldify () =
    state.chunks <- Array.copy state.top :: state.chunks;
    state.pos <- 0

  let add k v =
    if state.pos = size 
    then oldify ();
   
    state.top.(state.pos) <- (k,v);
    state.pos <- state.pos + 1

  let addv keys values =
    let n = Array.length  keys in
    state.chunks <- Array.init n (fun i -> keys.(i),values.(i)) :: state.chunks

  let iter f =
    List.iter (fun v -> Array.iter f v) state.chunks;
    for i = 0 to state.pos -1 do
      f state.top.(i)
    done

  let forall f =
    let b = List.for_all (fun v -> Array.fold_left (fun acc x -> acc && f x) true v) state.chunks  in 
    let rec aux acc i = 
      if i = state.pos then acc
      else aux (acc && f state.top.(i)) (i+1) 
    in
    aux b 0

end

(* Client *)
module FState(X:sig val n : int val ops: Term.OSet.t val tfold: bool end)(O: ORACLE) = struct
    
  exception Found of Term.exp
  let f p = 
    if Constraints.forall (fun (k,v) -> Eval.eval p k = v)
    then match O.guess p with
    | Equiv -> raise (Found p)
    | Discr (k',v') -> Constraints.add k' v'
    else ()

    
  let loop () =
    try ignore (Smart_gen.populate X.n X.ops f);
	
        Printf.eprintf "Search was incomplete!\n%!";
	(match O.reveal  () with Some s -> 
	  (Print.print_exp s)
	|None -> ())
   	  
    with Found p -> 
      Printf.printf "result\n";
      Print.(print_exp p);
      print_newline ()
	
end
    
