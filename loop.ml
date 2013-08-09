(* discriminate evaluate loop *)

let discriminating _ n = Array.init n (fun x -> Random.int64 Int64.max_int)

let evalv v p = Array.map (Term.eval p) v

type guess_result = Discr of int64 * int64 | Equiv

module Sim(Secret : sig val secret : Term.exp end)  = struct
  include Secret
  let eval v = evalv v secret

  let guess p' =
    let confidence = 10000 in 
    let tests = discriminating () confidence in 
    let rec aux i = 
      if i = confidence - 1 then Equiv 
      else
	let x = tests.(i) in
	if Term.eval p' x = Term.eval secret x 
	then aux (succ i)
	else Discr (x,Term.eval secret x)
    in aux 0      
end

open Term 

let secret =  let open Notations in  mk_arg ++ C1 				   
module Test = Sim (struct open Term open Term.Notations let secret = secret  end)   

module FState(X:sig val n : int end) = struct
  include X
  (* a bitvector representation of a set of the possible programs *)
  type t = Bitv.t 

  let terms = Array.of_list (Generator.generate n (Generator.operators secret))
  let init = Bitv.create (Array.length terms) true

  exception NotEquiv
  let equiv p q a = 
    let n = Array.length q in 
    try
      for i = 0 to n - 1 do
	if Term.eval p q.(i) = a.(i)
	then ()
	else raise NotEquiv
      done;
      true
    with
      NotEquiv -> false

  let refine (p: t) (values: int64 array) (answers: int64 array) = 
    let p' = Bitv.copy p in 
    let refined = ref false in 
    Bitv.iteri_true 
      (fun i -> 
	if equiv terms.(i) values answers 
	then ()
	else (refined := true; Bitv.set p' i false)	  
      ) p;
    !refined, p'

  let refine1 p v a = 
    refine p [|v|] [|a|] 

  let size (p:t) = let r = ref 0 in Bitv.iteri_true (fun _ -> incr r) p; !r

  exception Found of int
  let choose p = 
    try
      Bitv.iteri_true (fun i -> raise (Found i)) p;
      raise Not_found
    with Found i -> terms.(i)

  let print =
    Bitv.iteri_true (fun i -> Print.(print_exp_nl terms.(i)))
end

module State=FState (struct let n = 3 end)

let rec loop p = 
  (* Printf.eprintf "size:%i\n" (State.size p); *)
  let values = discriminating p 256 in 
  let answers = Test.eval values in 
  let refined,p = State.refine p values answers in 
  if State.size p = 1 || not refined
  then 
    begin 
      let candidate = State.choose p in
      match Test.guess candidate with 
      | Equiv -> candidate
      | Discr (value, answer) -> 
	let refined,p = State.refine1 p value answer in 
	if not refined 
	then (Printf.eprintf "guess was not refining, size:%i\n" (State.size p); State.print p; assert false);
	loop p
    end
  else loop p
  
    
let _ = 
  Printf.printf "initial state\n";
  State.print State.init;
  Printf.printf "result\n";
  Print.(print_exp (loop State.init))
 
