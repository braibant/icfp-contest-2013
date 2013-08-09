(* discriminate evaluate loop *)

let discriminating _ n = Array.init n (fun x -> Random.int64 Int64.max_int)

let evalv v p = Array.map (Term.eval p) v

type guess_result = Discr of int64 * int64 | Equiv

(* Oracle *)
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

module Log = struct
  module IMap = Map.Make (struct type t = int64 let compare = Int64.compare end)

  include IMap

  type log = int64 IMap.t 



  let save n t = 
    let o = open_out n in 
      Marshal.to_channel o t [];
      close_out o

  let restore n = 
    let i = open_in n in
    let x = (Marshal.from_channel i : log) in 
    close_in i; x

  let logv t keys values =
    let n = Array.length keys in 
    assert (n = Array.length values);
    let t = ref t in 
    for i = 0 to n - 1 do
      t := add keys.(i) values.(i) !t
    done; 
    !t

  let log (t: log) key value =
      add key value t

  let print = 
    let open PPrint in 
    let int64 n = string (Printf.sprintf "0x%Lx" n) in
    separate_map (semi ^^ break 1) (fun (x, y) -> parens (int64 x ^/^ int64 y))
 
end

(* Client *)
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



  (* the main loop function:
     - it logs the previous queries and answers from the server in a text file
  *)
  let rec loop p = 
    (* Printf.eprintf "size:%i\n" (State.size p); *)
    let values = discriminating p 256 in 
    let answers = Test.eval values in 
    let refined,p = refine p values answers in 
    if size p = 1 || not refined
    then 
      begin 
	let candidate = choose p in
	match Test.guess candidate with 
	| Equiv -> candidate
	| Discr (value, answer) -> 
	  let refined,p = refine1 p value answer in 
	  if not refined 
	  then (Printf.eprintf "guess was not refining, size:%i\n" (size p); print p; assert false);
	  loop p
      end
    else loop p
      
end

module State=FState (struct let n = 3 end)

  
    
let _ = 
  Printf.printf "initial state\n";
  State.print State.init;
  Printf.printf "result\n";
  Print.(print_exp (State.(loop init)))
 
