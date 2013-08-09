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

  type log = 
    int64 IMap.t * Term.exp list

  let empty = IMap.empty, []
  let save n t = 
    let o = open_out n in 
      Marshal.to_channel o t [];
      close_out o

  let restore n = 
    let i = open_in n in
    let x = (Marshal.from_channel i : log) in 
    close_in i; x

  let logv ((t,g): log) keys values =
    let n = Array.length keys in 
    assert (n = Array.length values);
    let t = ref t in 
    for i = 0 to n - 1 do
      t := add keys.(i) values.(i) !t
    done; 
    !t,g

  let log ((t,g): log) key value =
      add key value t,g

  let guess (t,g) guess = 
    t, guess :: g

  let print ((t,g): log)= 
    let open PPrint in 
    let int64 n = string (Printf.sprintf "0x%Lx" n) in
    let t = group (separate_map (semi ^^ break 1) (fun (x, y) -> parens (int64 x ^/^ int64 y)) (IMap.bindings t)) in
    let g = group (separate_map (semi) Print.doc_exp g) in 
    group (prefix 2 1 (string "(* evals *)") t) ^^ hardline ^^
    group (prefix 2 1 (string "(* guesses *)") g) ^^ hardline 
      
  let print_short (t,g) =
    let open PPrint in 
    let t = List.length (IMap.bindings t) in 
    let g = List.length g in 
    let int n= string (Printf.sprintf "%i" n ) in 
    group (prefix 2 1 (string "(* evals *)") (int t)) ^^ hardline ^^
    group (prefix 2 1 (string "(* guesses *)") (int g)) ^^ hardline 

 
end

(* Client *)
module FState(X:sig val n : int end) = struct
  include X
  (* a bitvector representation of a set of the possible programs *)
  type t = Bitv.t 

  let terms = Array.of_list (Generator.generate ~filter:false (Term.size secret) (Generator.operators secret))
  let init = Bitv.create (Array.length terms) true

  let print (p:t): unit=
    Bitv.iteri_true (fun i -> Print.(print_exp_nl terms.(i))) p

 
    
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


  let rec iloop p (log: Log.log) =
    Print.(print (string "current state" ^//^ Log.print_short log));
    Printf.printf "current possible terms\n";
    print p;
    match read_line () with
    | "e" -> 
      let keys = discriminating p 256 in 
      let values = Test.eval keys in 
      let refined,p = refine p keys values in 
      Printf.printf "refined: %b\n" refined;
      iloop p (Log.logv log keys values)
    | "g" -> 
      let candidate = choose p in 
      begin match Test.guess candidate with 
      | Equiv -> candidate
      | Discr (key, value) -> 
	let log = Log.guess log candidate in 
	let log = Log.log log key value in 
	let refined,p = refine1 p key value in 
	Printf.printf "refined: %b\n" refined;	
	iloop p log
      end
    | "q" -> 
      exit 0
    | "s" -> 
      Log.save !Options.logfile log;
      iloop p log 
    | _ ->
      iloop p log 

         
  let iloop () = 
    let r = iloop init Log.empty in 
    Printf.printf "result\n";
    Print.(print_exp r)


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
      
  let loop () = 
    let r =  (loop init) in 
    Printf.printf "result\n";
    Print.(print_exp r)

end

let args = 
  let open Arg in 
  ["-o", Set_string Options.logfile, " set log file";
   "-i", Set Options.interactive_mode, " interactive mode" ;
   "-n", Set_int Options.problem_size, " set problem size"]

let _ =
  Arg.parse args (fun rest -> ()) "usage";
  
  let module Loop = FState (struct let n = !Options.problem_size end) in 
  if !Options.interactive_mode 
  then Loop.iloop ()
  else Loop.loop ()

  

  
    
