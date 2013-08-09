(* discriminate evaluate loop *)

let discriminating _ n = Array.init n (fun x -> Random.int64 Int64.max_int)

let evalv v p = Array.map (Term.eval p) v

type guess_result = Discr of int64 * int64 | Equiv

module type ORACLE = sig
  val eval : int64 array -> int64 array
  val guess : Term.exp -> guess_result
end


(* Client *)
module FState(X:sig val n : int val ops: Generator.OSet.t end)(O: ORACLE) = struct
  include X

  (* a bitvector representation of a set of the possible programs *)
  type t = Bitv.t 

  let terms = Array.of_list (Generator.generate ~filter:false n ops)
  let init = Bitv.create (Array.length terms) true

  let print (p:t)=
    let l = ref [] in
    Bitv.iteri_true (fun i -> l := terms.(i) :: !l) p;
    let open Print in 
    separate_map hardline (Print.doc_exp) !l

  let size (p:t) : int = let r = ref 0 in Bitv.iteri_true (fun _ -> incr r) p; !r

  let print_short p =  let open Print in string (string_of_int (size p))

    
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


  exception Found of int
  let choose p = 
    try
      Bitv.iteri_true (fun i -> raise (Found i)) p;
      raise Not_found
    with Found i -> terms.(i)


  let rec message l =
    let open Print in 
    separate_map hardline (fun (msg,doc) -> group (prefix 2 1 (string msg) doc)) l

  let invite () =
    print_newline ();
    print_string "$ "

  let rec iloop p (log: Log.log) =
    Print.print (message
      ["current state", Log.print_short log;
       "possible terms", print_short p]);
    invite ();
    match read_line () with
    | "e" -> 
      let keys = discriminating p 256 in 
      let values = O.eval keys in 
      let refined,p = refine p keys values in 
      Printf.printf "refined: %b\n" refined;
      iloop p (Log.logv log keys values)
    | "g" -> 
      let candidate = choose p in 
      begin match O.guess candidate with 
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
    | "p" ->
      Print.print (Log.print log);
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
    let answers = O.eval values in 
    let refined,p = refine p values answers in 
    if size p = 1 || not refined
    then 
      begin 
	let candidate = choose p in
	match O.guess candidate with 
	| Equiv -> candidate
	| Discr (value, answer) -> 
	  let refined,p = refine1 p value answer in 
	  if not refined 
	  then (Printf.eprintf "guess was not refining, size:%i\n" (size p); 
		Print.print (print p); assert false);
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
   "-n", Set_int Options.problem_size, " set problem size";
   "-s", Set_string Options.secret, " set the secret (debug)"]


(* Oracle *)
module Oracle(S: sig val secret : Term.exp end)  = struct
  include S
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


let _ =
  Arg.parse args (fun rest -> ()) "usage";
  let secret = Example.gen !Options.problem_size in 
  Printf.printf "start (size of the secret:%i)\n%!" (Term.size secret);
  let module Oracle = Oracle(struct let secret = secret end) in
  let module Params = struct let n = Term.size secret let ops = Generator.operators secret end in 
  let module Loop = FState(Params)(Oracle) in 
  if !Options.interactive_mode 
  then Loop.iloop ()
  else Loop.loop ()

  

  
    
