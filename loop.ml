let rnd64 = Term.rnd64

(* discriminate evaluate loop *)
type guess_result = Discr of int64 * int64 | Equiv 

module type ORACLE = sig
  val eval : int64 array -> int64 array
  val guess : Term.exp -> guess_result
  val reveal : unit  -> Term.exp option
end

(* Client *)
module FState(X:sig val n : int val ops: Generator.OSet.t val tfold: bool end)(O: ORACLE) = struct
  include X

  (* a bitvector representation of a set of the possible programs *)
  type t = 
    {terms: Term.exp array;
     sieve: Bitv.t} 

  let env =
    if not !Config.synthesis 
    then None
    else
      Utils.begin_end_msg "computing env" begin fun () ->
      Some (Synthesis.generate 7 (n- 5) ops) 
      end

  let get_env () = match env with None -> assert false | Some env -> env 

  (* initialize the term array *) 
  let init () : t = 
    let terms = Utils.begin_end_msg "computing terms" begin fun () ->
      if (n < 8 || tfold) && not !Config.synthesis then
	Array.of_list 
	  (if tfold then Generator.generate_tfold n ops
	   else Generator.generate n ops)
      else
      let keys = Array.init 256 (fun _ -> rnd64 ()) in 
      let values = O.eval keys in 
      let v = Array.of_list (Synthesis.synthesis (get_env ()) keys values) in 
      Printf.printf "synthesis generated %i terms\n" (Array.length v);
      v
    end 
    in
    let sieve = Bitv.create (Array.length terms) true in
    {terms; sieve}

  let size (p:t) : int = let r = ref 0 in Bitv.iteri_true (fun _ -> incr r) p.sieve; !r

  (* This function must be called to ensure that the current state is well-formed *)
  let check p =  
    if size p = 0 
    then init ()
    else p

  let print (p:t)=
    let l = ref [] in
    Bitv.iteri_true (fun i -> l := p.terms.(i) :: !l) p.sieve;
    let open Print in 
    separate_map hardline (Print.doc_exp) !l

  let size (p:t) : int = let r = ref 0 in Bitv.iteri_true (fun _ -> incr r) p.sieve; !r

  let print_short p =  let open Print in string (string_of_int (size p))

  exception End
  let iter2 f p =
    Bitv.iteri_true 
      (fun x -> 
	try 
	  Bitv.iteri_true (fun y -> if x < y then f p.terms.(x) p.terms.(y) 
	    else raise End) p.sieve
	with End -> ()) p.sieve

  (* find the n  indices that maximizes v.(n) (yet give different values to v.(n))  *)
  let maxn n v = 
    let s = Array.init (Array.length v) (fun i -> i) in 
    let compare x y = Pervasives.compare v.(y) v.(x) in 
    (Array.sort compare s);
    Array.sub s 0 n
   
  (* find the values that discriminate the most *)
  let best (p:t) =
    let n = 100_000 in 
    let v = Array.init n (fun _ -> rnd64 ()) in 
    let d = Array.create n 0 in
    iter2 
      (fun p1 p2 ->
	for i = 0 to n - 1 do
	  let x = v.(i) in 
	  if Eval.eval p1 x <> Eval.eval p2 x 
	  then d.(i) <- d.(i)+1
	done) p;
    (* What are the best 256 discriminating values ? *)
    let r = Array.map (fun i -> v.(i)) (maxn 256 d) in 
    (* Array.iteri (fun i x -> Printf.printf "%i %s\n" i (Int64.to_string x)) r; *)
    r

  exception Found of int

  (* find values that discriminate using sat solving *)
  let best_sat (p:t ) =
    let keys = ref [] in
    for j = 0 to 10 do
      let get_nth k =
	let cur = ref 0 in
	try
	  Bitv.iteri_true (fun i -> if !cur = k then raise (Found i) else incr cur) p.sieve;
	  assert false
	with Found i -> p.terms.(i)
      in
      let pairs = ref [] in
      let n = size p in
      for i = 0 to 6 do (* Increase 6 to whatever if there are many cores *)
	pairs := (get_nth (Random.int n), get_nth (Random.int n)) :: !pairs
      done;
      keys := (Sat.discriminate !pairs) @ !keys
    done;
    let keys = List.sort compare !keys in
    let rec uniq = function
      | [] -> []
      | (Sat.Unsat | Sat.Unknown)::q -> uniq q
      | t::t'::q when t=t' -> uniq (t'::q)
      | Sat.Sat t::q -> t::uniq q
    in
    let keys = uniq keys in
    Printf.printf "Found %d new discriminants by SAT\n" (List.length keys);
    Array.init 256 (fun i ->
      if i < List.length keys then List.nth keys i else rnd64 ())

  (* if we cannot find values that would make progress, we have to make a guess. *)
    
  let all_equiv (p: t) =
    let n = 100_000 in 
    let v = Array.init n (fun _ -> rnd64 ()) in 
    try 
      iter2 (fun p1 p2 ->
	for i = 0 to n - 1 do
	  let x = v.(i) in 
	  if Eval.eval p1 x <> Eval.eval p2 x 
	  then raise Not_found 
	done      
      ) p;
    true
    with
    | Not_found -> false
    
  exception NotEquiv
  let equiv p q a = 
    let n = Array.length q in 
    try
      for i = 0 to n - 1 do
	if Eval.eval p q.(i) = a.(i)
	then ()
	else raise NotEquiv
      done;
      true
    with
      NotEquiv -> false

  let refine (p: t) (values: int64 array) (answers: int64 array) = 
    let sieve = Bitv.copy p.sieve in 
    let refined = ref false in 
    Bitv.iteri_true 
      (fun i -> 
	if equiv p.terms.(i) values answers 
	then ()
	else (refined := true; Bitv.set sieve i false)	  
      ) p.sieve;
    !refined, {p with sieve}

  let refine1 p v a = 
    refine p [|v|] [|a|] 


  let rec choose p = 
    try
      Bitv.iteri_true (fun i -> raise (Found i)) p.sieve;
      raise Not_found
    with 
    |Found i -> p.terms.(i)
    |Not_found ->
      Printf.printf "choose failed: regenerate terms\n";
      choose (init ())


  let rec message l =
    let open Print in 
    separate_map hardline (fun (msg,doc) -> group (prefix 2 1 (string msg) doc)) l

  let invite () =
    print_newline ();
    print_string "s(a)t (b)est r(e)ndom (g)uess (c)heck_all_equiv";
    print_newline ();
    print_string "$ "

  let rec iloop (p:t) (log: Xlog.log) =
    let p = check p in 
    Print.print (message
      ["current state", Xlog.print_short log;
       "possible terms", print_short p]);
    invite ();
    match read_line () with
    | "b" ->  				(* best *)
      let keys = best p in 
      let values = O.eval keys in 
      let refined,p = refine p keys values in 
      Printf.printf "refined: %b\n" refined;
      iloop p (Xlog.logv log keys values)
    | "e" -> 				(* random *)
      let keys = Array.init 256 (fun _ -> rnd64 ()) in 
      let values = O.eval keys in 
      let refined,p = refine p keys values in 
      Printf.printf "refined: %b\n" refined;
      iloop p (Xlog.logv log keys values)
    | "a" -> 				(* with sat *)
      let keys = best_sat p in
      let values = O.eval keys in 
      let refined,p = refine p keys values in 
      Printf.printf "refined: %b\n" refined;
      iloop p (Xlog.logv log keys values)
    | "g" -> 
      let candidate = choose p in 
      begin match O.guess candidate with 
      | Equiv -> 
	candidate
      | Discr (key, value) -> 
	let log = Xlog.guess log candidate in 
	let log = Xlog.log log key value in 
	let refined,p = refine1 p key value in 
	Printf.printf "refined: %b\n" refined;	
	iloop p log
      end
    | "q" -> 
      exit 0
    | "s" -> 
      Xlog.save !Config.logfile log;
      iloop p log 
    | "p" ->
      Print.print (Xlog.print log);
      iloop p log 
    | "c" ->
      Printf.printf "all_equiv:%b\n" (all_equiv p);
      iloop p log
    | "?" ->
      Printf.printf "all initial terms\n";
      Array.iter Print.print_exp_nl p.terms;
      iloop p log

    | _ ->
      iloop p log
	
	
         
  let iloop () = 
    let r = iloop (init ()) Xlog.empty in 
    Printf.printf "result\n";
    Print.(print_exp_nl r);
    match O.reveal () with
      | None -> ()
      | Some secret ->
        Printf.printf "secret\n";
        Print.(print_exp_nl secret);
        print_newline ();
        ()

  let rec loop round p =
    let p = check p in 
    let oldsize = size p in
    let keys =
      if round < 2 then  Array.init 256 (fun _ -> rnd64 ())
      else best_sat p in 
    let values = O.eval keys in 
    let refined,p = refine p keys values in
    let cursize = size p in
    if cursize = 1 || oldsize-cursize <= oldsize/5
    then 
      begin 
	let candidate = choose p in
	match O.guess candidate with 
	| Equiv -> candidate
	| Discr (value, answer) -> 
	  let refined,p = refine1 p value answer in 
	  if not refined 
	  then (Printf.eprintf "guess was not refining, size:%i\n" (size p); 
		(* Print.print (print p); assert false *));
	  loop (succ round) p
      end
    else loop (succ round) p
      
  let loop () = 
    let r =  (loop 0 (init ())) in 
    Printf.printf "result\n";
    Print.(print_exp r);
    print_newline ()

end


